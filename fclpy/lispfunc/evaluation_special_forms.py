"""Special forms: QUOTE, IF, DEFUN, DEFMACRO, LAMBDA, declarations.

This module contains handlers for special forms that don't fall into
control flow, loops/conditionals, or condition handling categories.

DEFSTRUCT: Accept keywords as structure names (v2).
"""

import fclpy.lisptype as lisptype
import fclpy.state as state
import fclpy.classes as _clos_classes
from fclpy.lispfunc.core import car, cdr, _consp_internal, cons, _null_internal
from .binding import proclaim_special, root_environment, BindingFrame
from . import registry as _registry
from . import arrays as _arrays
import logging
import re

logger = logging.getLogger(__name__)

# CLHS 5.1.2.1's compound CAR/CDR accessors: cL1L2...LkR(x) = L1(L2(...Lk(x))),
# 1-4 letters of A/D between the C and the final R (CAR/CDR are the 1-letter,
# non-compound base case and are handled by their own place clauses).
_CXR_RE = re.compile(r'^C([AD]{2,4})R$')


def _cxr_target(op_name, obj):
    """Navigate a CxxxR place (2-4 letters) to (parent-cons, is-car).

    The innermost operation (the letter closest to R) is applied to `obj`
    first; navigation stops one cons short of the outermost letter (closest
    to C), which the caller mutates -- that outermost letter is what SETF
    is actually assigning. Returns None if `op_name` is not a CxxxR name.
    """
    m = _CXR_RE.match(op_name)
    if not m:
        return None
    letters = m.group(1)  # L1 (closest to C) .. Lk (closest to R)
    target = obj
    for c in reversed(letters[1:]):
        if not _consp_internal(target):
            raise lisptype.LispError(f"{op_name}: invalid structure")
        target = target.car if c == 'A' else target.cdr
    if not _consp_internal(target):
        raise lisptype.LispError(f"{op_name}: invalid structure")
    return target, letters[0] == 'A'


def _extract_tail_symbol_from_rest(rest_param):
    """Return the tail symbol from a rest destructuring spec or None.

    Handles cases like (HEAD . TAIL) where rest_param.cdr may be a symbol
    or wrapped as a tiny cons. This makes binding robust against different
    parser representations.
    """
    if not _consp_internal(rest_param):
        return None

    tail = getattr(rest_param, 'cdr', None)

    # Common direct symbol case
    if isinstance(tail, lisptype.LispSymbol):
        return tail

    # Defensive: tail might be a tiny cons whose cdr/car holds the symbol
    try:
        if isinstance(tail, lisptype.lispCons):
            # If the car is a symbol and cdr is NIL, treat car as the tail symbol
            if isinstance(tail.car, lisptype.LispSymbol):
                return tail.car
            # If the cdr is a symbol (rare representation), use it
            if isinstance(tail.cdr, lisptype.LispSymbol):
                return tail.cdr
    except Exception:
        pass

    # If object has a 'name' attribute that looks like a symbol name, intern it
    if hasattr(tail, 'name') and isinstance(getattr(tail, 'name'), str):
        try:
            return lisptype.intern_symbol(tail.name)
        except Exception:
            pass

    # If tail is a plain string, convert to a symbol via py_str_to_sym if available
    if isinstance(tail, str):
        try:
            return lisptype.py_str_to_sym(tail)
        except Exception:
            return lisptype.LispSymbol(tail)

    # Last-resort attempt: try to parse repr() as a symbol name
    try:
        rep = repr(tail)
        rep_clean = rep.strip("() ")
        if rep_clean.isidentifier() or rep_clean.isupper():
            try:
                return lisptype.intern_symbol(rep_clean)
            except Exception:
                return lisptype.LispSymbol(rep_clean)
    except Exception:
        pass

    return None

def eval_if(form, env, *, tail_target=None):
    """Evaluate IF special form.

    **A chain of IFs through the else branch is evaluated in this frame**, not
    one `eval`+`eval_if` pair per link. The else of an IF is very often
    another IF -- COND's expansion is exactly such a chain, and every level of
    a user function written with COND therefore paid two Python frames per
    clause per level of Lisp recursion (~17 frames/level for `split-list` in
    the ansi-test helpers, capping default-limit recursion at ~50 levels).
    The tests of the chain are still evaluated in order here; only the
    *branch dispatch* is flattened, so the semantics are the chain's.
    """
    # Import eval lazily to avoid circular imports
    from .evaluation_core import eval

    current = form
    while True:
        args = cdr(current)
        if not _consp_internal(args):
            raise lisptype.LispNotImplementedError("IF requires at least 2 arguments")

        test_form = car(args)
        then_form = car(cdr(args))
        else_form = car(cdr(cdr(args))) if _consp_internal(cdr(cdr(args))) else None

        test_result = eval(test_form, env)
        if lisptype.is_truthy(test_result):
            return eval(then_form, env, tail_target=tail_target)
        if else_form is None:
            return None
        # The else of an IF is very often another IF -- step into it in this
        # frame instead of recursing through eval + eval_if per link.
        if _consp_internal(else_form):
            op = car(else_form)
            if isinstance(op, lisptype.LispSymbol) and op.name == 'IF':
                current = else_form
                continue
        return eval(else_form, env, tail_target=tail_target)


def eval_setq(form, env):
    """Evaluate SETQ special form.

    CLHS: if `var` names a symbol-macro, SETQ behaves as if the form were
    SETF (`setf-symbol-macro.2`/`.3`) -- including a symbol-macro whose
    expansion is itself a non-symbol place like `(values y z)`. Either way,
    only the *primary* value of the last value-form is returned (`setq.4`);
    a place that itself takes several values (a VALUES place) still gets
    all of them via `_place_accessor`'s setter.
    """
    from .evaluation_core import eval

    args = cdr(form)
    result = None

    while _consp_internal(args) and _consp_internal(cdr(args)):
        var = car(args)
        value_form = car(cdr(args))

        while isinstance(var, lisptype.LispSymbol):
            expansion = env.get_symbol_macro(var)
            if expansion is None:
                break
            var = expansion

        if isinstance(var, lisptype.LispSymbol):
            result = lisptype.primary_value(eval(value_form, env))
            env.set_variable(var, result)
        elif _consp_internal(var):
            getter, setter = _place_accessor(var, env)
            full_result = eval(value_form, env)
            setter(full_result)
            result = lisptype.primary_value(full_result)
        else:
            raise lisptype.LispNotImplementedError("SETQ: variable must be a symbol")

        args = cdr(cdr(args))

    return result


def eval_the(form, env):
    """Evaluate THE special operator.

    THE is a type assertion form. Per ANSI CL tests used here we should
    not evaluate the type-designator (first argument) as a variable; only
    evaluate and return the second argument (the expression). This keeps
    constructs like (THE SYMBOL 'T) from attempting to look up SYMBOL as
    a variable.
    """
    from .evaluation_core import eval

    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("THE requires at least two arguments")

    type_spec = car(args)
    expr = car(cdr(args)) if _consp_internal(cdr(args)) else lisptype.NIL

    # Do not evaluate type_spec here; evaluate and return the expression value
    return eval(expr, env)


@_registry.cl_macro('WITH-OPEN-FILE', documentation='WITH-OPEN-FILE macro expander')
def with_open_file_macro(bindings, *body):
    """Macro expander for WITH-OPEN-FILE.

    Transforms:
      (WITH-OPEN-FILE (var filespec &key ...) body...)
    into:
      (LET ((var (OPEN filespec &key ...)))
        (UNWIND-PROTECT
            (PROGN body...)
            (CLOSE var)))
    """
    # Helper to convert Python iterable to lispCons list
    def to_cons(seq):
        cur = lisptype.NIL
        for e in reversed(seq):
            cur = lisptype.lispCons(e, cur)
        return cur

    # Normalize binding forms (single binding or list of bindings)
    binding_forms = []
    if isinstance(bindings, lisptype.lispCons) and isinstance(bindings.car if hasattr(bindings, 'car') else None, lisptype.LispSymbol):
        # Single binding: (var filespec ...)
        binding_forms = [bindings]
    else:
        # Multiple bindings
        cur = bindings
        while isinstance(cur, lisptype.lispCons):
            binding_forms.append(cur.car)
            cur = cur.cdr

    let_bindings = lisptype.NIL
    close_forms = lisptype.NIL

    for b in reversed(binding_forms):
        stream_sym = b.car
        filespec = b.cdr.car if isinstance(b.cdr, lisptype.lispCons) else lisptype.NIL

        # Build OPEN call: (OPEN filespec &key ...)
        elems = [lisptype.LispSymbol('OPEN'), filespec]
        rest = b.cdr.cdr if isinstance(b.cdr, lisptype.lispCons) else lisptype.NIL
        cur_kw = rest
        while isinstance(cur_kw, lisptype.lispCons):
            key = cur_kw.car
            val = cur_kw.cdr.car if isinstance(cur_kw.cdr, lisptype.lispCons) else lisptype.NIL
            elems.append(key)
            elems.append(val)
            if isinstance(cur_kw.cdr, lisptype.lispCons) and isinstance(cur_kw.cdr.cdr, lisptype.lispCons):
                cur_kw = cur_kw.cdr.cdr
            else:
                break

        open_call = to_cons(elems)

        # Create binding pair: (var open-call)
        binding_pair = lisptype.lispCons(stream_sym, lisptype.lispCons(open_call, lisptype.NIL))
        let_bindings = lisptype.lispCons(binding_pair, let_bindings)

        # Create close form: (CLOSE var)
        close_form = lisptype.lispCons(lisptype.LispSymbol('CLOSE'), lisptype.lispCons(stream_sym, lisptype.NIL))
        close_forms = lisptype.lispCons(close_form, close_forms)

    # Build PROGN for body
    if body:
        progn_sym = lisptype.LispSymbol('PROGN')
        body_list = lisptype.NIL
        for f in reversed(body):
            body_list = lisptype.lispCons(f, body_list)
        progn_form = lisptype.lispCons(progn_sym, body_list)
    else:
        progn_form = lisptype.NIL

    # Build UNWIND-PROTECT: (UNWIND-PROTECT progn (PROGN close-forms...))
    close_progn = lisptype.lispCons(lisptype.LispSymbol('PROGN'), close_forms) if close_forms is not lisptype.NIL else lisptype.NIL
    unwind = lisptype.lispCons(lisptype.LispSymbol('UNWIND-PROTECT'), lisptype.lispCons(progn_form, lisptype.lispCons(close_progn, lisptype.NIL)))

    # Build LET: (LET bindings unwind)
    let_form = lisptype.lispCons(lisptype.LispSymbol('LET'), lisptype.lispCons(let_bindings, lisptype.lispCons(unwind, lisptype.NIL)))
    return let_form


def _cons_from(seq):
    """Build a Lisp list from a Python sequence."""
    result = lisptype.NIL
    for element in reversed(list(seq)):
        result = lisptype.lispCons(element, result)
    return result


def _progn_of(body):
    """Wrap `body` (a Python sequence of forms) in a PROGN form."""
    if not body:
        return lisptype.NIL
    return lisptype.lispCons(lisptype.LispSymbol('PROGN'), _cons_from(body))


def _binding_parts(spec):
    """Split a `(var form...)` binding spec into (var, [form, ...]).

    The WITH- string-stream macros all take a single parenthesised spec as
    their first subform. That spec is *syntax*, not a form to evaluate --
    which is the whole reason these must be macros: registered as plain
    functions, `(with-output-to-string (stream) ...)` evaluated `(stream)`
    as a call and failed with `Undefined function STREAM`.
    """
    if not isinstance(spec, lisptype.lispCons):
        # A bare symbol is tolerated as shorthand for `(var)`.
        return spec, []
    var = spec.car
    rest = []
    current = spec.cdr
    while isinstance(current, lisptype.lispCons):
        rest.append(current.car)
        current = current.cdr
    return var, rest


def _strip_keywords(forms):
    """Return the leading positional forms, dropping any `:keyword value` tail."""
    positional = []
    for form in forms:
        if isinstance(form, lisptype.lispKeyword):
            break
        positional.append(form)
    return positional


@_registry.cl_macro('PRINT-UNREADABLE-OBJECT',
                    documentation='PRINT-UNREADABLE-OBJECT macro expander (CLHS 22.4)')
def print_unreadable_object_macro(spec, *body):
    """Macro expander for PRINT-UNREADABLE-OBJECT (CLHS 22.4).

    Transforms

        (PRINT-UNREADABLE-OBJECT (object stream :type t :identity t) body...)

    into

        (LET ((#:o object) (#:s stream))
          (%PRINT-UNREADABLE-PREFIX #:o #:s t)
          body...
          (%PRINT-UNREADABLE-SUFFIX #:o #:s t)
          NIL)

    It has to be a macro -- `(x s :type t)` is *syntax*, so a `cl_function`
    would evaluate it as a call -- and the two runtime halves live in
    `io_write.py` because the `#<...>` layout is printer behaviour, not
    macrology. `object` and `stream` are bound once, so a side-effecting
    stream form is not evaluated twice (the prefix and suffix both need it);
    the body is spliced *unchanged* and writes to whatever stream variable it
    already has in scope, which is what CLHS specifies.

    The value is always NIL, and exactly one value -- every test here checks
    `(multiple-value-list ...)` is `(NIL)`, so a body ending in
    `(values 1 2 3)` or `(values)` must not leak through.
    """
    obj_form, rest = _binding_parts(spec)
    stream_form = rest[0] if rest else lisptype.NIL

    # `:type` / `:identity`, in either order; anything else is left alone.
    type_form = lisptype.NIL
    identity_form = lisptype.NIL
    tail = rest[1:]
    for i, item in enumerate(tail):
        if isinstance(item, lisptype.lispKeyword) and i + 1 < len(tail):
            if item.name.upper() == 'TYPE':
                type_form = tail[i + 1]
            elif item.name.upper() == 'IDENTITY':
                identity_form = tail[i + 1]

    obj_var = lisptype.LispSymbol('%PUO-OBJECT')
    stream_var = lisptype.LispSymbol('%PUO-STREAM')
    bindings = _cons_from([_cons_from([obj_var, obj_form]),
                           _cons_from([stream_var, stream_form])])
    prefix = _cons_from([lisptype.LispSymbol('%PRINT-UNREADABLE-PREFIX'),
                         obj_var, stream_var, type_form])
    suffix = _cons_from([lisptype.LispSymbol('%PRINT-UNREADABLE-SUFFIX'),
                         obj_var, stream_var, identity_form])
    return _cons_from(
        [lisptype.LispSymbol('LET'), bindings, prefix]
        + list(body)
        + [suffix, lisptype.NIL]
    )


@_registry.cl_macro('WITH-OUTPUT-TO-STRING',
                    documentation='WITH-OUTPUT-TO-STRING macro expander')
def with_output_to_string_macro(spec, *body):
    """Macro expander for WITH-OUTPUT-TO-STRING (CLHS 21.2).

    Transforms:
      (WITH-OUTPUT-TO-STRING (var) body...)
    into:
      (LET ((var (MAKE-STRING-OUTPUT-STREAM)))
        body...
        (GET-OUTPUT-STREAM-STRING var))

    Supports the :ELEMENT-TYPE keyword parameter:
      (WITH-OUTPUT-TO-STRING (var :element-type 'base-char) body...)

    so the form returns the accumulated string. When a string argument is
    supplied -- `(WITH-OUTPUT-TO-STRING (var string) body...)` -- output is
    appended to that string instead and the form returns the *body's* value,
    per CLHS.

    This was previously a `cl_function` stub that neither evaluated its body
    nor created a stream; because `cl_function` evaluates arguments eagerly,
    its binding spec `(var)` was evaluated as a function call. Every
    FORMATTER test in the ANSI suite is written in terms of this macro, so
    the stub failed all of them with `Undefined function STREAM` regardless
    of whether FORMAT itself was correct.
    """
    var, rest = _binding_parts(spec)
    positional = _strip_keywords(rest)

    # Extract :element-type keyword if present
    keywords = {}
    tail = rest[len(positional):]
    for i in range(0, len(tail) - 1, 2):
        key = tail[i]
        if isinstance(key, lisptype.lispKeyword):
            keywords[str(key.name).upper()] = tail[i + 1]

    # Check if a non-NIL string was supplied as the first positional argument
    has_string = (positional and
                  positional[0] is not None and
                  positional[0] is not lisptype.NIL)

    if has_string:
        # Output accumulates into the supplied string and the value is the
        # body's, so `var` is bound to a stream that *writes into that string*
        # (streams.FillPointerOutputStream). This used to bind a fresh
        # MAKE-STRING-OUTPUT-STREAM and never transfer its contents anywhere,
        # so the supplied string stayed empty however much the body printed --
        # the measurement gate described on that class.
        make_args = [
            lisptype.LispSymbol('%MAKE-FILL-POINTER-OUTPUT-STREAM'),
            positional[0],
        ]
        if 'ELEMENT-TYPE' in keywords:
            make_args.extend([lisptype.intern_keyword('ELEMENT-TYPE'), keywords['ELEMENT-TYPE']])
        stream_form = _cons_from(make_args)
        binding = _cons_from([var, stream_form])
        return _cons_from([
            lisptype.LispSymbol('LET'),
            _cons_from([binding]),
            _progn_of(body),
        ])

    make_args = [lisptype.LispSymbol('MAKE-STRING-OUTPUT-STREAM')]
    if 'ELEMENT-TYPE' in keywords:
        make_args.extend([lisptype.intern_keyword('ELEMENT-TYPE'), keywords['ELEMENT-TYPE']])
    stream_form = _cons_from(make_args)
    binding = _cons_from([var, stream_form])
    get_string = _cons_from([lisptype.LispSymbol('GET-OUTPUT-STREAM-STRING'), var])

    return _cons_from(
        [lisptype.LispSymbol('LET'), _cons_from([binding])]
        + list(body)
        + [get_string]
    )


@_registry.cl_macro('WITH-HASH-TABLE-ITERATOR',
                    documentation='WITH-HASH-TABLE-ITERATOR macro expander (CLHS 18.2)')
def with_hash_table_iterator_macro(spec, *body):
    """Macro expander for WITH-HASH-TABLE-ITERATOR (CLHS 18.2).

    Transforms:
      (WITH-HASH-TABLE-ITERATOR (name hash-table) body...)
    into:
      (LET ((#:state (%MAKE-HASH-TABLE-ITERATOR hash-table)))
        (MACROLET ((name () '(%HASH-TABLE-ITERATOR-NEXT #:state)))
          (LOCALLY body...)))

    Three properties of that shape are the specification, not style.

    **`name` becomes a local *macro*, not a function.** CLHS 18.2 defines it
    as one, and `with-hash-table-iterator.9` checks it directly by asking
    ``(macroexpand '(%x) env)`` to differ from ``(%x)``. It was previously
    registered as a `cl_function` taking a table and returning
    ``iter(table.items())`` -- a raw Python iterator as a Lisp value
    (standing rule 2) -- which is a different operator wearing the name.

    **The table form is evaluated in the `LET`'s init position**, i.e. outside
    the body and before any of the body's declarations take effect. That is
    what `with-hash-table-iterator.12` measures: a body-level
    ``(declare (special x))`` must not reach back into the table form, so
    ``(with-hash-table-iterator (m (return-from done x)) (declare (special
    x)))`` sees the *lexical* X.

    **The body is wrapped in `LOCALLY`** so leading `DECLARE` forms are
    declarations rather than forms to evaluate (`.8`, `.8a`), an empty body
    is NIL (`.1`), and the body's values pass through unmolested -- `.2`
    returns *zero* values and `.3` returns four. `MACROLET` alone would
    evaluate a `DECLARE` as a call.
    """
    from .utilities_symbols import gensym as _gensym_fn

    var, rest = _binding_parts(spec)
    table_form = rest[0] if rest else lisptype.NIL

    state = _gensym_fn()
    make_iterator = _cons_from(
        [lisptype.LispSymbol('%MAKE-HASH-TABLE-ITERATOR'), table_form])
    binding = _cons_from([state, make_iterator])

    # The MACROLET body is *evaluated* to produce the expansion, so the form
    # it must expand to has to be quoted.
    next_call = _cons_from(
        [lisptype.LispSymbol('%HASH-TABLE-ITERATOR-NEXT'), state])
    macro_binding = _cons_from([
        var,
        lisptype.NIL,
        _cons_from([lisptype.LispSymbol('QUOTE'), next_call]),
    ])
    macrolet = _cons_from([
        lisptype.LispSymbol('MACROLET'),
        _cons_from([macro_binding]),
        _cons_from([lisptype.LispSymbol('LOCALLY')] + list(body)),
    ])

    return _cons_from([
        lisptype.LispSymbol('LET'),
        _cons_from([binding]),
        macrolet,
    ])


@_registry.cl_macro('WITH-PACKAGE-ITERATOR',
                    documentation='WITH-PACKAGE-ITERATOR macro expander (CLHS 11.2)')
def with_package_iterator_macro(spec, *body):
    """Macro expander for WITH-PACKAGE-ITERATOR (CLHS 11.2).

    Transforms:
      (WITH-PACKAGE-ITERATOR (name package-list &rest symbol-types) body...)
    into:
      (LET ((#:state (%MAKE-PACKAGE-ITERATOR
                       (LIST pkg-form...) '(:INTERNAL ...))))
        (MACROLET ((name () '(%PACKAGE-ITERATOR-NEXT #:state)))
          (LOCALLY body...)))

    The shape is the specification, and it is the same shape
    WITH-HASH-TABLE-ITERATOR already established (CLHS 18.2's twin):

    **`name` becomes a local *macro*, not a function** -- CLHS defines it as
    one, and the hash-table twin's `.9` checks it directly.

    **The package forms are evaluated in the `LET`'s init position**, outside
    the body, so a body-level `(declare (special x))` cannot reach back into
    them (`with-package-iterator.22` measures exactly that).

    **The body is wrapped in `LOCALLY`** so leading DECLAREs are declarations
    rather than calls, an empty body is NIL, and the body's values pass
    through unmolested.

    The symbol-types are *unevaluated* keyword symbols naming the access
    kinds to visit; each is validated at expansion time against the set CLHS
    names, so a misspelled kind is an error rather than a silently empty
    iteration (standing rule 4).
    """
    from .utilities_symbols import gensym as _gensym_fn

    var, rest = _binding_parts(spec)
    if not rest:
        raise lisptype.LispProgramError(
            "WITH-PACKAGE-ITERATOR: missing package-list form")
    package_forms = rest[0]
    symbol_types = []
    for st in rest[1:]:
        if isinstance(st, lisptype.lispKeyword):
            name = str(st.name).upper()
        elif isinstance(st, lisptype.LispSymbol):
            name = str(st.name).upper()
        else:
            raise lisptype.LispProgramError(
                f"WITH-PACKAGE-ITERATOR: symbol-type {st!r} is not a symbol")
        if name not in ('INTERNAL', 'EXTERNAL', 'INHERITED'):
            raise lisptype.LispProgramError(
                f"WITH-PACKAGE-ITERATOR: unrecognized symbol type :{name}")
        symbol_types.append(name)

    state = _gensym_fn()
    quoted_packages = _cons_from(
        [lisptype.LispSymbol('QUOTE'), package_forms])
    types_list = lisptype.NIL
    for t in reversed(symbol_types):
        types_list = lisptype.lispCons(lisptype.intern_keyword(t), types_list)
    quoted_types = _cons_from([lisptype.LispSymbol('QUOTE'), types_list])

    make_iterator = _cons_from([
        lisptype.LispSymbol('%MAKE-PACKAGE-ITERATOR'),
        package_forms,
        quoted_types,
    ])
    binding = _cons_from([state, make_iterator])

    next_call = _cons_from(
        [lisptype.LispSymbol('%PACKAGE-ITERATOR-NEXT'), state])
    macro_binding = _cons_from([
        var,
        lisptype.NIL,
        _cons_from([lisptype.LispSymbol('QUOTE'), next_call]),
    ])
    macrolet = _cons_from([
        lisptype.LispSymbol('MACROLET'),
        _cons_from([macro_binding]),
        _cons_from([lisptype.LispSymbol('LOCALLY')] + list(body)),
    ])

    return _cons_from([
        lisptype.LispSymbol('LET'),
        _cons_from([binding]),
        macrolet,
    ])


@_registry.cl_macro('WITH-INPUT-FROM-STRING',
                    documentation='WITH-INPUT-FROM-STRING macro expander')
def with_input_from_string_macro(spec, *body):
    """Macro expander for WITH-INPUT-FROM-STRING (CLHS 21.2).

    Transforms:
      (WITH-INPUT-FROM-STRING (var string &key start end index) body...)
    into a form that creates a string input stream, executes the body, and
    updates the :INDEX place with the final stream position if provided.

    The :INDEX parameter, if provided, is a place that will be updated after
    the body executes to contain the final position in the stream.

    The form returns the body's value. Like its output counterpart this was
    a `cl_function` stub, so its binding spec was evaluated as a call.
    """
    var, rest = _binding_parts(spec)
    positional = _strip_keywords(rest)
    string_form = positional[0] if positional else lisptype.NIL

    make_args = [lisptype.LispSymbol('MAKE-STRING-INPUT-STREAM'), string_form]

    # :start / :end select a substring; pass them positionally, which is the
    # argument order MAKE-STRING-INPUT-STREAM already accepts.
    keywords = {}
    tail = rest[len(positional):]
    for i in range(0, len(tail) - 1, 2):
        key = tail[i]
        if isinstance(key, lisptype.lispKeyword):
            keywords[str(key.name).upper()] = tail[i + 1]
    if 'START' in keywords or 'END' in keywords:
        make_args.append(keywords.get('START', 0))
        if 'END' in keywords:
            make_args.append(keywords['END'])

    binding = _cons_from([var, _cons_from(make_args)])

    # If :INDEX is provided, wrap the body to update the index place with the
    # stream's final position after execution.
    if 'INDEX' in keywords:
        index_place = keywords['INDEX']
        # Use an UNWIND-PROTECT-like approach: execute the body, always update
        # the index place, and return the body result.
        if body:
            # (progn body... (setf index-place (stream-position var)))
            # But we need to capture the body result first, then update index,
            # then return the body result. Use a PROG1 pattern.
            wrapped_body = [
                _cons_from([
                    lisptype.LispSymbol('PROG1'),
                    _cons_from([lisptype.LispSymbol('PROGN')] + list(body)),
                    _cons_from([
                        lisptype.LispSymbol('SETF'),
                        index_place,
                        _cons_from([lisptype.LispSymbol('STREAM-POSITION'), var])
                    ])
                ])
            ]
        else:
            # No body: just set index and return nil
            wrapped_body = [
                _cons_from([
                    lisptype.LispSymbol('PROGN'),
                    _cons_from([
                        lisptype.LispSymbol('SETF'),
                        index_place,
                        _cons_from([lisptype.LispSymbol('STREAM-POSITION'), var])
                    ]),
                    lisptype.NIL
                ])
            ]
    else:
        wrapped_body = list(body)

    return _cons_from(
        [lisptype.LispSymbol('LET'), _cons_from([binding])] + wrapped_body
    )


@_registry.cl_macro('ASSERT', documentation='ASSERT macro expander (CLHS 5.1)')
def assert_macro(test_form, *rest):
    """Macro expander for ASSERT (CLHS 5.1):
        (assert test-form [(place*) [datum-form argument-form*]])

    The `(place*)` list names SETF-able places an interactive CONTINUE
    restart could prompt new values into before retrying test-form -- it is
    syntax, not a form to evaluate. `ASSERT` was previously a `cl_function`,
    so `cl_function`'s eager argument evaluation ran that list as an
    ordinary call: `(assert (= (length tail) 0) (tail) ...)` failed with
    `Undefined function TAIL` regardless of whether the assertion would
    have passed -- the same defect class as the WITH-*-STRING macros above.

    This runtime has no interactive user to drive the retry loop, so
    `place*` is accepted per the grammar and otherwise ignored -- but the
    *retry* itself is real: ASSERT's failure is a CONTINUE-restart-bearing
    error (CLHS 5.1: "assert ... repeatedly evaluates test-form until it is
    true"), and now that RESTART-CASE auto-associates a literal `(ERROR
    ...)` protected form with the condition it signals, invoking that
    restart -- directly, or via `(continue)`/`(continue condition)` from a
    handler that has fixed up whatever `test-form` reads -- re-enters the
    loop rather than merely being accepted and ignored. Every non-interactive
    caller in the ANSI suite that does *not* invoke CONTINUE still just gets
    an error signaled, as before.
    """
    datum_and_args = list(rest[1:]) if len(rest) > 1 else []
    if datum_and_args:
        error_call = _cons_from([lisptype.LispSymbol('ERROR')] + datum_and_args)
    else:
        quoted_test = _cons_from([lisptype.LispSymbol('QUOTE'), test_form])
        error_call = _cons_from([
            lisptype.LispSymbol('ERROR'),
            lisptype.LispString("Assertion failed: ~S"),
            quoted_test,
        ])
    continue_clause = _cons_from([
        lisptype.LispSymbol('CONTINUE'), lisptype.NIL,
        lisptype.intern_keyword('REPORT'), lisptype.LispString("Retry the assertion."),
        lisptype.NIL,
    ])
    restart_case_form = _cons_from(
        [lisptype.LispSymbol('RESTART-CASE'), error_call, continue_clause])
    when_form = _cons_from([
        lisptype.LispSymbol('WHEN'), test_form,
        _cons_from([lisptype.LispSymbol('RETURN'), lisptype.NIL])])
    return _cons_from([lisptype.LispSymbol('LOOP'), when_form, restart_case_form])


@_registry.cl_macro('WITH-OPEN-STREAM',
                    documentation='WITH-OPEN-STREAM macro expander')
def with_open_stream_macro(spec, *body):
    """Macro expander for WITH-OPEN-STREAM (CLHS 21.2).

    Transforms:
      (WITH-OPEN-STREAM (var stream-form) body...)
    into:
      (LET ((var stream-form))
        (UNWIND-PROTECT (PROGN body...) (CLOSE var)))

    so the stream is closed however the body exits -- the same shape
    WITH-OPEN-FILE already expands to.
    """
    var, rest = _binding_parts(spec)
    stream_form = rest[0] if rest else lisptype.NIL

    binding = _cons_from([var, stream_form])
    close_form = _cons_from([lisptype.LispSymbol('CLOSE'), var])
    unwind = _cons_from([
        lisptype.LispSymbol('UNWIND-PROTECT'),
        _progn_of(body),
        close_form,
    ])
    return _cons_from([
        lisptype.LispSymbol('LET'),
        _cons_from([binding]),
        unwind,
    ])


def _slot_entries(spec):
    """Parse a WITH-SLOTS/WITH-ACCESSORS binding-spec list into
    `(variable-name, accessor-or-slot-name)` pairs (CLHS 7.5.5).

    Each entry is a bare symbol -- WITH-SLOTS' shorthand for a variable
    named the same as its slot -- or a `(variable-name name)` pair. The
    spec itself is syntax (its symbols name slots/accessors, not variables
    to evaluate), which is why WITH-SLOTS/WITH-ACCESSORS must be macros:
    registered as plain functions, `(with-slots (a b c) obj ...)` evaluated
    `(a b c)` as a call and failed with `Undefined function A` regardless
    of whether the class under test was otherwise correct.
    """
    entries = []
    current = spec
    while _consp_internal(current):
        entry = car(current)
        if isinstance(entry, lisptype.LispSymbol):
            entries.append((entry, entry))
        elif _consp_internal(entry):
            var = car(entry)
            rest = cdr(entry)
            name = car(rest) if _consp_internal(rest) else var
            entries.append((var, name))
        current = cdr(current)
    return entries


def _with_slot_macro(slot_entries, instance_form, body, expansion_of):
    """Shared expansion for WITH-SLOTS and WITH-ACCESSORS: bind the
    instance to a fresh (uncapturable) variable, then SYMBOL-MACROLET each
    entry's variable to `expansion_of(instance_var, name)` around the body.

    Binding the instance through a gensym rather than through the user's
    own variable name is what CLHS means by evaluating instance-form
    "exactly once" while keeping it invisible to the body (`with-slots.14`/
    `.15`/`.16` rebind an unrelated variable of the same name inside the
    body and must not see it).
    """
    from .utilities_symbols import gensym as _gensym_fn

    instance_var = _gensym_fn()
    bindings = [
        _cons_from([var, expansion_of(instance_var, name)])
        for var, name in _slot_entries(slot_entries)
    ]
    symbol_macrolet = _cons_from(
        [lisptype.LispSymbol('SYMBOL-MACROLET'), _cons_from(bindings)] + list(body))
    let_binding = _cons_from([_cons_from([instance_var, instance_form])])
    return _cons_from([lisptype.LispSymbol('LET'), let_binding, symbol_macrolet])


@_registry.cl_macro('WITH-SLOTS', documentation='WITH-SLOTS macro expander (CLHS 7.5.5)')
def with_slots_macro(slot_entries, instance_form, *body):
    """Macro expander for WITH-SLOTS (CLHS 7.5.5).

    Transforms:
      (WITH-SLOTS (slot-entry*) instance-form decl* form*)
    into:
      (LET ((#:instance instance-form))
        (SYMBOL-MACROLET ((var1 (SLOT-VALUE #:instance 'slot1)) ...)
          decl* form*))

    Was a `cl_function` stub that evaluated every body form and discarded
    all but the last, establishing none of the slot bindings -- the same
    defect class as WITH-STANDARD-IO-SYNTAX and the WITH-*-STRING macros
    before they were fixed.
    """
    def slot_value_of(instance_var, slot_name):
        quoted = _cons_from([lisptype.LispSymbol('QUOTE'), slot_name])
        return _cons_from([lisptype.LispSymbol('SLOT-VALUE'), instance_var, quoted])

    return _with_slot_macro(slot_entries, instance_form, body, slot_value_of)


@_registry.cl_macro('WITH-ACCESSORS', documentation='WITH-ACCESSORS macro expander (CLHS 7.5.5)')
def with_accessors_macro(slot_entries, instance_form, *body):
    """Macro expander for WITH-ACCESSORS (CLHS 7.5.5).

    Transforms:
      (WITH-ACCESSORS ((var1 accessor1) ...) instance-form decl* form*)
    into:
      (LET ((#:instance instance-form))
        (SYMBOL-MACROLET ((var1 (accessor1 #:instance)) ...)
          decl* form*))

    Same defect as WITH-SLOTS above: a `cl_function` stub with no bindings.
    """
    def accessor_call_of(instance_var, accessor_name):
        return _cons_from([accessor_name, instance_var])

    return _with_slot_macro(slot_entries, instance_form, body, accessor_call_of)


def _cl(name):
    """The *interned* COMMON-LISP symbol named `name`.

    A macro expander must not build its variable names with a bare
    `LispSymbol(...)`. A global variable's home is the symbol's own value
    cell and lookup is by symbol *identity* (CLAUDE.md, "the global
    environment has no lexical variables"), so a freshly constructed
    `*PRINT-BASE*` would be bound and read as a *different* variable from the
    interned one the printer consults.
    """
    return lisptype.COMMON_LISP_PACKAGE.intern_symbol(name)


def _standard_io_syntax_bindings():
    """The binding list CLHS 23.4 gives WITH-STANDARD-IO-SYNTAX.

    Built fresh on each expansion because two of the values are objects, not
    literals: the standard readtable and the standard pprint dispatch table.
    """
    from fclpy.readtable import standard_readtable
    from .io_write import standard_pprint_dispatch

    def quoted(obj):
        return _cons_from([lisptype.LispSymbol('QUOTE'), obj])

    return [
        # CLHS names the package by string rather than by the value of
        # *PACKAGE* at expansion time, and WITH-STANDARD-IO-SYNTAX.1 checks
        # exactly that: it runs under `(let ((*package* (find-package
        # :cl-test))) ...)` and requires *PACKAGE* to be CL-USER inside.
        (_cl('*PACKAGE*'), _cons_from([lisptype.LispSymbol('FIND-PACKAGE'),
                                       lisptype.LispString("COMMON-LISP-USER")])),
        (_cl('*PRINT-ARRAY*'), lisptype.T),
        (_cl('*PRINT-BASE*'), 10),
        (_cl('*PRINT-CASE*'), quoted(lisptype.intern_keyword('UPCASE'))),
        (_cl('*PRINT-CIRCLE*'), lisptype.NIL),
        (_cl('*PRINT-ESCAPE*'), lisptype.T),
        (_cl('*PRINT-GENSYM*'), lisptype.T),
        (_cl('*PRINT-LENGTH*'), lisptype.NIL),
        (_cl('*PRINT-LEVEL*'), lisptype.NIL),
        (_cl('*PRINT-LINES*'), lisptype.NIL),
        (_cl('*PRINT-MISER-WIDTH*'), lisptype.NIL),
        (_cl('*PRINT-PPRINT-DISPATCH*'), quoted(standard_pprint_dispatch())),
        (_cl('*PRINT-PRETTY*'), lisptype.NIL),
        (_cl('*PRINT-RADIX*'), lisptype.NIL),
        (_cl('*PRINT-READABLY*'), lisptype.T),
        (_cl('*PRINT-RIGHT-MARGIN*'), lisptype.NIL),
        (_cl('*READ-BASE*'), 10),
        (_cl('*READ-DEFAULT-FLOAT-FORMAT*'), quoted(_cl('SINGLE-FLOAT'))),
        (_cl('*READ-EVAL*'), lisptype.T),
        (_cl('*READ-SUPPRESS*'), lisptype.NIL),
        # "The standard readtable" -- the object itself, not a copy. It is
        # immutable (CLHS 23.1.1), and ansi-test rebinds *READTABLE* to
        # `(copy-readtable nil)` wherever it means to modify one.
        (_cl('*READTABLE*'), quoted(standard_readtable())),
    ]


@_registry.cl_macro('DEFINE-COMPILER-MACRO',
                    documentation='DEFINE-COMPILER-MACRO macro expander')
def define_compiler_macro_macro(*args):
    """DEFINE-COMPILER-MACRO (CLHS 3.2.2.1) expander: quote the definition
    into a call to `%DEFINE-COMPILER-MACRO`, the runtime half (in
    utilities_functions.py) that builds the macro function through the one
    macro binder and installs it as the name's compiler macro function.
    Expanding to a *call* -- rather than registering as a side effect of
    expansion -- is what makes the definition happen at LOAD time of
    compiled output, like every defining form. `name` may be a symbol or a
    ``(setf symbol)`` function name. The expander receives the call's raw
    arguments (name lambda-list body...), the same convention every
    cl_macro expander follows."""
    import fclpy.lisptype as lisptype

    def quote(x):
        return _ccm_list([lisptype.LispSymbol('QUOTE'), x])

    name = args[0] if args else lisptype.NIL
    lambda_list = args[1] if len(args) > 1 else lisptype.NIL
    body_forms = [quote(item) for item in args[2:]]

    # ANSI: the macro function of a defining macro signals PROGRAM-ERROR
    # when funcalled malformed -- (macro-function 'define-compiler-macro)
    # funcalled with 0/1 args, or with a name that is neither a symbol nor
    # a (setf symbol) function name (define-compiler-macro.error.1-.3).
    if len(args) < 3:
        raise lisptype.LispProgramError(
            "DEFINE-COMPILER-MACRO requires a name, a lambda list and a body")
    valid_name = (isinstance(name, lisptype.LispSymbol)
                  or (_consp_internal(name) and len(args[0]) == 2
                      and isinstance(car(name), lisptype.LispSymbol)
                      and car(name).name.upper() == 'SETF'
                      and isinstance(car(cdr(name)), lisptype.LispSymbol)))
    if not valid_name:
        raise lisptype.LispProgramError(
            "DEFINE-COMPILER-MACRO: invalid function name")

    call_items = [lisptype.LispSymbol('%DEFINE-COMPILER-MACRO'),
                  quote(name), quote(lambda_list)] + body_forms
    return _ccm_list(call_items)


def _ccm_list(items):
    """A Lisp list from a Python sequence (local to the compiler-macro
    expanders)."""
    result = lisptype.NIL
    for item in reversed(items):
        result = lisptype.lispCons(item, result)
    return result


@_registry.cl_macro('WITH-COMPILATION-UNIT',
                    documentation='WITH-COMPILATION-UNIT macro expander')
def with_compilation_unit_macro(options, *body):
    """Macro expander for WITH-COMPILATION-UNIT (CLHS 3.2.5).

    Transforms

        (WITH-COMPILATION-UNIT (option-name option-form ...) form*)

    into

        (PROGN option-form ... NIL form*)

    which is the whole of the standardized semantics for an implementation
    that does not defer compiler diagnostics: the option *forms* are evaluated
    ("override -- a generalized boolean; evaluated"), their values are
    discarded, and the form's value, values and non-local exits are the
    body's. The trailing NIL is what makes an empty body answer NIL rather
    than the last option's value.

    It was a `cl_function` -- the registry defect CLAUDE.md describes -- and
    all four of its consequences showed: the option list `(:OVERRIDE NIL)` was
    *evaluated as a function call*, so every test that passed an option
    signalled UNDEFINED-FUNCTION OVERRIDE; the body forms were evaluated
    before the form ran; only the last one's *primary* value came back, so
    `(with-compilation-unit () (values 1 2 3 4 5))` answered 1 and
    `(values)` answered NIL instead of no values; and a `RETURN-FROM` out of
    the body could not be a non-local exit from a form that had already
    finished evaluating its arguments.
    """
    option_forms = []
    cur = options
    while isinstance(cur, lisptype.lispCons):
        # (name form name form ...): the names are syntax, the forms are
        # evaluated. A trailing name with no form contributes nothing.
        cur = cur.cdr
        if isinstance(cur, lisptype.lispCons):
            option_forms.append(cur.car)
            cur = cur.cdr

    return _cons_from([lisptype.LispSymbol('PROGN')] + option_forms
                      + [lisptype.NIL] + list(body))


@_registry.cl_macro('WITH-STANDARD-IO-SYNTAX',
                    documentation='WITH-STANDARD-IO-SYNTAX macro expander')
def with_standard_io_syntax_macro(*body):
    """Macro expander for WITH-STANDARD-IO-SYNTAX (CLHS 23.4).

    Transforms:
      (WITH-STANDARD-IO-SYNTAX body...)
    into:
      (LET ((*PACKAGE* (FIND-PACKAGE "COMMON-LISP-USER")) ...21 bindings...)
        body...)

    so the form's value, its multiple values and any non-local exit out of it
    are LET's, which is what CLHS requires and what
    `WITH-STANDARD-IO-SYNTAX.19/.20/.21/.22` check.

    It was a `cl_function` whose body was "evaluate every argument eagerly,
    return the last", i.e. it established **none** of the twenty-one bindings
    -- `(let ((*print-base* 2)) (with-standard-io-syntax (prin1-to-string 5)))`
    answered "101" where ANSI requires "5". That is the registry defect
    CLAUDE.md describes: a form whose subforms must be evaluated in a
    modified dynamic environment cannot be a `cl_function`, because
    `cl_function` evaluates them before the form runs at all.

    Expanding to LET is deliberate and is not a second binding mechanism:
    `BindingFrame` already decides lexical vs. dynamic, and every one of these
    variables is proclaimed special by `lispenv.STANDARD_SPECIAL_VARIABLES`,
    so LET binds all twenty-one in their value cells -- the one home the
    printer and reader read them from.

    455 uses across 58 ansi-test files go through this macro, `def-pprint-test`
    among them, so it gates the pretty-printer test vocabulary the way
    `(copy-readtable nil)` gated `printer/print-integers.lsp`.
    """
    bindings = [_cons_from([var, init]) for var, init in _standard_io_syntax_bindings()]
    return _cons_from(
        [lisptype.LispSymbol('LET'), _cons_from(bindings)] + list(body)
    )


def eval_pprint_logical_block(form, env):
    """PPRINT-LOGICAL-BLOCK special form (CLHS 22.2.2).

    (PPRINT-LOGICAL-BLOCK (stream-symbol object-form
                           &key prefix per-line-prefix suffix)
      declaration* form*)

    `stream-symbol` is a syntactic position, not a form to evaluate -- either
    a symbol already bound (in the enclosing environment) to a stream, which
    gets a fresh binding to a (possibly per-line-prefix-wrapped) stream for
    the body's dynamic extent, or the literal designator T/NIL. A
    `cl_function` stub used to be registered under this name: since a
    `cl_function`'s arguments are evaluated eagerly, `(pprint-logical-block
    (os 1))` evaluated `(os 1)` as a call to a function named OS -- the same
    registry defect CLAUDE.md documents for WITH-STANDARD-IO-SYNTAX, and for
    the same reason: a form whose first "argument" is unevaluated syntax
    cannot be a plain function.

    Everything but the binding and the body's implicit `BLOCK NIL` is
    `io_write.pprint_logical_block_setup`'s job (the "not a list" bypass,
    `*PRINT-LEVEL*` truncation, and prefix/per-line-prefix output), so it is
    not duplicated between this call site and PPRINT-POP/
    PPRINT-EXIT-IF-LIST-EXHAUSTED, which consult the same frame stack.
    """
    from .evaluation_core import eval, ReturnFromException
    from . import io_write as _io_write
    import fclpy.state as _state

    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispProgramError("PPRINT-LOGICAL-BLOCK requires a binding list")

    spec = car(args)
    body = cdr(args)

    var, rest = _binding_parts(spec)
    if not rest:
        raise lisptype.LispProgramError("PPRINT-LOGICAL-BLOCK requires an object form")
    object_form = rest[0]
    plist = rest[1:]

    prefix_form = per_line_prefix_form = suffix_form = lisptype.NIL
    prefix_given = per_line_prefix_given = suffix_given = False
    i = 0
    while i < len(plist) - 1:
        key, value = plist[i], plist[i + 1]
        if isinstance(key, lisptype.LispSymbol):
            if key.name == 'PREFIX':
                prefix_form, prefix_given = value, True
            elif key.name == 'PER-LINE-PREFIX':
                per_line_prefix_form, per_line_prefix_given = value, True
            elif key.name == 'SUFFIX':
                suffix_form, suffix_given = value, True
        i += 2

    if prefix_given and per_line_prefix_given:
        # CLHS 22.2.2: at most one of :PREFIX, :PER-LINE-PREFIX may be given.
        raise lisptype.LispProgramError(
            "PPRINT-LOGICAL-BLOCK: :PREFIX and :PER-LINE-PREFIX are mutually exclusive")

    object_value = eval(object_form, env)

    if _null_internal(var):
        stream_designator = lisptype.NIL
        bind_var = None
    elif var is lisptype.T or (isinstance(var, lisptype.LispSymbol) and var.name == 'T'):
        stream_designator = lisptype.T
        bind_var = None
    else:
        stream_designator = eval(var, env)
        bind_var = var

    prefix_value = eval(prefix_form, env)
    per_line_prefix_value = eval(per_line_prefix_form, env)
    suffix_value = eval(suffix_form, env)

    kind, stream, frame, suffix_text = _io_write.pprint_logical_block_setup(
        stream_designator, object_value, prefix_value, per_line_prefix_value, suffix_value,
        prefix_given=prefix_given, per_line_prefix_given=per_line_prefix_given,
        suffix_given=suffix_given)

    if kind == 'atom':
        _io_write.write_text(_io_write._write_object(object_value), stream)
        return lisptype.NIL
    if kind == 'level-exceeded':
        _io_write.write_text('#', stream)
        return lisptype.NIL

    block_env = lisptype.Environment(env)
    bound_vars = [bind_var] if bind_var is not None else []
    bf = BindingFrame(block_env, body=body, bound_vars=bound_vars)
    if bind_var is not None:
        bf.bind(bind_var, frame.stream)

    # CLHS 22.2.2.1: the body is executed as an implicit block nil. Its frame
    # is registered on `block_env`, so a RETURN-FROM NIL in the body -- or in
    # a closure the body defines -- resolves here by lexical identity. The
    # raw `(RETURN-FROM NIL)` that io_write's `%PPRINT-EXIT-IF-LIST-EXHAUSTED`
    # raises from Python carries no frame (`block_frame is None`) and is still
    # caught by the legacy name rule below.
    from .evaluation_control_flow import (
        establish_block_frame, deactivate_frame)
    nil_block = establish_block_frame(block_env, lisptype.NIL)

    old_env = _state.current_environment
    _state.current_environment = block_env
    _state.pprint_stack.append(frame)
    try:
        try:
            current = body
            while _consp_internal(current):
                eval(car(current), block_env)
                current = cdr(current)
        except ReturnFromException as e:
            if e.block_frame is not nil_block and not (
                    e.block_frame is None and _null_internal(e.tag)):
                raise
        _io_write.flush_pprint_frame(frame, suffix_text)
        return lisptype.NIL
    finally:
        deactivate_frame(nil_block)
        _state.pprint_stack.pop()
        _state.current_environment = old_env
        bf.unwind()


def _canonicalize_nil_symbol(value):
    """Canonicalize an *interned* non-keyword symbol spelled NIL to the
    singleton, so a parameter bound from it compares EQ to NIL (CLAUDE.md:
    "NIL has three Python spellings").

    A KEYWORD named NIL (`:NIL`) is not one of those spellings -- CLHS makes
    it a distinct, ordinary (and truthy) symbol, and `(eq :nil nil)` is NIL --
    so it must never be coerced here. The four call sites that inlined this
    check each wrote `isinstance(a, lisptype.LispSymbol)`, which is also true
    of `lispKeyword` (KEYWORD is a subtype of SYMBOL), so a `:NIL` argument
    was silently rewritten to the unbindable NIL constant before any
    keyword-argument matching saw it: `(make-struct-test-66 :nil 5)` (a
    structure with a slot literally named NIL) turned `:NIL` into NIL at the
    front door and then failed keyword-argument binding on "NIL is not a
    valid keyword argument name".

    An *uninterned* symbol named NIL is equally not a spelling of NIL:
    `(eq (make-symbol "NIL") nil)` is NIL (CLHS eq), and ansi-test's
    `*universe*` carries one (`#:nil` from `*uninterned-symbols*`), which is
    how BOOLEAN-TYPE.3 caught this -- the predicate argument path rewrote
    `#:nil` to NIL so `is-t-or-nil` answered T for it while `(typep #:nil
    'boolean)` answered NIL. "Interned" is the same test `_null_internal`
    applies (`package is not None`), so the two cannot drift apart.
    """
    if (isinstance(value, lisptype.LispSymbol) and not isinstance(value, lisptype.lispKeyword)
            and value.package is not None
            and value.name.upper() == 'NIL'):
        return lisptype.NIL
    return value


def _current_frame_keyword_context():
    """The CLHS 7.6.5 keyword-argument validity of the generic-function
    call whose method is currently executing, or None outside any method --
    see `classes.call_frame_keyword_context`."""
    return _clos_classes.call_frame_keyword_context()


def keyword_argument_key(symbol):
    """The identity on which a `&key` parameter and an actual argument match.

    CLHS 3.4.1.4: `&key b` declares the keyword `:B` -- the symbol B in the
    KEYWORD package -- whereas `((b var) init)` declares whatever symbol B
    names in the package it was read in, and CLHS 3.4.1.4.1.1 allows *any*
    symbol there, not only a keyword. Matching on the upper-cased name alone
    conflates the two, so `((lambda (&key b) b) 'b 100)` bound B from a
    non-keyword symbol the lambda list never named -- it should be an
    unrecognized keyword argument.

    Symbols are compared by (package name, symbol name) rather than by
    identity because a lambda list may be built in Python, where the symbol
    object differs from the interned one of the same name.
    """
    package = getattr(symbol, 'package', None)
    package_name = getattr(package, 'name', None)
    return (package_name, symbol.name.upper())


def _keyword_param_parts(param_spec):
    """Decompose one `&key` parameter spec into
    ``(keyword-symbol, variable, default-form, supplied-p)``.

    CLHS 3.4.1.4 gives four shapes -- ``var``, ``(var init)``,
    ``(var init supplied-p)`` and ``((keyword-name var) init [supplied-p])``.
    The fourth is why this is a function: the keyword a parameter answers to
    and the variable it binds are not always the same name, and the two loops
    that used to decompose these specs (once to install defaults, once to
    match an actual argument) each assumed they were, so
    ``&key ((:x y) 9)`` bound Y to 9 whatever ``:x`` was given. They also
    disagreed -- only one of them read the default form -- which is the
    ordinary consequence of writing the same decomposition twice.

    The keyword is returned as a *symbol*, not a name: in the first three
    shapes CLHS 3.4.1.4 says the parameter answers to the keyword of the same
    name -- ``&key b`` means ``:B``, regardless of the package the variable B
    itself lives in -- while in the fourth it answers to the symbol written,
    whatever package that is. Compare two of them with `keyword_argument_key`.
    """
    if not _consp_internal(param_spec):
        return lisptype.intern_keyword(param_spec.name), param_spec, None, None

    head = car(param_spec)
    tail = cdr(param_spec)
    default_form = car(tail) if _consp_internal(tail) else None
    tail2 = cdr(tail) if _consp_internal(tail) else None
    supplied_p = car(tail2) if _consp_internal(tail2) else None

    if _consp_internal(head):
        # ((keyword-name var) init [supplied-p])
        keyword_name = car(head)
        variable = car(cdr(head)) if _consp_internal(cdr(head)) else None
        if not isinstance(keyword_name, lisptype.LispSymbol):
            raise lisptype.LispProgramError(
                f"&key parameter name must be a symbol, not {keyword_name!r}")
        return keyword_name, variable, default_form, supplied_p

    return lisptype.intern_keyword(head.name), head, default_form, supplied_p


def _bind_keyword_parameters(parsed, trailing, func_env, eval_fn, frame, default_fallback=None):
    """Bind a user lambda list's `&key` parameters from the keyword region
    (CLHS 3.4.1.4, 3.5.1.5).

    `trailing` is every argument after the required and &optional ones -- the
    whole keyword region, decided by the lambda list rather than guessed from
    the argument values. Within it the standard applies in full: an odd number
    of arguments is a PROGRAM-ERROR, the *leftmost* pair wins for a repeated
    keyword, and a keyword the lambda list does not name is a PROGRAM-ERROR
    unless `&allow-other-keys` was declared or `:ALLOW-OTHER-KEYS` is true in
    the call itself.

    This mirrors `evaluation_core._split_declared_keywords`, which applies the
    same section of the standard to a *builtin's* Python signature. The two
    cannot be one function -- one reads a Lisp lambda list and binds into an
    environment, the other reads a Python signature and builds a kwargs dict --
    but they must agree, and this is the copy that did not.

    `frame` is the caller's `BindingFrame`, so a parameter the body declares
    SPECIAL binds in the symbol's value cell rather than lexically
    (CLHS 3.4.1/11.1.2.1.2). Every parameter in every section goes through it.
    """
    keyword_params = parsed['keyword']
    allow_other_keys = bool(parsed.get('allow_other_keys'))

    specs = [_keyword_param_parts(spec) for spec in keyword_params]

    if len(trailing) % 2:
        raise lisptype.LispProgramError(
            f"odd number of keyword arguments: {trailing[-1]!r} has no value")

    allow_other_keys_key = keyword_argument_key(
        lisptype.intern_keyword('ALLOW-OTHER-KEYS'))

    pairs = []
    for i in range(0, len(trailing), 2):
        key = trailing[i]
        if not lisptype.is_symbol(key) or key is lisptype.NIL:
            raise lisptype.LispProgramError(
                f"{key!r} is not a valid keyword argument name")
        pairs.append((keyword_argument_key(key), trailing[i + 1]))

    # CLHS 3.4.1.4.1: the leftmost :ALLOW-OTHER-KEYS pair governs.
    for name, value in pairs:
        if name == allow_other_keys_key:
            allow_other_keys = allow_other_keys or lisptype.is_truthy(value)
            break

    declared_names = {keyword_argument_key(keyword)
                      for keyword, _variable, _default, _supplied in specs}

    # Leftmost pair wins for a repeated keyword; an unrecognized name is a
    # PROGRAM-ERROR resolved before any default-value form runs, matching
    # CLHS 3.4.1.4's argument-processing pass.
    #
    # `:ALLOW-OTHER-KEYS` is *always* permissible (CLHS 3.4.1.4.1) but it is
    # not thereby excluded from the argument list: a lambda list may declare
    # `((:allow-other-keys aok))` as an ordinary `&key` parameter and must
    # then receive the value, which skipping the pair outright prevented.
    supplied_values = {}
    for name, value in pairs:
        if name in supplied_values:
            continue
        if (name not in declared_names and name != allow_other_keys_key
                and not allow_other_keys):
            # CLHS 7.6.5: the keyword arguments accepted by a generic
            # function for a particular call are the union of those named
            # by its own lambda list and by every applicable method's. A
            # method binder therefore must not reject a keyword the
            # *generic function* accepted -- reinitialize-instance.8's
            # :after method binds `&key x` while the call passes other
            # keywords, and defgeneric.28's `(fn 1 :bar 'b)` passes a
            # keyword only a less-specific applicable method names. The
            # frame carries what the dispatching call computed; outside
            # any method (an ordinary DEFUN) there is no frame and this
            # check is exactly what it was before.
            frame_ctx = _current_frame_keyword_context()
            frame_keys, frame_aok = frame_ctx if frame_ctx is not None else (None, False)
            if not frame_aok and (frame_keys is None or name not in frame_keys):
                raise lisptype.LispProgramError(
                    f"unrecognized keyword argument: {name[1]}")
        supplied_values[name] = value

    # CLHS 3.4.1.1: a parameter's init-form is evaluated, in left-to-right
    # lambda-list order, in an environment where every *earlier* parameter
    # -- whether defaulted or supplied -- is already bound. Evaluating
    # every default form first and only afterward overwriting the supplied
    # ones (the previous structure here) broke that: a later parameter's
    # default form referencing an earlier `&key` parameter always saw that
    # earlier parameter at its OWN default, never at the value the caller
    # actually supplied. `pathnames/make-pathname.lsp`'s own test helper is
    # exactly this shape --
    # `(defun make-pathname-test (&rest args &key (defaults nil) (device
    # (if defaults (pathname-device defaults) ...)) ...))` -- so
    # `(make-pathname-test :defaults *default-pathname-defaults*)` derived
    # every expected component from `defaults`, which read back NIL for
    # every parameter after the first regardless of what was passed.
    for keyword, variable, default_form, supplied_p in specs:
        if variable is None:
            continue
        name = keyword_argument_key(keyword)
        if name in supplied_values:
            frame.bind(variable, supplied_values[name])
            if supplied_p is not None:
                frame.bind(supplied_p, lisptype.T)
        else:
            effective_default = default_form
            if effective_default is None and default_fallback is not None:
                effective_default = default_fallback(variable)
            if effective_default is not None:
                frame.bind(variable, eval_fn(effective_default, func_env))
            else:
                frame.bind(variable, lisptype.NIL)
            if supplied_p is not None:
                frame.bind(supplied_p, lisptype.NIL)


def _bind_ordinary_lambda_list_tail(parsed, call_args, arg_index, func_env, eval_fn, frame,
                                     default_fallback=None):
    """Bind &optional/&rest/&key/&aux parameters (CLHS 3.4.1) into `func_env`
    from `call_args`, starting at `arg_index` -- i.e. after any required
    parameters already bound positionally by the caller. Shared by DEFUN's
    ordinary lambda list and DEFMETHOD/DEFGENERIC's specialized lambda list,
    whose &optional/&rest/&key/&aux tail is itself ordinary (CLHS 7.6.2:
    only required parameters may be specialized) -- before this was
    extracted, a specialized lambda list's tail was flattened to bare
    parameter names and bound positionally with a NIL fallback, so a method
    with `&optional (x 1)` bound X to NIL instead of 1 when omitted, a
    supplied-p variable was never bound at all (raising Unbound variable on
    first read), and &key/&rest/&aux were not supported in a method lambda
    list at all.

    `frame` is the caller's `BindingFrame`. Every parameter is established
    through it rather than through `func_env.add_variable`, because a
    parameter the function body declares SPECIAL must bind dynamically
    (CLHS 3.4.1, 11.1.2.1.2) and be undone on exit however the function
    exits. `add_variable` can express neither.

    `default_fallback`, when given, is consulted for an &optional/&key
    parameter that has *no* default-value form of its own, in place of the
    ordinary "no form means NIL" rule -- CLHS 3.4.6's BOA constructor rule:
    an omitted default-value form for a parameter that names a DEFSTRUCT slot
    takes that slot's own default initform instead of NIL. Every other caller
    passes nothing and gets the ordinary behavior unchanged.
    """
    optional_params = parsed['optional']
    rest_param = parsed['rest']
    keyword_params = parsed['keyword']
    aux_params = parsed.get('aux', [])

    # Bind optional parameters (support supplied-p variable)
    for param_spec in optional_params:
        if _consp_internal(param_spec):
            param = car(param_spec)
            rest = cdr(param_spec)
            default_form = car(rest) if _consp_internal(rest) else None
            rest2 = cdr(rest) if _consp_internal(rest) else None
            supplied_p = car(rest2) if _consp_internal(rest2) else None
        else:
            param = param_spec
            default_form = None
            supplied_p = None

        if arg_index < len(call_args):
            frame.bind(param, call_args[arg_index])
            if supplied_p is not None:
                frame.bind(supplied_p, lisptype.T)
            arg_index += 1
        else:
            # Use default value if provided, else the fallback (BOA
            # constructors only), else NIL.
            effective_default = default_form
            if effective_default is None and default_fallback is not None:
                effective_default = default_fallback(param)
            if effective_default is not None:
                default_value = eval_fn(effective_default, func_env)
                frame.bind(param, default_value)
            else:
                frame.bind(param, lisptype.NIL)
            if supplied_p is not None:
                frame.bind(supplied_p, lisptype.NIL)

    # From here on, `arg_index` is the start of the *keyword region*: CLHS
    # 3.4.1 puts it immediately after the required and &optional parameters,
    # and that is a property of the lambda list, not of what the arguments
    # happen to look like. This code used to scan the arguments for the first
    # keyword-shaped value instead, which got both halves wrong:
    #
    #   * `&rest` received only the values *before* that scan stopped, so
    #     `(defun g (a &rest args) ...)` called as `(g 1 :b 2)` bound ARGS to
    #     NIL. CLHS is explicit that &rest gets *all* the remaining arguments,
    #     the ones the &key parameters also consume included -- which is the
    #     whole point of the `&rest args &key ...` idiom the ANSI suite's own
    #     helpers are written in (`load-file-test` forwards its ARGS to LOAD,
    #     so LOAD never saw a single `:verbose`/`:print` argument).
    #   * a non-keyword value in the keyword region silently became a
    #     positional argument instead of the PROGRAM-ERROR CLHS 3.5.1.5 asks
    #     for -- the same defect `LambdaListShape` was introduced to remove on
    #     the builtin side, here in the user-lambda-list copy.
    trailing = list(call_args[arg_index:])

    if rest_param:
        rest_list = lisptype.NIL
        for item in reversed(trailing):
            rest_list = lisptype.lispCons(item, rest_list)

        # Support destructuring rest spec: either a symbol or a cons
        if isinstance(rest_param, lisptype.LispSymbol):
            frame.bind(rest_param, rest_list)
        elif _consp_internal(rest_param):
            # Dotted pair like (head . tail): bind head to first element,
            # tail to the cdr (list) of the rest_list.
            head = car(rest_param)
            tail = rest_param.cdr
            if _consp_internal(rest_list):
                first = car(rest_list)
                rest_tail = cdr(rest_list)
            else:
                first = lisptype.NIL
                rest_tail = lisptype.NIL

            if isinstance(head, lisptype.LispSymbol):
                frame.bind(head, first)
            if isinstance(tail, lisptype.LispSymbol):
                frame.bind(tail, rest_tail)

    # `mentions_key`, not `keyword_params`: `&key` naming no parameters at all
    # still opens the keyword region, so `(lambda (&rest x &key) x)` must
    # reject `(:w 5)` as an unrecognized keyword (CLHS 3.4.1.4) rather than
    # accept anything. Testing the parameter *list* made a bare `&key`
    # indistinguishable from no `&key` at all.
    if keyword_params or parsed.get('mentions_key') or parsed.get('allow_other_keys'):
        _bind_keyword_parameters(parsed, trailing, func_env, eval_fn, frame,
                                  default_fallback=default_fallback)

    # Bind &aux parameters
    for param_spec in aux_params:
        if _consp_internal(param_spec):
            param = car(param_spec)
            init_form = car(cdr(param_spec))
            init_value = eval_fn(init_form, func_env)
            frame.bind(param, init_value)
        else:
            frame.bind(param_spec, lisptype.NIL)


def split_function_body(body):
    """Split a function body into ``(docstring, declaration_forms, forms)``.

    CLHS 3.4.11: a body may open with any number of declarations and at most
    one documentation string, interleaved in any order. A *lone* string is the
    body, not documentation -- documentation must be followed by at least one
    form -- which is why `(defun f () "x")` returns "x".
    """
    docstring = None
    declarations = []
    rest = body
    while _consp_internal(rest):
        first = car(rest)
        if (_consp_internal(first) and isinstance(car(first), lisptype.LispSymbol)
                and car(first).name.upper() == 'DECLARE'):
            declarations.append(first)
            rest = cdr(rest)
            continue
        if (docstring is None and isinstance(first, (str, lisptype.LispString))
                and _consp_internal(cdr(rest))):
            docstring = str(first)
            rest = cdr(rest)
            continue
        break
    return docstring, declarations, rest


def _lambda_list_variables(parsed):
    """Every variable an ordinary lambda list binds, supplied-p variables
    included -- what `BindingFrame` needs in order to tell a *bound* SPECIAL
    declaration (which changes where the parameter itself is bound) from a
    *free* one (which only redirects references in the body). CLHS 3.3.4
    turns on exactly that distinction.
    """
    variables = []

    def add(var):
        if isinstance(var, lisptype.LispSymbol):
            variables.append(var)

    for param in parsed['required']:
        add(param)
    for spec in parsed['optional']:
        if _consp_internal(spec):
            add(car(spec))
            tail = cdr(spec)
            tail2 = cdr(tail) if _consp_internal(tail) else None
            if _consp_internal(tail2):
                add(car(tail2))
        else:
            add(spec)
    rest_param = parsed['rest']
    if rest_param is not None:
        if _consp_internal(rest_param):
            add(car(rest_param))
            add(rest_param.cdr)
        else:
            add(rest_param)
    for spec in parsed['keyword']:
        _name, variable, _default, supplied_p = _keyword_param_parts(spec)
        add(variable)
        add(supplied_p)
    for spec in parsed.get('aux', []):
        add(car(spec) if _consp_internal(spec) else spec)
    environment = parsed.get('environment')
    if environment is not None:
        add(environment)
    return variables


def _check_ordinary_arity(parsed, call_args, name):
    """CLHS 3.5.1.2/3.5.1.3: too few or too many arguments is a PROGRAM-ERROR.

    Every binder in this implementation used to pad a missing required
    argument with NIL and discard a surplus one, so `((lambda (a) a))`
    answered NIL and `((lambda (a) a) 1 2)` answered 1. Those are wrong
    *values*, not merely missing errors: a caller cannot tell a legitimately
    NIL argument from one that was never passed.
    """
    supplied = len(call_args)
    required = len(parsed['required'])
    if supplied < required:
        raise lisptype.LispProgramError(
            f"{name} called with {supplied} argument(s), but requires "
            f"at least {required}")

    # A lambda list that mentions &rest, &body, &key or &allow-other-keys
    # accepts everything past its positional parameters; only one without any
    # of them has an upper bound.
    if (parsed['rest'] is not None or parsed.get('mentions_rest')
            or parsed.get('mentions_key') or parsed.get('allow_other_keys')):
        return
    maximum = required + len(parsed['optional'])
    if supplied > maximum:
        raise lisptype.LispProgramError(
            f"{name} called with {supplied} argument(s), but accepts "
            f"at most {maximum}")


def _signal_program_error(message):
    """Raise CLHS 3.5.1's PROGRAM-ERROR as a Lisp *condition*, not a Python
    exception.

    The binders that run at macroexpansion time need this rather than a bare
    `LispProgramError`: the ordinary-call path converts that Python exception
    into a condition (eval's call try), but the macroexpansion dispatch does
    not -- it propagates conditions unchanged. Raising the condition directly
    is therefore what lets `signals-error`'s handler match a program-error,
    the same reasoning as `coerce_to_function`, which raises its
    UNDEFINED-FUNCTION condition directly.
    """
    from .evaluation_core import ConditionException
    raise ConditionException(
        lisptype.ProgramError(message=message), recoverable=False)


def _check_destructuring_arity(pattern, value, name):
    """CLHS 3.5.1.2 for a destructuring lambda list (CLHS 3.4.4/3.4.5): the
    pattern's required parameters consume one element each, so a value that
    supplies too few is a PROGRAM-ERROR -- not the silent binding of NIL to
    every element that is missing. Nested required patterns are checked
    against their own element, and so is a nested &WHOLE pattern against the
    whole value.

    &OPTIONAL/&KEY/&AUX positions are deliberately *not* checked: an absent
    element takes its default, and validating a nested pattern against that
    default would mean evaluating a default form here -- running program code
    a pure arity check must not run. Surplus elements are also not an error:
    a destructuring lambda list simply ignores them, because only &REST/
    &BODY would name them.
    """
    if isinstance(pattern, lisptype.LispSymbol) or pattern is None \
            or pattern is lisptype.NIL:
        return
    if not _consp_internal(pattern):
        return

    from .evaluation_core import parse_lambda_list
    parsed = parse_lambda_list(pattern)

    # How many elements the value supplies -- an atom supplies none, which is
    # the same walk `bind_destructuring_pattern` performs when it would have
    # bound NIL to everything missing.
    supplied = 0
    cur = value
    while _consp_internal(cur):
        supplied += 1
        cur = cdr(cur)

    required = parsed.get('required', [])
    if len(required) > supplied:
        _signal_program_error(
            "{}: the destructuring pattern {} requires {} element(s), but "
            "the value {} supplies {}".format(
                name, pattern, len(required), value, supplied))
        return

    cur = value
    for param in required:
        element = car(cur) if _consp_internal(cur) else lisptype.NIL
        if _consp_internal(param):
            _check_destructuring_arity(param, element, name)
        cur = cdr(cur) if _consp_internal(cur) else cur

    whole = parsed.get('whole')
    if whole is not None and _consp_internal(whole):
        _check_destructuring_arity(whole, value, name)


def make_ordinary_function(lambda_list, body, env, block_name=None, name=None):
    """Build the callable for a function defined by an *ordinary* lambda list.

    The one constructor behind LAMBDA, DEFUN, FLET and LABELS. Before this
    there were three, and they agreed on almost nothing: LAMBDA located the
    keyword region by scanning the arguments for the first keyword-shaped
    value (so `&rest` never saw the keyword arguments and a repeated keyword
    took the *rightmost* value), FLET/LABELS had a hand-rolled parser that did
    not use `parse_lambda_list` at all and silently dropped every supplied-p
    variable, `&aux` and `&allow-other-keys`, and none of the three signalled
    a PROGRAM-ERROR for a wrong argument count or an unrecognized keyword.
    DEFUN alone reached `_bind_ordinary_lambda_list_tail`, which is correct --
    so the fix is to delete the other two rather than repair them.

    Three properties the shape of this function encodes:

    * **Parameters bind through a `BindingFrame`**, so `(declare (special x))`
      on a parameter binds the symbol's value cell for the call's dynamic
      extent (CLHS 11.1.2.1.2) and is undone however the call exits.
    * **Free special declarations are installed only after the parameters are
      bound.** CLHS 3.3.4 excludes initialization forms from a free
      declaration's scope, so `(lambda (&aux (y x)) (declare (special x)) y)`
      reads the *lexical* X for the init form. That is what
      `defer_free_declarations` is for.
    * **The implicit block encloses the body only, not the lambda list.**
      A `(return-from f ...)` in an `&aux` init form therefore leaves the
      function rather than returning from it -- FLET.6 asserts exactly that.
    """
    from .evaluation_core import eval, parse_lambda_list
    from .evaluation_loops_conditionals import _implicit_block_frame
    from .binding import BindingFrame

    parsed = parse_lambda_list(lambda_list)
    docstring, declarations, forms = split_function_body(body)
    parameters = _lambda_list_variables(parsed)
    required_params = parsed['required']
    environment_param = parsed.get('environment')

    # `BindingFrame` reads the declarations governing its bindings off the
    # body it is handed, and `split_declarations` stops at the first non-
    # DECLARE form -- so hand it just the declarations, which a docstring
    # sitting in front of them would otherwise hide.
    declaration_body = lisptype.NIL
    for decl in reversed(declarations):
        declaration_body = lisptype.lispCons(decl, declaration_body)

    if isinstance(name, lisptype.LispSymbol):
        display_name = name.name
    elif name:
        display_name = str(name)
    else:
        display_name = 'anonymous function'

    def call(*call_args):
        from .evaluation_core import (_enter_lisp_call, _leave_lisp_call,
                                      ConditionException, TailCall)
        if _enter_lisp_call(display_name):
            # CLHS STORAGE-CONDITION, signalled while Python stack remains
            # (recursion-plan.md Step 5) -- not a RecursionError later.
            raise ConditionException(
                lisptype.StorageCondition(
                    message=f"Stack overflow calling {display_name}: "
                            f"Lisp recursion exceeded the available stack"),
                recoverable=False)
        try:
            # recursion-plan.md Step 4: the self tail-call trampoline. A call
            # to *this* closure from the body's tail position comes back from
            # `eval` as a `TailCall` marker instead of consuming another ~6
            # Python frames per level; the parameters are re-bound from it and
            # the body runs again in *this* frame. A self-recursive Lisp
            # function therefore costs O(1) Python stack, which is what lets
            # ansi-test's own spine-recursive helpers finish at the default
            # recursion limit -- `check-cons-copy` (auxiliary/cons-aux.lsp)
            # recurses on the cdr spine, so its depth is the *list length*:
            # 700 for COPY-TREE.2's `*universe*`, ~4200 Python frames before.
            #
            # The loop is written out here rather than in a `_call_once`
            # helper, because such a helper costs one Python frame per
            # activation and would buy unbounded *tail* depth at the price of
            # *non-tail* depth (measured: 197 -> 124 levels). Every frame this
            # closure holds is multiplied by the recursion depth of the
            # program being run, so the trampoline has to be free.
            while True:
                call_args = tuple(_canonicalize_nil_symbol(a)
                                  for a in call_args)

                _check_ordinary_arity(parsed, call_args, display_name)

                func_env = lisptype.Environment(env)
                frame = BindingFrame(func_env, body=declaration_body,
                                     bound_vars=parameters,
                                     defer_free_declarations=True)
                try:
                    for index, param in enumerate(required_params):
                        frame.bind(param, call_args[index])

                    _bind_ordinary_lambda_list_tail(
                        parsed, call_args, len(required_params), func_env,
                        eval, frame)

                    if environment_param is not None:
                        frame.bind(environment_param, env)

                    frame.install_free_declarations()

                    # The body loop runs directly in this frame -- no
                    # `run_body` thunk, and the implicit block via
                    # `_implicit_block_frame`, whose `with` holds no frame
                    # while the body runs. Going through
                    # `_run_with_nil_block(run_body, ...)` here cost two held
                    # frames per level of Lisp recursion (~8 Python frames per
                    # level, capping default-limit recursion at ~140 levels --
                    # recursion-plan.md Step 3).
                    #
                    # The body's *last* form is evaluated with
                    # `tail_target=call`, so a self call there answers a
                    # `TailCall` for the loop above instead of recursing.
                    # Earlier body forms are not in tail position and get no
                    # target. The implicit block does not prevent this, and
                    # that is the case that matters -- every DEFUN has one, so
                    # excluding it would mean Step 4 never fires for a named
                    # function. It is sound because the block's extent
                    # genuinely ends when the tail call is taken: `with blk`
                    # exits normally as the marker is returned, this
                    # activation's `frame.unwind()` runs, and the next
                    # activation pushes a *fresh* block frame. RETURN-FROM
                    # resolves by frame identity (M7), so a closure made
                    # during activation N and called during N+1 sees N's frame
                    # as out of extent and gets a CONTROL-ERROR -- correct,
                    # because N has returned. That is what a tail call means.
                    if block_name is None:
                        result = lisptype.NIL
                        current = forms
                        while _consp_internal(current):
                            rest = cdr(current)
                            if _consp_internal(rest):
                                result = eval(car(current), func_env)
                            else:
                                result = eval(car(current), func_env,
                                              tail_target=call)
                            current = rest
                    else:
                        # The implicit block's frame is registered on
                        # `func_env` only here, after the parameters are
                        # bound, so the body forms (and closures they define)
                        # resolve a RETURN-FROM to it through the lexical
                        # chain -- while an &aux/&key init form, evaluated in
                        # `func_env` *before* this registration, sees no block
                        # and its RETURN-FROM leaves the function (FLET.6).
                        blk = _implicit_block_frame(block_name, func_env)
                        with blk:
                            result = lisptype.NIL
                            current = forms
                            while _consp_internal(current):
                                rest = cdr(current)
                                if _consp_internal(rest):
                                    result = eval(car(current), func_env)
                                else:
                                    result = eval(car(current), func_env,
                                                  tail_target=call)
                                current = rest
                        if blk.caught:
                            result = blk.value
                finally:
                    # Unwinds *before* the next activation binds, which is the
                    # order a real tail call gives. Note the arguments were
                    # already evaluated at the call site, while this
                    # activation's bindings were still live, and the marker
                    # carries values -- so nothing is read after this point.
                    frame.unwind()

                if type(result) is not TailCall:
                    return result
                if result.kwargs:
                    # A keyword argument reaching here would have to be
                    # re-split against the lambda list. `eval` only emits a
                    # marker for a call it has already split into positionals,
                    # so this is unreachable -- loudly, because silently
                    # dropping them would be a wrong value, not a crash.
                    raise lisptype.LispError(
                        "internal: TailCall carried keyword arguments")
                call_args = result.args
        finally:
            _leave_lisp_call()

    call.__lisp_docstring__ = docstring
    call.__lisp_lambda_list__ = lambda_list
    call.__lisp_name__ = name
    return call


def function_name_parts(spec, operator):
    """A function-name designator's ``(storage-symbol, block-name)``.

    CLHS 3.1.2.1.2.2 / 5.1: a function name is a symbol or ``(SETF symbol)``,
    and the implicit block a defining form establishes is named by `symbol`
    in both cases -- so ``(defun (setf foo) ...)`` may say
    ``(return-from foo ...)``. `operator` only names the caller in the error.

    DEFUN, FLET and LABELS all need both halves; FLET and LABELS previously
    tested `isinstance(name, LispSymbol)` and silently defined nothing for
    every other shape, which is why a local ``(setf %f)`` function -- and a
    local function named NIL -- simply did not exist.
    """
    from .utilities_functions import _function_spec_to_key

    if lisptype.is_symbol(spec):
        symbol = spec if isinstance(spec, lisptype.LispSymbol) else lisptype.LispSymbol('NIL')
        return symbol, symbol
    key = _function_spec_to_key(spec)
    if key is None:
        raise lisptype.LispProgramError(
            f"{operator}: function name must be a symbol or (SETF symbol), not {spec!r}")
    return key, car(cdr(spec))


def eval_defun(form, env):
    """Evaluate DEFUN special form.

    DEFUN defines a function in the GLOBAL environment, not the local one.
    This is standard Common Lisp behavior - DEFUN creates top-level function
    bindings. The lambda list is bound by `make_ordinary_function`, the one
    constructor shared with LAMBDA, FLET and LABELS.
    """
    args = cdr(form)
    if not _consp_internal(args) or not _consp_internal(cdr(args)):
        raise lisptype.LispNotImplementedError("DEFUN requires at least 2 arguments")

    func_name_spec = car(args)
    param_list = car(cdr(args))
    body = cdr(cdr(args))

    func_name, block_name_symbol = function_name_parts(func_name_spec, 'DEFUN')

    user_function = make_ordinary_function(
        param_list, body, env, block_name=block_name_symbol, name=func_name)
    docstring = user_function.__lisp_docstring__

    # Find the global/root environment for defining the function
    # DEFUN always creates global function bindings
    global_env = env
    while global_env.parent is not None:
        global_env = global_env.parent
    
    # Add function to the GLOBAL environment (not local). Deliberately NOT
    # mirrored into the lexical environment either: CLHS 3.1.2.1.2 makes
    # DEFUN a *global* definition, and a lexical copy shadows every later
    # global redefinition of the same name for the extent of this binding --
    # `(defun g ...)` inside a LET followed by `(setf (macro-function g) ...)`
    # kept calling the old function (macro-function.10). The global binding
    # is already visible here through the parent chain.
    global_env.add_function(func_name, user_function)
    
    # Store docstring on the function symbol's property list
    if docstring:
        if not hasattr(func_name, 'plist'):
            func_name.plist = {}
        func_name.plist['DOCUMENTATION'] = docstring
        # Also on the callable itself, so `(documentation (symbol-function
        # sym) t)` -- the *function object*, not the symbol -- can read it
        # (CLHS 25.1.3; documentation.function.t.2).
        user_function.__doc__ = str(docstring)

    # CLHS: DEFUN returns the function *name* -- as written, so a
    # ``(defun (setf foo) ...)`` answers the list ``(SETF FOO)`` and not
    # the internal storage symbol (define-compiler-macro.4's second check).
    return func_name_spec
def _create_macro_function(macro_name, lambda_list, body, env,
                           unsupplied_default=None):
    """Create a macro function callable from lambda-list and body.

    This is used by DEFMACRO, MACROLET and DEFTYPE to create macro functions.
    The resulting function has __is_macro__ = True and captures the defining
    environment for proper lexical scoping.

    Args:
        macro_name: LispSymbol for the macro name (used for debugging)
        lambda_list: Lisp list of parameter specifications
        body: Lisp list of body forms
        env: The environment where the macro is defined (captured for closure)
        unsupplied_default: what an &OPTIONAL/&KEY parameter with *no default
            form* gets when the caller omits it. NIL for a macro, but the symbol
            `*` for a DEFTYPE lambda list (CLHS 4.2.3) -- which is the whole
            reason DEFTYPE can share this binder instead of getting a seventh
            copy of it (plan.md Finding C). It is why
            `(deftype foo (&optional x) `(integer 0 ,x))` written bare denotes
            `(integer 0 *)`, i.e. UNSIGNED-BYTE, rather than `(integer 0 nil)`.

    Returns:
        A callable with __is_macro__ = True
    """
    from .evaluation_core import (eval, parse_lambda_list, bind_destructuring_pattern,
                                   destructuring_pattern_variables)
    from .binding import BindingFrame

    if unsupplied_default is None:
        unsupplied_default = lisptype.NIL

    # Extract docstring if present (first form in body can be a string)
    docstring = None
    actual_body = body
    if _consp_internal(body):
        first_form = car(body)
        if isinstance(first_form, (str, lisptype.LispString)):
            docstring = str(first_form)  # Convert to Python str for storage
            actual_body = cdr(body)

    # Parse lambda list to handle &optional, &rest, &key, &whole, &environment etc.
    parsed_params = parse_lambda_list(lambda_list)

    required_params = parsed_params.get('required', [])
    optional_params = parsed_params.get('optional', [])
    rest_param = parsed_params.get('rest', None)
    keyword_params = parsed_params.get('keyword', [])
    environment_param = parsed_params.get('environment', None)

    # Every variable this lambda list binds -- what BindingFrame needs to
    # tell a *bound* SPECIAL declaration (naming one of this macro's own
    # parameters, which changes where that parameter is bound: CLHS 3.3.4)
    # from a *free* one (which only redirects references in the body).
    pattern_vars = destructuring_pattern_variables(lambda_list)

    # Create the macro callable
    def macro_callable(*call_args):
        # Create a new environment extending the definition environment
        macro_env = lisptype.Environment(parent=env)

        # A macro function's parameters bind through a BindingFrame, exactly
        # like an ordinary lambda list (make_ordinary_function): a parameter
        # the body declares SPECIAL must bind the symbol's value cell for the
        # macroexpansion call's extent, not always lexically in macro_env.
        # MACROLET.44/.45 are this: a macro parameter shares a name with a
        # variable the *caller's* environment declares special, and a closure
        # captured there must see the macro's dynamic binding when invoked
        # from inside the macro's own body via CALL-NEXT-METHOD-style FUNCALL.
        # `defer_free_declarations=True` mirrors CLHS 3.3.4: a free
        # declaration governs the body, not a parameter's own default-form.
        frame = BindingFrame(macro_env, body=actual_body, bound_vars=pattern_vars,
                              defer_free_declarations=True)

        # Detect optional trailing expansion-time Environment argument.
        # The macroexpander may invoke the macro callable with the
        # expansion environment as the last positional argument. If so,
        # use it for &ENVIRONMENT bindings; otherwise use the captured
        # definition environment.
        expansion_env = env
        if len(call_args) > 0 and isinstance(call_args[-1], lisptype.Environment):
            expansion_env = call_args[-1]
            call_args = tuple(call_args[:-1])

        call_args = tuple(_canonicalize_nil_symbol(a) for a in call_args)

        arg_idx = 0

        # Handle &WHOLE parameter - binds the entire macro form (passed as first arg
        # when __expects_whole__ is true), using destructuring if the pattern is complex
        whole_param = parsed_params.get('whole') if isinstance(parsed_params, dict) else None
        if whole_param is not None:
            if len(call_args) > 0:
                whole_form = call_args[0]
                bind_destructuring_pattern(whole_param, whole_form, macro_env, frame)
                arg_idx = 1
            else:
                # No whole form provided; bind to NIL
                bind_destructuring_pattern(whole_param, lisptype.NIL, macro_env, frame)
                arg_idx = 1

        # CLHS 3.5.1.2/3.5.1.3 for the macro lambda list (CLHS 3.4.4): too
        # few or too many arguments is a PROGRAM-ERROR, the same rule
        # make_ordinary_function applies to ordinary lambda lists --
        # `_check_ordinary_arity` is the one home of that decision, and this
        # reuses it rather than a second copy. The count covers the
        # *positional* arguments: after the macroexpander's trailing
        # expansion-environment argument has been stripped, and, when &WHOLE
        # is declared, accounting for the whole form it consumes. The error
        # is signalled as a condition (`_signal_program_error`), because the
        # macroexpansion dispatch propagates conditions, not Python
        # exceptions, and `signals-error`'s handler must see program-error.
        positional = (call_args[1:] if len(call_args) > 0 else call_args) \
            if whole_param is not None else call_args

        # parse_lambda_list records only *bindable* required parameters: a
        # required parameter that is the empty-list pattern binds nothing
        # and is dropped from that list (macrolet.39's `(())`), but it still
        # consumes one positional argument. Count the dropped shapes so the
        # arity check sees the real positional capacity.
        dropped_required = 0
        ll_cursor = lambda_list
        while _consp_internal(ll_cursor):
            item = car(ll_cursor)
            if isinstance(item, lisptype.LispSymbol) and item.name.startswith('&'):
                marker = item.name.upper()
                if marker in ('&WHOLE', '&ENVIRONMENT'):
                    # Their parameter is not a positional slot; skip it.
                    ll_cursor = cdr(cdr(ll_cursor))
                    continue
                break
            if not isinstance(item, lisptype.LispSymbol) \
                    and not _consp_internal(item):
                dropped_required += 1
            ll_cursor = cdr(ll_cursor)
        check_parsed = dict(parsed_params)
        if dropped_required:
            check_parsed['required'] = list(required_params) \
                + [None] * dropped_required

        def _looks_like_macro_funcall_convention(args):
            """CLHS's macro-function calling convention delivers the whole
            macro form and the expansion environment -- macro-function.7
            FUNCALLs `(macro-function '%m)` as `(fn '(%m) nil)`. The whole
            form is a compound form whose operator is *this* macro, and the
            trailing argument is the (possibly NIL) environment; anything
            else is an ordinary dispatch-convention call.
            """
            if not args or not _consp_internal(args[0]):
                return False
            head = car(args[0])
            if not isinstance(head, lisptype.LispSymbol) \
                    or not isinstance(macro_name, lisptype.LispSymbol) \
                    or head.name.upper() != macro_name.name.upper():
                return False
            if len(args) == 1:
                return True
            tail = args[-1]
            return tail is None or tail is lisptype.NIL \
                or isinstance(tail, lisptype.Environment)

        try:
            _check_ordinary_arity(check_parsed, positional, macro_name)
        except lisptype.LispProgramError as e:
            if not _looks_like_macro_funcall_convention(positional):
                _signal_program_error(str(e))
            # The excess is the macro-function convention's own
            # (whole-form, environment) pair: bind nothing for it, exactly
            # as this binder always did for the surplus of a direct FUNCALL.

        # A required parameter may itself be a nested destructuring pattern
        # (CLHS 3.4.4); its element must supply the nested pattern's own
        # required parameters (CLHS 3.5.1.2, the same rule one level down).
        for index, param in enumerate(required_params):
            if _consp_internal(param) and index < len(positional):
                _check_destructuring_arity(param, positional[index], macro_name)

        # Bind &ENVIRONMENT to the expansion-time environment if provided
        # The macro callable may be invoked with an extra trailing Environment
        # argument by the macroexpander. If so, prefer that; otherwise fall
        # back to the environment captured at definition time.
        if environment_param is not None:
            frame.bind(environment_param, expansion_env)

        # Bind required parameters. A required parameter spec may be a plain
        # symbol or an arbitrary nested destructuring pattern (CLHS 3.4.4,
        # e.g. `(arg1 (&whole w arg2))` or `(&rest vars)`); either shape is
        # handled by the one shared destructuring binder rather than a second
        # case here for every lambda-list-keyword combination that can appear
        # nested.
        for param in required_params:
            val = call_args[arg_idx] if arg_idx < len(call_args) else lisptype.NIL
            bind_destructuring_pattern(param, val, macro_env, frame)
            arg_idx += 1

        # A destructuring-pattern parameter name (in &OPTIONAL/&KEY position)
        # is bound the same way a required one is.
        def _bind_pattern(pat, val):
            bind_destructuring_pattern(pat, val, macro_env, frame)

        def _kw_parts(param):
            """Split a &key parameter name into (keyword-name, var-pattern).

            `param` is either a plain symbol (keyword name implied by the
            symbol) or a compound ((:keyword var-pattern)) spec, where
            var-pattern may itself be a destructuring pattern rather than a
            plain symbol.
            """
            if isinstance(param, lisptype.LispSymbol):
                return param.name.upper(), param
            if _consp_internal(param):
                kw = car(param)
                var_pattern = car(cdr(param)) if _consp_internal(cdr(param)) else None
                kw_name = kw.name.upper() if isinstance(kw, (lisptype.LispSymbol, lisptype.lispKeyword)) else None
                return kw_name, var_pattern
            return None, None

        # Bind optional parameters
        for param_spec in optional_params:
            # Support (name default supplied-p) optional syntax
            if isinstance(param_spec, lisptype.LispSymbol):
                opt_name = param_spec
                opt_default = None
                supplied_p = None
            elif _consp_internal(param_spec):
                opt_name = car(param_spec)
                rest = cdr(param_spec)
                opt_default = car(rest) if _consp_internal(rest) else None
                rest2 = cdr(rest) if _consp_internal(rest) else None
                supplied_p = car(rest2) if _consp_internal(rest2) else None
            else:
                # Unknown shape - skip defensively
                continue

            if arg_idx < len(call_args):
                val = call_args[arg_idx]
                if isinstance(opt_name, lisptype.LispSymbol):
                    frame.bind(opt_name, val)
                else:
                    _bind_pattern(opt_name, val)
                if supplied_p is not None:
                    frame.bind(supplied_p, lisptype.T)
                arg_idx += 1
            else:
                if opt_default is not None:
                    default_value = eval(opt_default, macro_env)
                else:
                    default_value = unsupplied_default

                if isinstance(opt_name, lisptype.LispSymbol):
                    frame.bind(opt_name, default_value)
                else:
                    _bind_pattern(opt_name, default_value)

                if supplied_p is not None:
                    frame.bind(supplied_p, lisptype.NIL)

        # Bind &rest parameter (which may be a plain symbol or a destructuring pattern)
        if rest_param:
            remaining_args = call_args[arg_idx:]
            if remaining_args:
                rest_list = lisptype.NIL
                for arg in reversed(remaining_args):
                    rest_list = cons(arg, rest_list)
            else:
                rest_list = lisptype.NIL

            if isinstance(rest_param, lisptype.LispSymbol):
                # Simple case: &REST var binds var to the list of remaining args
                frame.bind(rest_param, rest_list)
            else:
                # Destructuring case: &REST (pattern) destructures remaining args
                # This handles &REST (X Y Z) to bind X, Y, Z to individual elements
                bind_destructuring_pattern(rest_param, rest_list, macro_env, frame)
        
        # Bind &key parameters (CLHS 3.4.1.1): an init-form is evaluated, in
        # left-to-right lambda-list order, in an environment where every
        # *earlier* parameter -- whether defaulted or supplied -- is already
        # bound. This used to bind every parameter to its own default value
        # FIRST and only afterward overwrite the ones the caller actually
        # supplied, in a separate second pass -- so a later parameter's
        # default form referencing an earlier one always saw that earlier
        # parameter at its own default, never at the caller's value.
        # `_bind_keyword_parameters` (this file, used by DEFUN/LET/DO) was
        # already fixed for exactly this shape; this is DEFMACRO/MACROLET/
        # DEFTYPE's own separate lambda-list binder and had not been.
        # `streams/open.lsp`'s own `def-open-output-test`/`def-open-io-test`
        # test-generating macros are this shape --
        # `(&rest keyargs &key (element-type 'character) (build-form (cond
        # ((subtypep element-type 'integer) ...) ...)) ...)` -- so
        # `(def-open-output-test ... :element-type '(unsigned-byte 12))`
        # built its write loop against ELEMENT-TYPE's *default* (CHARACTER),
        # not the type the caller actually passed, regardless of what
        # ELEMENT-TYPE itself later read as.
        keyword_start = arg_idx
        supplied = {}
        # CLHS 3.5.1.4/3.5.1.5: an odd number of keyword arguments is a
        # PROGRAM-ERROR (a keyword with no value), and so is a keyword the
        # lambda list does not name -- unless &ALLOW-OTHER-KEYS was declared
        # or the call itself carried a non-NIL :allow-other-keys marker
        # (CLHS 3.4.1.4). Non-keyword atoms in the region are still skipped:
        # with &REST preceding &KEY the region holds the rest arguments too.
        #
        # The scan is gated on the lambda list actually *naming* keyword
        # parameters (`keyword_params`): without them there is no keyword
        # region to check and trailing keywords are ordinary &rest/body
        # elements -- the ansi-test harness's own `(defmacro deftest
        # (name &rest body) ...)` passes its expected values, `:good` among
        # them, through that &REST, and scanning them as keywords would
        # reject the harness itself. A bare `&key` naming no parameters
        # therefore accepts any keywords; the keyword region is a property
        # of the lambda list, never inferred from what the arguments look
        # like.
        if keyword_params:
            i = keyword_start
            while i < len(call_args):
                key = call_args[i]
                if isinstance(key, lisptype.lispKeyword):
                    if i + 1 >= len(call_args):
                        _signal_program_error(
                            "odd number of keyword arguments: :{} has no "
                            "value".format(key.name))
                    key_name = key.name.upper()
                    if key_name not in supplied:
                        supplied[key_name] = call_args[i + 1]
                    i += 2
                else:
                    i += 1

            if keyword_params and not parsed_params.get('allow_other_keys'):
                if not lisptype.is_truthy(supplied.get('ALLOW-OTHER-KEYS',
                                                       lisptype.NIL)):
                    # keyword_params holds full specs -- `((:foo bar) default)`
                    # or plain `foo` -- and the keyword name is the spec's
                    # first part, the same split the binding loop below
                    # performs.
                    declared = set()
                    for param_spec in keyword_params:
                        param = car(param_spec) if _consp_internal(param_spec) \
                            else param_spec
                        kw_name, _var_pattern = _kw_parts(param)
                        if kw_name is not None:
                            declared.add(kw_name)
                    for key_name in supplied:
                        if key_name != 'ALLOW-OTHER-KEYS' \
                                and key_name not in declared:
                            _signal_program_error(
                                "unrecognized keyword argument: "
                                ":{}".format(key_name))

        for param_spec in keyword_params:
            if _consp_internal(param_spec):
                param = car(param_spec)
                rest = cdr(param_spec)
                default_form = car(rest) if _consp_internal(rest) else None
                rest2 = cdr(rest) if _consp_internal(rest) else None
                supplied_p = car(rest2) if _consp_internal(rest2) else None
            else:
                param = param_spec
                default_form = None
                supplied_p = None

            kw_name, var_pattern = _kw_parts(param)

            if kw_name in supplied:
                _bind_pattern(var_pattern, supplied[kw_name])
                if supplied_p is not None:
                    frame.bind(supplied_p, lisptype.T)
            else:
                if default_form is not None:
                    default_value = eval(default_form, macro_env)
                else:
                    default_value = unsupplied_default
                _bind_pattern(var_pattern, default_value)
                if supplied_p is not None:
                    frame.bind(supplied_p, lisptype.NIL)

        # Bind &aux parameters (CLHS 3.4.4 includes &aux in the macro lambda
        # list): sequential like LET*, each init form evaluated in an
        # environment where the parameters bound before it are already
        # bound. A bare &aux variable binds `unsupplied_default` (NIL for a
        # macro, `*` for a DEFTYPE), the same default an absent &OPTIONAL
        # init-form takes here.
        for aux_spec in parsed_params.get('aux', []):
            if isinstance(aux_spec, lisptype.LispSymbol):
                _bind_pattern(aux_spec, unsupplied_default)
            elif _consp_internal(aux_spec):
                aux_name = car(aux_spec)
                aux_rest = cdr(aux_spec)
                init_form = car(aux_rest) if _consp_internal(aux_rest) else None
                init_value = eval(init_form, macro_env) if init_form is not None \
                    else unsupplied_default
                _bind_pattern(aux_name, init_value)

        try:
            frame.install_free_declarations()

            # If no body, return NIL
            if not _consp_internal(actual_body):
                return lisptype.NIL

            # Evaluate the body inside an implicit BLOCK named for the macro.
            # This mirrors DEFUN/DEFMACRO semantics where the function/macro
            # body is implicitly a BLOCK so RETURN-FROM can target the name.
            block_form = lisptype.lispCons(lisptype.LispSymbol('BLOCK'), lisptype.lispCons(macro_name, actual_body))
            return eval(block_form, macro_env)
        finally:
            frame.unwind()

    # Mark as macro
    setattr(macro_callable, '__is_macro__', True)
    if isinstance(parsed_params, dict) and parsed_params.get('whole') is not None:
        setattr(macro_callable, '__expects_whole__', True)
    # Indicate that this macro function expects an expansion-time environment
    # if the lambda-list contained &ENVIRONMENT
    if environment_param is not None:
        setattr(macro_callable, '__expects_environment__', True)
    
    # Store docstring if present
    if docstring and isinstance(macro_name, lisptype.LispSymbol):
        if not hasattr(macro_name, 'plist'):
            macro_name.plist = {}
        macro_name.plist['DOCUMENTATION'] = docstring
        # Also on the callable itself, so `(documentation (macro-function sym)
        # t)` -- the *function object*, not the symbol -- can read it
        # (CLHS 25.1.3; documentation.function.t.4).
        macro_callable.__doc__ = str(docstring)

    return macro_callable


def eval_defmacro(form, env):
    """Evaluate DEFMACRO special form: register a macro in the environment.

    This creates a Python callable that evaluates the macro body in an
    environment where the parameters are bound to the arguments. This allows
    QUASIQUOTE/UNQUOTE to work correctly in macro templates.
    """
    args = cdr(form)
    if not _consp_internal(args) or not _consp_internal(cdr(args)):
        raise lisptype.LispNotImplementedError("DEFMACRO requires a name, lambda-list and body")

    macro_name = car(args)
    lambda_list = car(cdr(args))
    body = cdr(cdr(args))

    if not isinstance(macro_name, lisptype.LispSymbol):
        raise lisptype.LispNotImplementedError("DEFMACRO: macro name must be a symbol")

    # Extract docstring if present (first form in body can be a string)
    docstring = None
    if _consp_internal(body):
        first_form = car(body)
        if isinstance(first_form, (str, lisptype.LispString)):
            docstring = str(first_form)  # Convert to Python str for storage
    
    # Create the macro function using the shared helper
    macro_callable = _create_macro_function(macro_name, lambda_list, body, env)
    
    # Find the global/root environment for defining the macro
    # DEFMACRO always creates global macro bindings (like DEFUN)
    global_env = env
    while global_env.parent is not None:
        global_env = global_env.parent
    
    # Add macro to the GLOBAL environment (not local). Same rule as DEFUN
    # above: DEFMACRO is a global definition (CLHS 3.1.2.1.2); a lexical
    # copy would shadow later global redefinitions for the extent of this
    # binding and is already visible here through the parent chain.
    global_env.add_function(macro_name, macro_callable)
    
    # Store docstring on the macro symbol's property list
    if docstring:
        if not hasattr(macro_name, 'plist'):
            macro_name.plist = {}
        macro_name.plist['DOCUMENTATION'] = docstring
    
    return macro_name


def eval_macroexpand_1(form, env):
    """Evaluate MACROEXPAND-1 special form.
    
    (MACROEXPAND-1 form) - expand a macro call one level.
    If form is a macro call, expands the macro and returns the expansion.
    Otherwise returns form unchanged.
    """
    from .evaluation_core import eval
    
    args = cdr(form)
    if not _consp_internal(args):
        # Signal a PROGRAM-ERROR for wrong argument count per ANSI CL
        raise lisptype.LispProgramError("MACROEXPAND-1 requires 1 argument")
    
    # MACROEXPAND-1 is an ordinary *function* (CLHS 3.8), so both of its
    # arguments are evaluated, exactly once, left to right -- which is what
    # `macroexpand-1.6` measures:
    #
    #   (macroexpand-1 (progn (setf a (incf i)) form)
    #                  (progn (setf b (incf i)) nil))   => i=2, a=1, b=2
    #
    # This used to evaluate its first argument only when it was *literally*
    # `(QUOTE x)` and otherwise take it unevaluated, i.e. behave as a special
    # operator. The `(quote ...)` case is the one that appears in test source,
    # so the heuristic looked right while any other argument expression --
    # a variable, a PROGN, a function call -- was silently treated as the form
    # to expand rather than evaluated to produce it. Same defect as
    # MACRO-FUNCTION's, in the neighbouring function.
    form_to_expand = eval(car(args), env)

    # The optional environment argument. NIL (and an omitted argument) both
    # mean the null lexical environment; anything else is used as given.
    expand_env = env
    env_args = cdr(args)
    if _consp_internal(env_args):
        if _consp_internal(cdr(env_args)):
            # `(macroexpand-1 form env extra)` -- MACROEXPAND-1 takes at most
            # two arguments (CLHS 3.8), so a third is a PROGRAM-ERROR. As a
            # special-operator branch this path does no arity checking of its
            # own, so surplus arguments were simply ignored.
            raise lisptype.LispProgramError(
                "MACROEXPAND-1 takes at most 2 arguments")
        supplied_env = eval(car(env_args), env)
        if supplied_env is not None and not _null_internal(supplied_env):
            expand_env = supplied_env

    # MACROEXPAND-1 returns *two* values -- the expansion and whether an
    # expansion happened (CLHS 3.8). It returned one, so every
    # `(multiple-value-list (macroexpand-1 x))` was one element short and the
    # three `check-predicate` tests here failed for every object in the
    # universe.
    def _unexpanded(value):
        return lisptype.MultipleValues(value, lisptype.NIL)

    # CLHS 3.8: a symbol that names a symbol-macro also expands. This was
    # the missing half -- `(symbol-macrolet ((a b)) (macrolet ((foo (x
    # &environment env) (eq (macroexpand-1 x env) 'a))) (foo a)))` must
    # answer NIL (A macroexpands to B, so they are not EQ); with only the
    # cons-call case handled, a bare symbol was always "reported
    # unexpanded" and returned as-is, so it stayed EQ to itself
    # (`macrolet.14`).
    if isinstance(form_to_expand, lisptype.LispSymbol):
        expansion = expand_env.get_symbol_macro(form_to_expand)
        if expansion is not None:
            return lisptype.MultipleValues(expansion, lisptype.T)
        return _unexpanded(form_to_expand)

    # Only cons cells can be macro calls
    if not _consp_internal(form_to_expand):
        return _unexpanded(form_to_expand)

    operator = car(form_to_expand)
    if not isinstance(operator, lisptype.LispSymbol):
        return _unexpanded(form_to_expand)

    # Try to find the operator function
    try:
        macro_func = expand_env.find_func(operator)
    except Exception:
        macro_func = None
        logger.error(f"[DEBUG] Error looking up macro function for {operator}", exc_info=True)

    if not macro_func or not callable(macro_func):
        return _unexpanded(form_to_expand)

    # Check if it's actually a macro
    if not getattr(macro_func, '__is_macro__', False):
        return _unexpanded(form_to_expand)

    # Call the macro with unevaluated arguments
    args_list = []
    current = cdr(form_to_expand)
    while _consp_internal(current):
        args_list.append(car(current))
        current = cdr(current)
    
    expects_whole = getattr(macro_func, '__expects_whole__', False)
    expects_env = getattr(macro_func, '__expects_environment__', False)

    # Build call arguments based on macro function expectations
    call_args = []
    if expects_whole:
        call_args.append(form_to_expand)
    call_args.extend(args_list)

    # If macro expects expansion-time environment, append it as trailing arg
    if expects_env:
        call_args.append(expand_env)

    # An error raised by the expander is the *program's* error and propagates.
    # There used to be a blanket `except Exception: return form_to_expand`
    # here, which reported a broken macro as "this was not a macro call" --
    # silently swallowing the failure and handing back an unexpanded form that
    # the caller then treated as final.
    #
    # The expander's own result is reduced to its primary value before being
    # wrapped as MACROEXPAND-1's expansion -- an expansion is one form, the
    # same rule `eval`'s own macro dispatch applies. Without it, a macro whose
    # body ends in a multiple-valued call (e.g. this project's own aux macro
    # `EXPAND-IN-CURRENT-ENV`, `(macroexpand form env)`) left the raw
    # MultipleValues object nested inside the outer wrapper instead of the
    # form it carries.
    return lisptype.MultipleValues(
        lisptype.primary_value(macro_func(*call_args)), lisptype.T)


def eval_macro_function(form, env):
    """Evaluate MACRO-FUNCTION special form.
    
    (MACRO-FUNCTION symbol [environment]) - return the macro function for a symbol, or NIL if not a macro.

    The lambda list is `(symbol &optional environment)` (CLHS 25.1.3's
    macro-function: "macro-name [environment]"): a second argument is the
    environment designator (NIL meaning the global environment), and a
    third is a PROGRAM-ERROR (macro-function.error.2). The two-argument
    call is what macro-function.13 exercises with each argument form
    evaluated -- and both argument forms *are* evaluated.
    """
    from .evaluation_core import eval, ConditionException
    from .binding import root_environment

    args = cdr(form)
    arg_count = 0
    cur = args
    while _consp_internal(cur):
        arg_count += 1
        cur = cdr(cur)

    if arg_count == 0 or arg_count > 2:
        cond = lisptype.ProgramError(
            message="MACRO-FUNCTION requires 1 or 2 arguments")
        raise ConditionException(cond, recoverable=False)
    
    symbol_form = car(args)

    # MACRO-FUNCTION is an ordinary function (CLHS 8.1), not a special
    # operator -- its argument is evaluated like any function call's. A bare
    # symbol in the form position is a *variable reference* and must be
    # evaluated to the symbol it holds, not treated as that symbol literally;
    # treating it literally is what made `(let ((s 'foo)) (macro-function s))`
    # look up the symbol S instead of FOO while `(macro-function 'foo)`
    # happened to work only because `(QUOTE FOO)` isn't itself a LispSymbol.
    # Evaluate symbol first (left-to-right evaluation order per CLHS 3.4.1)
    symbol = eval(symbol_form, env)

    lookup_env = env
    if arg_count == 2:
        # Then evaluate environment argument
        env_value = eval(car(cdr(args)), env)
        if env_value is None or env_value is lisptype.NIL:
            # NIL designates the global environment, not "unspecified"
            lookup_env = root_environment(env)
        elif isinstance(env_value, lisptype.Environment):
            lookup_env = env_value

    if not isinstance(symbol, lisptype.LispSymbol):
        return lisptype.NIL

    # Try to find the function in the environment first
    func = lookup_env.find_func(symbol)

    # If environment binding is a macro callable, return it immediately
    if callable(func) and getattr(func, '__is_macro__', False):
        return func

    # Otherwise fall back to the function registry
    entry = None
    try:
        from . import registry as _registry
        entry = _registry.function_registry.get(symbol.name)
    except Exception:
        entry = None

    if entry:
        # If the registry entry stores the actual Python callable, prefer it
        if getattr(entry, 'func', None) is not None:
            candidate = entry.func
            if callable(candidate) and getattr(candidate, '__is_macro__', False):
                return candidate
        # Otherwise try to resolve by py_name (modules may register py_name)
        py_name = entry.get('py_name') if hasattr(entry, 'get') else getattr(entry, 'py_name', None)
        if py_name:
            try:
                import importlib
                import fclpy.lispfunc as lispfunc_mod
                candidate = getattr(lispfunc_mod, py_name, None)
                if candidate is None:
                    # Try loading known submodules to find implementation
                    for sub in ('core', 'math', 'sequences', 'vectors', 'streams', 'pathnames', 'hashtables', 'evaluation', 'comparison', 'characters', 'io', 'io_read', 'io_write', 'utilities', 'classes', 'misc_macros'):
                        try:
                            mod = importlib.import_module(f'fclpy.lispfunc.{sub}')
                            candidate = getattr(mod, py_name, None)
                            if candidate:
                                try:
                                    setattr(lispfunc_mod, py_name, candidate)
                                except Exception:
                                    pass
                                break
                        except Exception:
                            continue
                if callable(candidate) and getattr(candidate, '__is_macro__', False):
                    return candidate
            except Exception:
                pass

    # As a last resort, look for a pure expansion function named
    # `_<lowercase-symbol>_expander` (returns a Lisp form, not a value).
    # We must NOT wrap the `eval_<name>` handler because those handlers
    # evaluate the expansion and return a value; the macroexpander would
    # then evaluate the value a second time causing double-evaluation.
    try:
        import fclpy.lispfunc.evaluation_special_forms as _self_mod
        expander_name = f"_{symbol.name.lower()}_expander"
        expander_fn = getattr(_self_mod, expander_name, None)
        if callable(expander_fn) and getattr(expander_fn, '__is_macro__', False):
            return expander_fn
    except Exception:
        pass

    return lisptype.NIL


def eval_destructuring_bind(form, env):
    """Evaluate DESTRUCTURING-BIND special form.

    Syntax: (DESTRUCTURING-BIND lambda-list expression &body body)
    This binds variables according to the lambda-list by destructuring the
    evaluated expression, then evaluates the body forms with those bindings.
    """
    from .evaluation_core import eval, bind_destructuring_pattern

    args = cdr(form)
    if not _consp_internal(args) or not _consp_internal(cdr(args)):
        raise lisptype.LispNotImplementedError("DESTRUCTURING-BIND requires a pattern, an expression and body")

    pattern = car(args)
    expr_form = car(cdr(args))
    body = cdr(cdr(args))

    # Validate that the pattern doesn't use NIL, T, or keywords as variable names
    _check_destructuring_pattern_vars(pattern)

    # Evaluate the expression to destructure
    expr_val = eval(expr_form, env)

    # CLHS 3.5.1.2: the pattern's required parameters consume one element
    # each, so a value with too few is a PROGRAM-ERROR -- exactly as for a
    # function call -- instead of the silent NIL bindings the binder below
    # would give every missing element. Checked after the expression is
    # evaluated, so an error in the value form still wins.
    _check_destructuring_arity(pattern, expr_val, 'DESTRUCTURING-BIND')

    # Create a new environment for the bindings
    bind_env = lisptype.Environment(parent=env)

    # DESTRUCTURING-BIND's pattern is the same destructuring-lambda-list
    # grammar DEFMACRO/MACROLET parameters use (CLHS 3.4.4), so it shares
    # the one recursive binder with them instead of a second, partial copy.
    bind_destructuring_pattern(pattern, expr_val, bind_env)

    # Evaluate body forms in bind_env
    result = lisptype.NIL
    cur = body
    while _consp_internal(cur):
        result = eval(car(cur), bind_env)
        cur = cdr(cur)

    return result


def _check_destructuring_pattern_vars(pattern):
    """Validate that a destructuring pattern doesn't use invalid variable names.

    NIL, T, and keywords cannot be used as variable names (CLHS 3.4.4).
    Raises PROGRAM-ERROR if any are found.
    """
    if isinstance(pattern, lisptype.lispKeyword):
        raise lisptype.LispProgramError(
            "Keywords cannot be used as variable names in a destructuring pattern")

    # NIL, in any form (None, lispNull, or LispSymbol named NIL)
    if pattern is None or pattern is lisptype.NIL or isinstance(pattern, lisptype.lispNull):
        raise lisptype.LispProgramError(
            "NIL cannot be used as a variable name in a destructuring pattern")

    if isinstance(pattern, lisptype.LispSymbol):
        name_upper = pattern.name.upper()
        if name_upper == 'T':
            raise lisptype.LispProgramError(
                "T cannot be used as a variable name in a destructuring pattern")
        elif name_upper == 'NIL':
            raise lisptype.LispProgramError(
                "NIL cannot be used as a variable name in a destructuring pattern")
        return

    # Recursively check list patterns
    if _consp_internal(pattern):
        cur = pattern
        while _consp_internal(cur):
            elem = car(cur)

            # Skip lambda-list keywords (&REST, &KEY, etc.)
            if isinstance(elem, lisptype.LispSymbol) and elem.name.startswith('&'):
                cur = cdr(cur)
                continue

            # Recursively check the element
            _check_destructuring_pattern_vars(elem)

            cur = cdr(cur)


def eval_lambda(form, env):
    """Evaluate a LAMBDA expression to the function it denotes.

    CLHS 3.1.2.1.2.4: a lambda expression's parameters are an *ordinary*
    lambda list, the same one DEFUN/FLET/LABELS take, so it is built by the
    same `make_ordinary_function`. LAMBDA establishes no implicit block
    (CLHS 3.1.2.1.2.4 -- only the defining forms do), hence `block_name=None`.
    """
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("LAMBDA requires at least 1 argument")

    return make_ordinary_function(car(args), cdr(args), env)



def eval_declare(form, env):
    """Evaluate DECLARE special form.
    
    DECLARE is used inside function/macro/block bodies to provide declarations.
    This function stores declarations on the containing symbol's property list.
    
    Format: (DECLARE (declare-spec1) (declare-spec2) ...)
    Common declare-specs: (OPTIMIZE ...) (SPECIAL var ...) (TYPE type var ...) (IGNORE var ...)
    """
    args = cdr(form)
    
    # Collect all declare-specs
    result = None
    while _consp_internal(args):
        spec = car(args)
        # Each spec is a list like (OPTIMIZE ...) or (SPECIAL x y z)
        if _consp_internal(spec):
            spec_type = car(spec)
            if isinstance(spec_type, lisptype.LispSymbol):
                spec_name = spec_type.name.upper()
                
                # Store declaration in a reserved property list key
                if not hasattr(env, '_declarations'):
                    env._declarations = {}
                if spec_name not in env._declarations:
                    env._declarations[spec_name] = []
                env._declarations[spec_name].append(spec)
        
        args = cdr(args)
    
    # DECLARE returns NIL
    return lisptype.NIL


def eval_declaim(form, env):
    """Evaluate DECLAIM special form (global declarations).
    
    DECLAIM provides global declarations that affect the entire program.
    Stores declarations globally in the environment.
    
    Format: (DECLAIM (declare-spec1) (declare-spec2) ...)
    """
    args = cdr(form)
    
    # Get the global environment (root environment)
    global_env = env
    while global_env.parent is not None:
        global_env = global_env.parent
    
    # Collect all declare-specs globally
    result = None
    while _consp_internal(args):
        spec = car(args)
        # Each spec is a list like (OPTIMIZE ...) or (SPECIAL x y z)
        if _consp_internal(spec):
            spec_type = car(spec)
            if isinstance(spec_type, lisptype.LispSymbol):
                spec_name = spec_type.name.upper()
                
                # Handle different declaration types
                if spec_name == 'OPTIMIZE':
                    # Store optimization settings globally
                    _store_optimization_declaration(global_env, spec)
                elif spec_name == 'SPECIAL':
                    # Store special variable declarations globally
                    _store_special_declaration(global_env, spec)
                else:
                    # Store other declarations
                    if not hasattr(global_env, '_global_declarations'):
                        global_env._global_declarations = {}
                    if spec_name not in global_env._global_declarations:
                        global_env._global_declarations[spec_name] = []
                    global_env._global_declarations[spec_name].append(spec)
        
        args = cdr(args)
    
    # DECLAIM returns NIL
    return lisptype.NIL


def eval_proclaim(form, env):
    """Evaluate PROCLAIM special form.

    Unlike DECLAIM (a macro over an implicit body of several decl-specs),
    PROCLAIM's lambda list is `(decl-spec)` -- CLHS 3.8: exactly one
    argument, evaluated (so a backquoted form works), naming exactly one
    declaration. The loop this replaced treated PROCLAIM like DECLAIM,
    consuming every argument as its own spec, so a second argument was
    silently accepted instead of signalling PROGRAM-ERROR
    (`environment/proclaim.lsp`'s `proclaim.error.2`:
    `(proclaim '(optimize) nil)`).
    """
    from .evaluation_core import eval as lisp_eval
    from .sequence_protocol import list_elements

    args = cdr(form)

    if not _consp_internal(args) or _consp_internal(cdr(args)):
        raise lisptype.LispProgramError("PROCLAIM requires exactly one declaration specifier")

    spec_expr = car(args)
    # Evaluate the spec expression so backquote/unquote is handled.
    spec = lisp_eval(spec_expr, env)

    # Get the global/root environment to store effects
    global_env = env
    while global_env.parent is not None:
        global_env = global_env.parent

    if _consp_internal(spec):
        # A decl-spec is a proper list (CLHS 3.8's `declaration ::= (decl-
        # identifier decl-data*)`); every one of `proclaim.error.3` through
        # `.11` is a *dotted* spec -- `(optimize . foo)`, `(type integer .
        # foo)`, `(ftype (function (t) t) . foo)` -- which this rejects the
        # same way any other CLHS 14.2 list-argument operator does, through
        # the one shared dotted-list check rather than a second copy of it.
        elements = list_elements(spec, what='PROCLAIM declaration specifier', dotted='error')
        spec_type = elements[0] if elements else None
        if isinstance(spec_type, lisptype.LispSymbol):
            spec_name = spec_type.name.upper()
            if spec_name == 'OPTIMIZE':
                _store_optimization_declaration(global_env, spec)
            elif spec_name == 'SPECIAL':
                _store_special_declaration(global_env, spec)
            else:
                if not hasattr(global_env, '_global_declarations'):
                    global_env._global_declarations = {}
                if spec_name not in global_env._global_declarations:
                    global_env._global_declarations[spec_name] = []
                global_env._global_declarations[spec_name].append(spec)

    return lisptype.NIL


def _store_optimization_declaration(env, spec):
    """Helper to store OPTIMIZE declaration on environment."""
    # OPTIMIZE spec format: (OPTIMIZE (quality level) ...)
    # Qualities: SPEED, SAFETY, DEBUG, COMPILATION-SPEED, SPACE
    # Levels: 0 (minimum) to 3 (maximum)
    
    if not hasattr(env, '_optimization_policy'):
        env._optimization_policy = {
            'speed': 1,
            'safety': 1,
            'debug': 1,
            'compilation-speed': 1,
            'space': 1
        }
    
    # Parse (OPTIMIZE (quality level) ...)
    specs = cdr(spec)  # Skip 'OPTIMIZE' keyword
    while _consp_internal(specs):
        item = car(specs)
        if _consp_internal(item):
            quality = car(item)
            level = car(cdr(item))
            
            if isinstance(quality, lisptype.LispSymbol) and isinstance(level, int):
                q_name = quality.name.lower().replace('-', '_')
                env._optimization_policy[q_name] = max(0, min(3, level))
        
        specs = cdr(specs)


def _store_special_declaration(env, spec):
    """Record a global ``(SPECIAL var ...)`` proclamation from DECLAIM/PROCLAIM."""
    # SPECIAL spec format: (SPECIAL var1 var2 ...)
    vars_to_declare = cdr(spec)  # Skip 'SPECIAL' keyword

    while _consp_internal(vars_to_declare):
        var = car(vars_to_declare)
        if isinstance(var, lisptype.LispSymbol):
            proclaim_special(var, env)
        vars_to_declare = cdr(vars_to_declare)


def eval_defvar(form, env):
    """Evaluate DEFVAR special form.

    (DEFVAR name)           - proclaims special, leaves the value alone
    (DEFVAR name value)     - proclaims special and initializes if unbound
    (DEFVAR name value doc) - with documentation string

    CLHS 3.1.2.1.1 / the DEFVAR page: the proclamation is unconditional, and
    the *value* is assigned only when an initial-value form is supplied and
    the variable is not already bound. With no initial-value form DEFVAR
    leaves the value cell undisturbed, so ``(defvar *x*)`` proclaims `*x*`
    special and leaves ``(boundp '*x*)`` NIL -- it does not bind it to NIL.

    The variable is global, so its home is the symbol's value cell, which is
    what the global environment's `add_variable` writes (CLHS 3.1.1.1; see
    `Environment`). It used to write a global *lexical* binding instead, which
    `SYMBOL-VALUE`/`BOUNDP`/`PROGV` could not see and which shadowed every
    dynamic binding a later LET established.

    Returns the symbol name.
    """
    from .evaluation_core import eval as lisp_eval

    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("DEFVAR requires at least a variable name")

    name = car(args)
    if not isinstance(name, lisptype.LispSymbol):
        raise lisptype.LispNotImplementedError("DEFVAR: first argument must be a symbol")

    # The proclamation is what makes every later binding of this name dynamic,
    # so it must happen before the initial-value form is evaluated -- that form
    # may itself bind the variable.
    global_env = root_environment(env)
    proclaim_special(name, global_env)

    # "Already bound" is a question about the value cell, not about whatever
    # lexical binding happens to surround this DEFVAR.
    rest_args = cdr(args)
    has_value_form = _consp_internal(rest_args)

    if has_value_form and not global_env.has_variable(name):
        value_form = car(rest_args)
        global_env.add_variable(name, lisp_eval(value_form, env))

    # Handle documentation string if present (third argument). CLHS's DEFVAR
    # grammar is `(defvar name [value [doc]])` -- the doc can follow a value
    # form or, in the `(defvar name)` shape, be the *first* thing after the
    # name. The old `has_value_form` gate dropped the latter entirely
    # (documentation.symbol.variable.1 uses exactly that shape).
    doc_args = cdr(rest_args) if has_value_form else rest_args
    if _consp_internal(doc_args):
        docstring = car(doc_args)
        if isinstance(docstring, (str, lisptype.LispString)):
            # Store documentation on symbol's property list
            if not hasattr(name, 'plist'):
                name.plist = {}
            name.plist['DOCUMENTATION'] = docstring
            name.plist['VARIABLE-DOCUMENTATION'] = docstring

    return name


def eval_defparameter(form, env):
    """Evaluate DEFPARAMETER special form.
    
    (DEFPARAMETER name value)     - proclaims special and always sets value
    (DEFPARAMETER name value doc) - with documentation string

    Unlike DEFVAR, DEFPARAMETER always sets the value, even if already bound.
    Like DEFVAR, the variable is global, so the value goes in the symbol's
    value cell -- see `eval_defvar`.

    Returns the symbol name.
    """
    from .evaluation_core import eval as lisp_eval

    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("DEFPARAMETER requires a variable name")

    name = car(args)
    if not isinstance(name, lisptype.LispSymbol):
        raise lisptype.LispNotImplementedError("DEFPARAMETER: first argument must be a symbol")

    global_env = root_environment(env)
    proclaim_special(name, global_env)

    # Get the value form (required for DEFPARAMETER)
    rest_args = cdr(args)
    if not _consp_internal(rest_args):
        raise lisptype.LispNotImplementedError("DEFPARAMETER requires an initial value")
    
    value_form = car(rest_args)
    value = lisp_eval(value_form, env)
    global_env.add_variable(name, value)
    
    # Handle documentation string if present (third argument)
    doc_args = cdr(rest_args)
    if _consp_internal(doc_args):
        docstring = car(doc_args)
        if isinstance(docstring, (str, lisptype.LispString)):
            # Store documentation on symbol's property list
            if not hasattr(name, 'plist'):
                name.plist = {}
            name.plist['DOCUMENTATION'] = docstring
            name.plist['VARIABLE-DOCUMENTATION'] = docstring

    return name


def eval_defconstant(form, env):
    """Evaluate DEFCONSTANT special form.
    
    (DEFCONSTANT name value)     - declares and always sets constant value
    (DEFCONSTANT name value doc) - with documentation string
    
    DEFCONSTANT always sets the value and marks it as a constant.
    Constants cannot be rebound (though we don't enforce this strictly).
    
    DEFCONSTANT defines variables in the GLOBAL environment.
    
    Returns the symbol name.
    """
    from .evaluation_core import eval as lisp_eval
    
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("DEFCONSTANT requires a variable name")
    
    name = car(args)
    if not isinstance(name, lisptype.LispSymbol):
        raise lisptype.LispNotImplementedError("DEFCONSTANT: first argument must be a symbol")
    
    # Find the global/root environment for defining the variable
    global_env = env
    while global_env.parent is not None:
        global_env = global_env.parent
    
    # Get the value form (required for DEFCONSTANT)
    rest_args = cdr(args)
    if not _consp_internal(rest_args):
        raise lisptype.LispNotImplementedError("DEFCONSTANT requires an initial value")
    
    value_form = car(rest_args)
    value = lisp_eval(value_form, env)
    global_env.add_variable(name, value)
    
    # Handle documentation string if present (third argument)
    doc_args = cdr(rest_args)
    if _consp_internal(doc_args):
        docstring = car(doc_args)
        if isinstance(docstring, (str, lisptype.LispString)):
            # Store documentation on symbol's property list
            if not hasattr(name, 'plist'):
                name.plist = {}
            name.plist['DOCUMENTATION'] = docstring
            name.plist['VARIABLE-DOCUMENTATION'] = docstring
    
    # Record that the name is constant, through the one table CONSTANTP reads.
    # This used to write a private `global_env._constants` dict that nothing
    # ever consulted, so `(constantp 'a-defconstant-name)` was NIL.
    from .binding import proclaim_constant
    proclaim_constant(name, global_env)

    return name


def eval_deftype(form, env):
    """Evaluate DEFTYPE (CLHS 4.2.3, 25.1.3): (DEFTYPE name lambda-list
    &body body). Arguments are not evaluated -- the name is a symbol and
    lambda-list/body are literal specification syntax, like DEFMACRO.

    Stores a real *expander*, not the raw source. This dict used to hold
    `lambda_list`/`body`/`env` and nothing anywhere read it, so a
    DEFTYPE'd name was invisible to both TYPEP and SUBTYPEP --
    `(deftype foo () '(integer 0 10))` succeeded and then
    `(typep 5 'foo)` was NIL. The expander is built by the one
    macro-lambda-list binder (CLHS 4.2.3: a deftype lambda list is a
    macro lambda list, except that an omitted &OPTIONAL/&KEY parameter
    defaults to `*` rather than NIL, which is what `unsupplied_default`
    supplies). Reusing it is also what gives DEFTYPE &WHOLE, &REST,
    &KEY, destructuring, the docstring and the implicit BLOCK that
    `(return-from <type-name> ...)` needs -- all of which ansi-test's
    deftype.9-.19 exercise.
    """
    args = cdr(form)
    if args is None or args == lisptype.NIL:
        raise lisptype.LispError("DEFTYPE requires a name")

    name = car(args)
    if not isinstance(name, lisptype.LispSymbol):
        raise lisptype.LispError("DEFTYPE: name must be a symbol")

    rest = cdr(args)
    lambda_list = car(rest) if _consp_internal(rest) else lisptype.NIL
    body = cdr(rest) if _consp_internal(rest) else lisptype.NIL

    global_env = env
    while global_env.parent is not None:
        global_env = global_env.parent

    if not hasattr(global_env, 'user_types'):
        global_env.user_types = {}

    wild = lisptype.COMMON_LISP_PACKAGE.intern('*')
    expander = _create_macro_function(
        name, lambda_list, body, env, unsupplied_default=wild)

    # CLHS 25.1.3: a DEFTYPE body may open with a documentation string;
    # `(documentation name 'type)` reads it back.
    _type_doc, _type_decls, _type_forms = split_function_body(body)

    global_env.user_types[name.name] = {
        'name': name,
        'lambda_list': lambda_list,
        'body': body,
        'env': env,          # Capture lexical environment
        'expander': expander,
        'documentation': str(_type_doc) if _type_doc else None,
    }

    return name


def eval_defsetf(form, env):
    """Evaluate DEFSETF (CLHS 5.1.2.3): defines how to SETF `access-fn`.

    Short form:  (DEFSETF access-fn update-fn [documentation])
    Long form:   (DEFSETF access-fn lambda-list (store-var...)
                          [decl] [doc] form...)

    Arguments are not evaluated -- they are symbol names and code
    templates, stored in the global `setf_expanders` table SETF's own
    expansion (`get_setf_expansion`) reads. Returns access-fn, as CLHS
    specifies.
    """
    args = cdr(form)
    if args is None or args == lisptype.NIL:
        raise lisptype.LispError("DEFSETF requires arguments")

    access_fn = car(args)
    rest = cdr(args)

    if not isinstance(access_fn, lisptype.LispSymbol):
        raise lisptype.LispError("DEFSETF: access-fn must be a symbol")

    if rest is None or rest == lisptype.NIL:
        raise lisptype.LispError("DEFSETF requires at least two arguments")

    second_arg = car(rest)
    third_and_beyond = cdr(rest)

    is_short_form = isinstance(second_arg, lisptype.LispSymbol)

    global_env = env
    while global_env.parent is not None:
        global_env = global_env.parent

    if not hasattr(global_env, 'setf_expanders'):
        global_env.setf_expanders = {}

    if is_short_form:
        update_fn = second_arg
        doc_string = None
        if _consp_internal(third_and_beyond):
            doc_form = car(third_and_beyond)
            if isinstance(doc_form, str):
                doc_string = doc_form

        global_env.setf_expanders[access_fn.name] = {
            'type': 'short',
            'update_fn': update_fn,
            'documentation': doc_string
        }
    else:
        lambda_list = second_arg

        if not _consp_internal(third_and_beyond):
            raise lisptype.LispError("DEFSETF long form requires store variables")

        store_vars = car(third_and_beyond)
        body = cdr(third_and_beyond)

        declarations = []
        doc_string = None
        actual_body = body

        while _consp_internal(actual_body):
            form_item = car(actual_body)
            if _consp_internal(form_item):
                op = car(form_item)
                if isinstance(op, lisptype.LispSymbol) and op.name == 'DECLARE':
                    declarations.append(form_item)
                    actual_body = cdr(actual_body)
                    continue
            if isinstance(form_item, str) and doc_string is None:
                doc_string = form_item
                actual_body = cdr(actual_body)
                continue
            break

        global_env.setf_expanders[access_fn.name] = {
            'type': 'long',
            'lambda_list': lambda_list,
            'store_vars': store_vars,
            'declarations': declarations,
            'documentation': doc_string,
            'body': actual_body,
            'env': env  # Capture lexical environment
        }

    return access_fn


def eval_define_setf_expander(form, env):
    """Evaluate DEFINE-SETF-EXPANDER (CLHS 5.1.2.4):
    (DEFINE-SETF-EXPANDER access-fn lambda-list
                          [[declaration* | documentation]] form*)

    Arguments are not evaluated. Stores the expander in the same global
    `setf_expanders` table DEFSETF's long form uses, tagged 'expander'
    since its lambda-list can take &ENVIRONMENT (the macro environment at
    the SETF call site) where DEFSETF's cannot. Returns access-fn.
    """
    args = cdr(form)
    if args is None or args == lisptype.NIL:
        raise lisptype.LispError("DEFINE-SETF-EXPANDER requires arguments")

    access_fn = car(args)
    rest = cdr(args)

    if not isinstance(access_fn, lisptype.LispSymbol):
        raise lisptype.LispError("DEFINE-SETF-EXPANDER: access-fn must be a symbol")

    if rest is None or rest == lisptype.NIL:
        raise lisptype.LispError("DEFINE-SETF-EXPANDER requires a lambda-list")

    lambda_list = car(rest)
    body = cdr(rest)

    global_env = env
    while global_env.parent is not None:
        global_env = global_env.parent

    if not hasattr(global_env, 'setf_expanders'):
        global_env.setf_expanders = {}

    declarations = []
    doc_string = None
    actual_body = body

    while _consp_internal(actual_body):
        form_item = car(actual_body)
        if _consp_internal(form_item):
            op = car(form_item)
            if isinstance(op, lisptype.LispSymbol) and op.name == 'DECLARE':
                declarations.append(form_item)
                actual_body = cdr(actual_body)
                continue
        if isinstance(form_item, str) and doc_string is None:
            doc_string = form_item
            actual_body = cdr(actual_body)
            continue
        break

    global_env.setf_expanders[access_fn.name] = {
        'type': 'expander',
        'lambda_list': lambda_list,
        'declarations': declarations,
        'documentation': doc_string,
        'body': actual_body,
        'env': env  # Capture lexical environment
    }

    return access_fn


def _defstruct_type_representation(type_option_form):
    """Resolve a DEFSTRUCT `:TYPE` option's value to CLHS 19.4.7's two
    representations: ``('list', None)`` or ``('vector', element-type-form)``,
    the latter defaulting to `T` for a bare `VECTOR` (no element-type form of
    its own to upgrade)."""
    def _head_name(x):
        if isinstance(x, (lisptype.LispSymbol, lisptype.lispKeyword)):
            return x.name.upper()
        return None

    if _consp_internal(type_option_form):
        head_name = _head_name(car(type_option_form))
        if head_name == 'VECTOR':
            rest = cdr(type_option_form)
            elt_form = car(rest) if _consp_internal(rest) else lisptype.T
            return 'vector', elt_form
        raise lisptype.LispNotImplementedError(
            f"DEFSTRUCT :TYPE {head_name}: not supported")

    head_name = _head_name(type_option_form)
    if head_name == 'LIST':
        return 'list', None
    if head_name == 'VECTOR':
        return 'vector', lisptype.T
    raise lisptype.LispNotImplementedError(
        f"DEFSTRUCT :TYPE {type_option_form}: not supported")


def _signal_defstruct_simple_error(message):
    """Signal a real SIMPLE-ERROR for an invalid `:TYPE` DEFSTRUCT option
    combination (CLHS 19.4.7's DEFSTRUCT.ERROR.3/.4), through the same
    build/signal path ERROR itself uses -- not a bare Python exception, which
    would match no HANDLER-CASE clause and surface as the value of the form."""
    from .evaluation_conditions import build_condition, signal_error_object
    condition = build_condition(message, [], lisptype.SimpleError)
    signal_error_object(condition)


def _eval_typed_defstruct(struct_name, struct_class_name, type_option_form, named_option,
                          initial_offset, include_parent_name, include_overrides, slot_specs,
                          conc_name, predicate_name, predicate_was_explicit, copier_name,
                          constructors, current_pkg, global_env, env, parse_slot_spec,
                          eval_initform):
    """DEFSTRUCT's `(:type list)`/`(:type vector)` representation (CLHS
    19.4.7): the structure *is* a plain list or vector, flat, with no class
    and no `LispInstance` -- so this is a different construction entirely
    from the CLOS-backed path in `eval_defstruct`, not a variant of it.
    `:INCLUDE` therefore composes flat layouts (`state.typed_struct_layouts`)
    rather than class hierarchies: a slot's *position* is what a subtype
    inherits, not a slot descriptor in an object model that doesn't exist
    here.
    """
    import fclpy.classes as classes

    representation, element_type_form = _defstruct_type_representation(type_option_form)

    if named_option and representation == 'vector':
        elt_name = (element_type_form.name.upper()
                   if isinstance(element_type_form, (lisptype.LispSymbol, lisptype.lispKeyword))
                   else None)
        if elt_name != 'T':
            _signal_defstruct_simple_error(
                f"DEFSTRUCT {struct_class_name}: a :NAMED (VECTOR {element_type_form}) "
                "structure has nowhere to hold its own type name, which is a SYMBOL")

    if not named_option:
        # CLHS 19.4.7: an unnamed :TYPE structure carries no type marker, so
        # nothing distinguishes an instance of it from any other list/vector
        # of the same shape -- an explicitly requested predicate name is
        # therefore an error (DEFSTRUCT.ERROR.3), and an omitted one is simply
        # absent rather than defaulting to NAME-P.
        if predicate_was_explicit and predicate_name is not None:
            _signal_defstruct_simple_error(
                f"DEFSTRUCT {struct_class_name}: :PREDICATE requires :NAMED "
                "when :TYPE is specified")
        predicate_name = None

    parent_layout = []
    if include_parent_name is not None:
        parent_entry = state.typed_struct_layouts.get(include_parent_name.upper())
        if parent_entry is None:
            raise lisptype.LispError(
                f"DEFSTRUCT: :INCLUDE parent {include_parent_name} is not a "
                ":TYPE structure")
        parent_layout = [dict(entry) for entry in parent_entry['layout']]
        if include_overrides:
            by_name = {entry['name']: entry for entry in parent_layout if entry['kind'] == 'slot'}
            for override_spec in include_overrides:
                name_str, default_form, type_spec, read_only = parse_slot_spec(override_spec)
                if name_str not in by_name:
                    raise lisptype.LispError(
                        f"DEFSTRUCT :INCLUDE: {name_str} does not name a slot of "
                        f"{include_parent_name}")
                by_name[name_str]['slot_def'] = classes.SlotDefinition(
                    name=lisptype.LispSymbol(name_str), initform=default_form,
                    type_spec=type_spec, read_only=read_only, definition_env=env)

    own_prefix = [{'kind': 'pad'} for _ in range(initial_offset)]
    if named_option:
        own_prefix.append({'kind': 'name', 'value': struct_name})

    own_slots = []
    cur = slot_specs
    while _consp_internal(cur):
        name_str, default_form, type_spec, read_only = parse_slot_spec(car(cur))
        own_slots.append({'kind': 'slot', 'name': name_str,
                          'slot_def': classes.SlotDefinition(
                              name=lisptype.LispSymbol(name_str), initform=default_form,
                              type_spec=type_spec, read_only=read_only, definition_env=env)})
        cur = cdr(cur)

    full_layout = parent_layout + own_prefix + own_slots
    total_length = len(full_layout)

    state.typed_struct_layouts[struct_class_name.upper()] = {
        'representation': representation,
        'element_type_form': element_type_form,
        'layout': full_layout,
    }

    ordered_slots = [(entry['name'], entry['slot_def']) for entry in full_layout
                     if entry['kind'] == 'slot']

    def build_container(slot_values):
        values = []
        for entry in full_layout:
            if entry['kind'] == 'pad':
                values.append(lisptype.NIL)
            elif entry['kind'] == 'name':
                values.append(entry['value'])
            else:
                values.append(slot_values.get(entry['name'], lisptype.NIL))
        if representation == 'list':
            from .sequence_protocol import make_lisp_list
            return make_lisp_list(values)
        return _arrays.make_array((total_length,), element_type=element_type_form,
                                  initial_contents=values)

    def default_constructor_lambda_list():
        tail = lisptype.NIL
        for name_str, _slot_def in reversed(ordered_slots):
            tail = lisptype.lispCons(lisptype.LispSymbol(name_str), tail)
        return lisptype.lispCons(lisptype.LispSymbol('&KEY'), tail)

    def make_typed_constructor(boa_ll, ctor_name):
        from .evaluation_core import parse_lambda_list, eval as eval_fn

        parsed = parse_lambda_list(boa_ll)
        required_params = parsed['required']
        param_vars = _lambda_list_variables(parsed)
        param_names = {var.name for var in param_vars}
        slot_initforms = {name_str: slot_def.initform for name_str, slot_def in ordered_slots}

        def default_fallback(var):
            if isinstance(var, lisptype.LispSymbol):
                return slot_initforms.get(var.name)
            return None

        def constructor(*call_args):
            call_args = tuple(_canonicalize_nil_symbol(a) for a in call_args)
            _check_ordinary_arity(parsed, call_args, ctor_name)

            func_env = lisptype.Environment(env)
            frame = BindingFrame(func_env, body=lisptype.NIL, bound_vars=param_vars)
            try:
                for index, param in enumerate(required_params):
                    frame.bind(param, call_args[index])
                _bind_ordinary_lambda_list_tail(
                    parsed, call_args, len(required_params), func_env, eval_fn, frame,
                    default_fallback=default_fallback)

                slot_values = {name_str: eval_initform(slot_def)
                               for name_str, slot_def in ordered_slots
                               if name_str not in param_names}
                for slot_name_str, _slot_def in ordered_slots:
                    for var in param_vars:
                        if var.name == slot_name_str:
                            slot_values[slot_name_str] = func_env.find_variable(var)
                            break
                return build_container(slot_values)
            finally:
                frame.unwind()

        constructor.__lisp_lambda_list__ = boa_ll
        return constructor

    for ctor_name, boa_ll in constructors:
        if boa_ll is not None:
            raise lisptype.LispNotImplementedError(
                "DEFSTRUCT: a BOA :CONSTRUCTOR lambda list is not supported "
                "together with :TYPE")
        ctor_sym = current_pkg.intern_symbol(ctor_name)
        global_env.add_function(
            ctor_sym, make_typed_constructor(default_constructor_lambda_list(), ctor_name))

    if predicate_name:
        name_position = next(i for i, entry in enumerate(full_layout)
                             if entry['kind'] == 'name' and entry['value'] is struct_name)

        def is_typed_structure(obj):
            try:
                if representation == 'list':
                    from .sequence_protocol import list_elements
                    elements = list_elements(obj, 'structure predicate', dotted='allow')
                    if name_position >= len(elements):
                        return lisptype.NIL
                    value = elements[name_position]
                else:
                    if (not _arrays.is_vector(obj)
                            or name_position >= _arrays.array_total_size_of(obj)):
                        return lisptype.NIL
                    value = _arrays.row_major_get(obj, name_position)
                return lisptype.T if value is struct_name else lisptype.NIL
            except lisptype.LispError:
                return lisptype.NIL

        predicate_sym = current_pkg.intern_symbol(predicate_name)
        global_env.add_function(predicate_sym, is_typed_structure)

    if copier_name:
        def copy_typed_structure(struct):
            if representation == 'list':
                from .sequence_protocol import list_elements, make_lisp_list
                return make_lisp_list(list_elements(struct, copier_name, dotted='error'))
            values = [_arrays.row_major_get(struct, i) for i in range(total_length)]
            return _arrays.make_array((total_length,), element_type=element_type_form,
                                      initial_contents=values)

        copier_sym = current_pkg.intern_symbol(copier_name)
        global_env.add_function(copier_sym, copy_typed_structure)

    for index, entry in enumerate(full_layout):
        if entry['kind'] != 'slot':
            continue
        slot_name_str = entry['name']
        slot_def = entry['slot_def']
        accessor_name = conc_name + slot_name_str

        def make_typed_getter(i, an):
            def getter(instance):
                if representation == 'list':
                    from .sequence_protocol import list_elements
                    return list_elements(instance, an, dotted='error')[i]
                return _arrays.row_major_get(instance, i)
            return getter

        accessor_sym = current_pkg.intern_symbol(accessor_name)
        global_env.add_function(accessor_sym, make_typed_getter(index, accessor_name))

        if not slot_def.read_only:
            def make_typed_setter(i):
                def setter(instance, value):
                    if representation == 'list':
                        cell = instance
                        for _ in range(i):
                            cell = cell.cdr
                        cell.car = value
                        return value
                    _arrays.row_major_set(instance, i, value)
                    return value
                return setter

            setter_sym = current_pkg.intern_symbol('SET-' + accessor_name)
            global_env.add_function(setter_sym, make_typed_setter(index))

    return struct_name


def eval_defstruct(form, env):
    """Evaluate DEFSTRUCT special form (CLHS 3.4.6, 7.2).

    (DEFSTRUCT name slot...)
    (DEFSTRUCT (name option...) slot...)

    DEFSTRUCT does not evaluate its arguments -- they are literal
    specifications. It defines a real `classes.LispClass` (metaclass
    STRUCTURE-CLASS, rooted at STRUCTURE-OBJECT or at the :INCLUDE parent's
    class) and its constructors build real `classes.LispInstance` objects,
    so TYPEP/SUBTYPEP/FIND-CLASS/COPY-STRUCTURE all see structures through
    the one class/instance model CLOS already uses -- rather than a second,
    unregistered Python class nothing else in the language could see at
    all. structures/structure-00.lsp's generated battery (tests 1, 13-17,
    20 per DEFSTRUCT) depends on exactly this: `(typep obj name)`,
    `(typep obj (find-class name))`, `(typep (find-class name)
    'structure-class)`, `(typep obj 'structure-object)`, disjointness with
    every other built-in type via SUBTYPEP, and a working `copy-structure`.

    BOA constructors (`:constructor name (boa-lambda-list)`) bind their
    lambda list the ordinary way (`make_boa_constructor`, below): a
    lambda-list variable whose name matches a slot initializes that slot,
    everything else (supplied-p variables, `&aux` locals used only for
    their side effect) is bound and discarded. Structures with only keyword
    constructors are unaffected.
    """
    import fclpy.classes as classes
    from .evaluation_core import eval as _eval
    from .misc_clos import _eval_initform

    current_pkg = getattr(state, 'current_package', None) or lisptype.COMMON_LISP_USER_PACKAGE

    global_env = env
    while global_env.parent is not None:
        global_env = global_env.parent

    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("DEFSTRUCT requires a name")

    name_and_options = car(args)
    rest_forms = cdr(args)

    def _sym_name(x):
        """A DEFSTRUCT option value's name, under CLHS 16.2's *string
        designator* rule (a string, or a symbol/character whose name/one-
        character string is used) -- not `str(x)`, which is a keyword's or a
        character's *printed representation* (`:FOO`, `#\\X`) rather than its
        name. `(:conc-name #\\X)` (STRUCT-TEST-13) needs the prefix "X", not
        "#\\X".
        """
        if isinstance(x, (lisptype.LispSymbol, lisptype.lispKeyword)):
            return x.name
        if isinstance(x, lisptype.Character):
            return x.char
        return str(x)

    def _is_nil_value(v):
        return (v is None or v is lisptype.NIL or
                (isinstance(v, lisptype.LispSymbol) and v.name == 'NIL'))

    doc_string = None
    if _consp_internal(rest_forms) and isinstance(car(rest_forms), (str, lisptype.LispString)):
        doc_string = str(car(rest_forms))
        rest_forms = cdr(rest_forms)
    slot_specs = rest_forms

    # Sentinels for :copier/:predicate, which -- unlike :constructor -- take
    # at most one name each: `_DEFAULT` means the option was given with no
    # name of its own (bare `:copier`, or `(:copier)`), which still affirms
    # the *default* name, distinct from never mentioning the option at all
    # (also the default) and from an explicit `(:copier nil)` (suppressed).
    _DEFAULT = object()
    _SUPPRESSED = object()

    conc_name_explicit = None
    constructors = []              # [(name-or-None, boa-lambda-list-or-None), ...]
    constructors_specified = False
    copier_name = _DEFAULT
    predicate_name = _DEFAULT
    include_parent_name = None
    include_overrides = []         # raw override slot-spec forms
    type_option_form = None        # raw (unevaluated) :TYPE value, or None
    named_option = False           # :NAMED given (bare or as a value-less clause)
    initial_offset = 0              # :INITIAL-OFFSET value

    if isinstance(name_and_options, (lisptype.LispSymbol, lisptype.lispKeyword)):
        struct_name = name_and_options
    elif _consp_internal(name_and_options):
        struct_name = car(name_and_options)
        if not isinstance(struct_name, (lisptype.LispSymbol, lisptype.lispKeyword)):
            raise lisptype.LispNotImplementedError(
                f"DEFSTRUCT: structure name must be a symbol, got {type(struct_name)}")

        options = cdr(name_and_options)
        while _consp_internal(options):
            opt = car(options)
            if _consp_internal(opt):
                opt_name = car(opt)
                opt_name_str = _sym_name(opt_name).upper()
                opt_rest = cdr(opt)
                opt_value = car(opt_rest) if _consp_internal(opt_rest) else None

                if opt_name_str == 'CONC-NAME':
                    conc_name_explicit = '' if _is_nil_value(opt_value) else _sym_name(opt_value)
                elif opt_name_str == 'CONSTRUCTOR':
                    constructors_specified = True
                    if opt_value is None:
                        # (:constructor) with no name -- affirms the default.
                        constructors.append((None, None))
                    elif not _is_nil_value(opt_value):
                        boa_ll = None
                        opt_rest2 = cdr(opt_rest)
                        if _consp_internal(opt_rest2):
                            boa_ll = car(opt_rest2)
                        constructors.append((_sym_name(opt_value), boa_ll))
                    # (:constructor nil) -- explicitly suppressed, add nothing.
                elif opt_name_str == 'COPIER':
                    if opt_value is None:
                        copier_name = _DEFAULT
                    elif _is_nil_value(opt_value):
                        copier_name = _SUPPRESSED
                    else:
                        copier_name = _sym_name(opt_value)
                elif opt_name_str == 'PREDICATE':
                    if opt_value is None:
                        predicate_name = _DEFAULT
                    elif _is_nil_value(opt_value):
                        predicate_name = _SUPPRESSED
                    else:
                        predicate_name = _sym_name(opt_value)
                elif opt_name_str == 'INCLUDE':
                    include_parent_name = _sym_name(opt_value)
                    ov = cdr(opt_rest)
                    while _consp_internal(ov):
                        include_overrides.append(car(ov))
                        ov = cdr(ov)
                elif opt_name_str == 'TYPE':
                    type_option_form = opt_value
                elif opt_name_str == 'DOCUMENTATION':
                    # CLHS 3.4.6 / 25.1.3: `(:documentation string)` is a
                    # DEFSTRUCT option; `(documentation name 'structure)` and
                    # `(documentation (find-class name) t)` read it back.
                    if opt_value is not None:
                        doc_string = str(opt_value)
                elif opt_name_str == 'NAMED':
                    named_option = True
                elif opt_name_str == 'INITIAL-OFFSET':
                    initial_offset = int(opt_value)
            elif isinstance(opt, (lisptype.LispSymbol, lisptype.lispKeyword)):
                bare = _sym_name(opt).upper()
                if bare == 'CONSTRUCTOR':
                    constructors_specified = True
                    constructors.append((None, None))
                elif bare == 'COPIER':
                    copier_name = _DEFAULT
                elif bare == 'PREDICATE':
                    predicate_name = _DEFAULT
                elif bare == 'NAMED':
                    named_option = True
                elif bare == 'CONC-NAME':
                    # A bare `:conc-name` atom is the same option as `(:conc-
                    # name)` -- no value supplied -- and CLHS 3.4.6 says *no*
                    # value (as opposed to omitting the option entirely)
                    # suppresses the prefix, matching `(:conc-name nil)`; it
                    # does not affirm the default the way bare `:copier`/
                    # `:predicate` affirm theirs. STRUCT-TEST-07/34 name their
                    # accessors with no prefix at all (`A07`, not
                    # `STRUCT-TEST-07-A07`) for exactly this option shape.
                    conc_name_explicit = ''
            options = cdr(options)
    else:
        struct_name = name_and_options

    struct_class_name = _sym_name(struct_name)
    conc_name = conc_name_explicit if conc_name_explicit is not None else struct_class_name + '-'

    if not constructors_specified:
        constructors = [('MAKE-' + struct_class_name, None)]
    else:
        constructors = [(nm if nm is not None else 'MAKE-' + struct_class_name, boa)
                         for nm, boa in constructors]
    copier_name = ('COPY-' + struct_class_name if copier_name is _DEFAULT
                   else None if copier_name is _SUPPRESSED else copier_name)
    predicate_was_explicit = predicate_name is not _DEFAULT
    predicate_name = (struct_class_name + '-P' if predicate_name is _DEFAULT
                      else None if predicate_name is _SUPPRESSED else predicate_name)

    def _parse_slot_spec(spec):
        """(name [default-form [:type type-spec] [:read-only ro-form]]) or a bare name."""
        if not _consp_internal(spec):
            return _sym_name(spec), lisptype.NIL, None, False
        slot_name_str = _sym_name(car(spec))
        tail = cdr(spec)
        default_form = lisptype.NIL
        if _consp_internal(tail):
            default_form = car(tail)
            tail = cdr(tail)
        type_spec = None
        read_only = False
        while _consp_internal(tail) and _consp_internal(cdr(tail)):
            key_str = _sym_name(car(tail)).upper()
            val = car(cdr(tail))
            if key_str == 'TYPE':
                type_spec = val
            elif key_str == 'READ-ONLY':
                read_only = not _is_nil_value(val)
            tail = cdr(cdr(tail))
        return slot_name_str, default_form, type_spec, read_only

    if type_option_form is not None:
        return _eval_typed_defstruct(
            struct_name=struct_name, struct_class_name=struct_class_name,
            type_option_form=type_option_form, named_option=named_option,
            initial_offset=initial_offset, include_parent_name=include_parent_name,
            include_overrides=include_overrides, slot_specs=slot_specs,
            conc_name=conc_name, predicate_name=predicate_name,
            predicate_was_explicit=predicate_was_explicit, copier_name=copier_name,
            constructors=constructors, current_pkg=current_pkg,
            global_env=global_env, env=env, parse_slot_spec=_parse_slot_spec,
            eval_initform=_eval_initform)

    # Resolve superclass: the :INCLUDE parent's class, or STRUCTURE-OBJECT.
    if include_parent_name is not None:
        parent_class = classes.find_class(include_parent_name)
        if parent_class is None:
            raise lisptype.LispError(
                f"DEFSTRUCT: :INCLUDE parent not found: {include_parent_name}")
    else:
        parent_class = classes.find_class('STRUCTURE-OBJECT')

    # This DEFSTRUCT's own direct slots: :INCLUDE overrides (of inherited
    # slots) first, matching structure-00.lsp's insertion-order-preserving
    # dict overlay in `LispClass.get_all_slots`, then this form's own new
    # body slots.
    direct_slots = []
    if include_parent_name is not None:
        parent_slot_names = set(parent_class.get_all_slots().keys())
        for override_spec in include_overrides:
            name_str, default_form, type_spec, read_only = _parse_slot_spec(override_spec)
            if name_str not in parent_slot_names:
                raise lisptype.LispError(
                    f"DEFSTRUCT :INCLUDE: {name_str} does not name a slot of {include_parent_name}")
            direct_slots.append(classes.SlotDefinition(
                name=lisptype.LispSymbol(name_str), initform=default_form,
                type_spec=type_spec, read_only=read_only, definition_env=env))

    cur = slot_specs
    while _consp_internal(cur):
        name_str, default_form, type_spec, read_only = _parse_slot_spec(car(cur))
        direct_slots.append(classes.SlotDefinition(
            name=lisptype.LispSymbol(name_str), initform=default_form,
            type_spec=type_spec, read_only=read_only, definition_env=env))
        cur = cdr(cur)

    struct_class = classes.make_class(
        name=struct_name,
        direct_superclasses=[parent_class] if parent_class is not None else [],
        direct_slots=direct_slots,
        documentation=doc_string,
        metaclass_name='STRUCTURE-CLASS')
    classes.register_class(struct_class)

    ordered_slots = list(struct_class.get_all_slots().items())

    def _default_slot_values():
        return {name_str: _eval_initform(slot_def) for name_str, slot_def in ordered_slots}

    def _default_constructor_lambda_list():
        """The implicit ``(&key slot...)`` lambda list CLHS 3.4.6 gives a
        keyword constructor -- one `&key` parameter per slot, each defaulting
        to that slot's own initform. Building this and handing it to
        `make_boa_constructor` below is what makes a keyword constructor go
        through the *same* CLHS 3.4.1.4/3.5.1.5 argument checking as a BOA
        constructor's `&key` parameters -- leftmost-wins on a repeated
        keyword, PROGRAM-ERROR on an odd argument count, a non-symbol key or
        an undeclared keyword. The hand-rolled loop this replaced matched
        keys case-insensitively by scanning every slot name and silently
        ignored all three.

        No `&allow-other-keys` here: that would make the lambda list itself
        permanently waive CLHS 3.5.1.5's check (STRUCTURE-BOA-TEST-16/3 --
        `(make-sbt-16 :d 1)` -- must still signal PROGRAM-ERROR). A caller
        passes `:allow-other-keys t` at the *call site* instead
        (STRUCTURE-BOA-TEST-16/7,8,11), which `_bind_keyword_parameters`
        already honors regardless of what the lambda list declares.
        """
        tail = lisptype.NIL
        for name_str, _slot_def in reversed(ordered_slots):
            tail = lisptype.lispCons(lisptype.LispSymbol(name_str), tail)
        return lisptype.lispCons(lisptype.LispSymbol('&KEY'), tail)

    def make_boa_constructor(boa_ll, ctor_name):
        """A `:constructor` whose second element is a BOA lambda list (CLHS
        3.4.6): an ordinary lambda list, bound the ordinary way, whose
        variables initialize the *same-named* slot rather than being
        collected as keyword arguments. Goes through the same
        `parse_lambda_list` / `_bind_ordinary_lambda_list_tail` /
        `BindingFrame` machinery as LAMBDA/DEFUN, so &optional/&rest/&key/
        &aux, supplied-p variables and arity checking all behave exactly as
        they do for an ordinary function -- rather than a second, partial
        parser for the same lambda-list grammar. The one BOA-specific rule,
        `default_fallback`, is a hook that mechanism now exposes: an
        &optional/&key parameter with no default-value form of its own
        defaults to the matching slot's own initform, not NIL (structures-03
        test 05: `(&optional a b c)` naming slots defaulted `3 2 1`).
        """
        from .evaluation_core import parse_lambda_list, eval as eval_fn
        from .binding import BindingFrame

        parsed = parse_lambda_list(boa_ll)
        required_params = parsed['required']
        param_vars = _lambda_list_variables(parsed)
        param_names = {var.name for var in param_vars}
        slot_initforms = {name_str: slot_def.initform for name_str, slot_def in ordered_slots}

        def default_fallback(var):
            if isinstance(var, lisptype.LispSymbol):
                return slot_initforms.get(var.name)
            return None

        def constructor(*call_args):
            call_args = tuple(_canonicalize_nil_symbol(a) for a in call_args)
            _check_ordinary_arity(parsed, call_args, ctor_name)

            # Rooted at `env` -- the lexical environment DEFSTRUCT itself
            # was evaluated in -- not `global_env`: a slot's default-value
            # form (BOA-supplied or the struct's own initform, reached here
            # through `default_fallback`) can close over a binding from that
            # scope, the way DEFCLASS's slot initforms already do (structures
            # -02's STRUCTURE-62 defines a slot whose default is `#'%f`, an
            # FLET-local function visible only through DEFSTRUCT's own
            # lexical environment).
            func_env = lisptype.Environment(env)
            frame = BindingFrame(func_env, body=lisptype.NIL, bound_vars=param_vars)
            try:
                for index, param in enumerate(required_params):
                    frame.bind(param, call_args[index])
                _bind_ordinary_lambda_list_tail(
                    parsed, call_args, len(required_params), func_env, eval_fn, frame,
                    default_fallback=default_fallback)

                # A slot the lambda list never mentions gets its own initform
                # here; a slot it does mention was already initialized by the
                # binder above (from the argument or `default_fallback`), and
                # must not be evaluated a second time here -- an initform can
                # have a side effect (structures-02's S-2-F6 slot default is
                # `(incf *s-2-f6-counter*)`), and evaluating it once to seed
                # this dict and again through the binder counted every
                # construction twice.
                slot_values = {name_str: _eval_initform(slot_def)
                               for name_str, slot_def in ordered_slots
                               if name_str not in param_names}
                for slot_name_str, _slot_def in ordered_slots:
                    for var in param_vars:
                        if var.name == slot_name_str:
                            slot_values[slot_name_str] = func_env.find_variable(var)
                            break
                return classes.LispInstance(lisp_class=struct_class, slot_values=slot_values)
            finally:
                frame.unwind()

        constructor.__lisp_lambda_list__ = boa_ll
        return constructor

    for ctor_name, boa_ll in constructors:
        ctor_sym = current_pkg.intern_symbol(ctor_name)
        effective_ll = boa_ll if boa_ll is not None else _default_constructor_lambda_list()
        global_env.add_function(ctor_sym, make_boa_constructor(effective_ll, ctor_name))

    if predicate_name:
        def is_structure(obj):
            # True for this structure type or any subtype (CLHS TYPEP semantics).
            if isinstance(obj, classes.LispInstance):
                for cls in obj.lisp_class.get_linearized_superclasses():
                    if cls is struct_class:
                        return lisptype.T
            return lisptype.NIL

        predicate_sym = current_pkg.intern_symbol(predicate_name)
        global_env.add_function(predicate_sym, is_structure)

    if copier_name:
        def copy_structure_fn(struct):
            # Copies using the instance's *actual* runtime class, so a
            # parent's copier called on a subtype instance still works.
            if not isinstance(struct, classes.LispInstance):
                raise lisptype.LispTypeError(f"Not a structure: {struct}")
            return classes.LispInstance(lisp_class=struct.lisp_class,
                                         slot_values=dict(struct.slot_values))

        copier_sym = current_pkg.intern_symbol(copier_name)
        global_env.add_function(copier_sym, copy_structure_fn)

    # Accessors for *every* slot of this struct, inherited included: CLHS
    # 7.2's :INCLUDE effectively appends the parent's slot descriptions to
    # this DEFSTRUCT's own, so a child gets a full accessor set under its
    # own conc-name even for a slot it never mentions (STRUCT-INCLUDE.7
    # calls `struct-include-04b-b` on a slot only STRUCT-INCLUDE-04A
    # declared). The parent's own accessor for that slot keeps working on
    # a child instance too, since it looks the slot up generically by name
    # rather than by exact class.
    for slot_name_str, slot_def in ordered_slots:
        accessor_name = conc_name + slot_name_str
        read_only = slot_def.read_only

        def make_getter(sn, an):
            def getter(instance):
                if not isinstance(instance, classes.LispInstance) or sn not in instance.lisp_class.get_all_slots():
                    raise lisptype.LispTypeError(f"{an}: not a {struct_class_name}: {instance}")
                return instance.slot_values.get(sn, lisptype.NIL)
            return getter

        accessor_sym = current_pkg.intern_symbol(accessor_name)
        global_env.add_function(accessor_sym, make_getter(slot_name_str, accessor_name))

        if not read_only:
            def make_setter(sn, an):
                def setter(instance, value):
                    if not isinstance(instance, classes.LispInstance) or sn not in instance.lisp_class.get_all_slots():
                        raise lisptype.LispTypeError(f"{an}: not a {struct_class_name}: {instance}")
                    instance.slot_values[sn] = value
                    return value
                return setter

            setter_sym = current_pkg.intern_symbol('SET-' + accessor_name)
            global_env.add_function(setter_sym, make_setter(slot_name_str, accessor_name))

    return struct_name


@_registry.cl_function('COPY-STRUCTURE')
def copy_structure(structure):
    """COPY-STRUCTURE (CLHS 7.2): a shallow copy of any structure, not just
    one whose own `copy-<name>` function was defined. Previously absent
    entirely -- `(copy-structure x)` raised Undefined function -- since
    DEFSTRUCT only ever bound the per-struct `COPY-<name>` name."""
    import fclpy.classes as classes
    if not isinstance(structure, classes.LispInstance) or structure.lisp_class.metaclass_name != 'STRUCTURE-CLASS':
        raise lisptype.LispTypeError(f"COPY-STRUCTURE: not a structure: {structure}")
    return classes.LispInstance(lisp_class=structure.lisp_class,
                                 slot_values=dict(structure.slot_values))


def _setf_form(head, *args):
    """Build the Lisp form (head arg1 arg2 ...), unevaluated.

    A string `head` is interned in COMMON-LISP rather than built as a
    bare, uninterned `LispSymbol` -- special-form dispatch only compares
    `.name` so an uninterned symbol would still evaluate correctly, but
    `get-setf-expansion.1` inspects the returned form structurally with
    EQUAL against a backquoted `(function (setf ,fn))`, whose FUNCTION/
    SETF symbols *are* the interned ones.
    """
    if isinstance(head, str):
        head = lisptype.COMMON_LISP_PACKAGE.intern_symbol(head)
    op = head
    result = lisptype.NIL
    for a in reversed(args):
        result = lisptype.lispCons(a, result)
    return lisptype.lispCons(op, result)


def _setf_pylist_to_form(items):
    """Build a proper Lisp list from a Python list, unevaluated."""
    result = lisptype.NIL
    for a in reversed(items):
        result = lisptype.lispCons(a, result)
    return result


def _setf_form_args(lst):
    """Walk a Lisp list into a Python list of its (unevaluated) elements."""
    out = []
    cur = lst
    while _consp_internal(cur):
        out.append(car(cur))
        cur = cdr(cur)
    return out


def _setf_literal_list_forms(form):
    """If `form` is literally (QUOTE (...)) or NIL, return its elements as a
    Python list of forms; otherwise None. Used by the APPLY place (CLHS
    5.1.2.5), which cannot resolve a non-literal spread list without
    evaluating the place -- something GET-SETF-EXPANSION must not do.
    """
    if form is lisptype.NIL or (isinstance(form, lisptype.LispSymbol) and form.name == 'NIL'):
        return []
    if _consp_internal(form) and isinstance(car(form), lisptype.LispSymbol) and car(form).name == 'QUOTE':
        quoted = car(cdr(form)) if _consp_internal(cdr(form)) else lisptype.NIL
        if quoted is lisptype.NIL or (isinstance(quoted, lisptype.LispSymbol) and quoted.name == 'NIL'):
            return []
        if _consp_internal(quoted):
            return _setf_form_args(quoted)
        return None
    return None


def _rewrite_setf_apply(place_args):
    """(APPLY #'fn a1..an spread) -> (fn a1..an . elements-of-spread), CLHS
    5.1.2.5, for the literal-final-argument shape ansi-test's setf-apply.*
    tests use. Returns the rewritten place, or None if it cannot be
    rewritten this way.
    """
    if not place_args:
        return None
    fn_designator = place_args[0]
    if not (_consp_internal(fn_designator) and isinstance(car(fn_designator), lisptype.LispSymbol)
            and car(fn_designator).name == 'FUNCTION'):
        return None
    fn_name = car(cdr(fn_designator)) if _consp_internal(cdr(fn_designator)) else None
    if not isinstance(fn_name, lisptype.LispSymbol):
        return None
    rest = place_args[1:]
    if not rest:
        return None
    spread = _setf_literal_list_forms(rest[-1])
    if spread is None:
        return None
    return lisptype.lispCons(fn_name, _setf_pylist_to_form(list(rest[:-1]) + spread))


def _fclpy_array_place_set(op_sym, value, *indices):
    """Internal helper: the write half of an array place (AREF/SVREF/BIT/
    SBIT/ROW-MAJOR-AREF/FILL-POINTER/...), reached only through a
    GET-SETF-EXPANSION storing-form -- there is no real `(SETF AREF)`
    Lisp function to call, since the write happens in
    `arrays.array_place_write`, so this is the one bridge from a form
    back into it. Registered as a plain internal function (the same
    pattern PUSH/POP use) rather than special-cased in the evaluator.
    """
    _arrays.array_place_write(op_sym.name, list(indices), value)
    return value


_registry.function_registry['%FCLPY-ARRAY-PLACE-SET'] = _registry.RegistryEntry(
    name='%FCLPY-ARRAY-PLACE-SET', py_name='_fclpy_array_place_set',
    kind='function', func=_fclpy_array_place_set)


def _fclpy_array_place_apply(op_sym, value, *args):
    """Internal helper: the write half of an APPLY place over an array
    operator -- `(setf (apply #'aref a i1... spread) v)`. `args` are APPLY's
    own arguments after the function designator, already evaluated, so the
    *last* one is APPLY's spread list and the rest are the leading
    subscripts (CLHS 14.2 / 5.1.2.5): `(apply #'aref a 1 2 '(3 4))` reads
    `(aref a 1 2 3 4)`.
    """
    rest = list(args)
    spread = rest.pop() if rest else None
    from .sequence_protocol import seq_elements
    indices = rest + (list(seq_elements(spread)) if not _null_internal(spread) else [])
    _arrays.array_place_write(op_sym.name, indices, value)
    return value


_registry.function_registry['%FCLPY-ARRAY-PLACE-APPLY'] = _registry.RegistryEntry(
    name='%FCLPY-ARRAY-PLACE-APPLY', py_name='_fclpy_array_place_apply',
    kind='function', func=_fclpy_array_place_apply)


def _fclpy_setf_symbol_value(sym, value):
    from .utilities_symbols import _require_symbol
    _require_symbol(sym, 'SYMBOL-VALUE')
    sym.value = value
    return value


def _fclpy_setf_symbol_function(sym, value):
    """SETF of SYMBOL-FUNCTION writes the symbol's *global* function cell
    (CLHS symbol-function has no lexical component: a FLET/MACROLET local
    binding shadows it but is not written by it), so this walks to the root
    environment and binds there. Writing to `state.current_environment`
    instead was wrong twice over: from inside a SETF expansion it was the
    expansion's own transient LET* environment, whose bindings evaporate
    with it (fboundp answered NIL immediately after the setf), and even at
    the site it would have overwritten a lexical FLET binding instead of
    the global cell."""
    from .utilities_symbols import _require_symbol
    _require_symbol(sym, 'SYMBOL-FUNCTION')
    env = state.current_environment
    while env is not None and env.parent is not None:
        env = env.parent
    env.add_function(sym, value)
    return value


def _fclpy_setf_symbol_plist(sym, value):
    from .utilities_symbols import _require_symbol
    _require_symbol(sym, 'SYMBOL-PLIST')
    sym.plist = value
    return value


def _fclpy_setf_gethash(key, table, value):
    from .misc_hashtables import puthash
    return puthash(key, table, value)


def _fclpy_setf_nth_element(idx, seq, value):
    """Set element at index in a list (for NTH/SECOND/etc)."""
    if _consp_internal(seq):
        cell = seq
        for _ in range(int(idx)):
            if not _consp_internal(cell):
                raise lisptype.LispError("NTH place: index out of bounds")
            cell = cdr(cell)
        if not _consp_internal(cell):
            raise lisptype.LispError("NTH place: index out of bounds")
        cell.car = value
        return value
    else:
        # Python list / vector
        seq[int(idx)] = value
        return value


def _sequence_place_bound(seq, what):
    """The index bound for the CHAR/SCHAR/ELT places, shared by both faces
    of the place. ELT is a *sequence* accessor (CLHS 17.3) and is bounded by
    the active length -- `seq_length`, so a fill pointer bounds it exactly as
    the ELT getter is. CHAR/SCHAR are *array* accessors (CLHS 15.1.1) and
    are **not** affected by the fill pointer: they may address every element
    up to the array total size, which is what makes CHAR.8's
    `(setf (char s 5))` on a size-6 string with fill pointer 4 legal.
    Checked *before* any write: the raw `seq[idx] = value` this replaced let
    Python's `IndexError` surface as the value of the form -- not a
    TYPE-ERROR (ELT-V.11, ELT-ADJ-ARRAY.10) -- and `LispArray.__setitem__`
    wraps negatives Python-style, so `(setf (elt v -100) d)` addressed a
    wrapped slot instead of signalling (ELT-ADJ-ARRAY.11)."""
    from .sequence_protocol import seq_length
    if what == 'ELT':
        return seq_length(seq, what)
    if isinstance(seq, lisptype.LispString):
        return len(seq._data)
    if isinstance(seq, lisptype.LispArray):
        return seq.total_size
    return seq_length(seq, what)


def _check_sequence_place_index(seq, idx, what):
    length = _sequence_place_bound(seq, what)
    if idx < 0 or idx >= length:
        raise lisptype.LispTypeError(
            f"{what}: index {idx} is out of bounds for a sequence of length "
            f"{length}",
            expected_type=f"index in [0,{length})", actual_value=idx)
    return length


def _fclpy_setf_sequence_element(seq, idx, value, what='ELT'):
    """Set element at index in a sequence (for CHAR/SCHAR/ELT)."""
    idx = int(idx)
    _check_sequence_place_index(seq, idx, what)
    if _consp_internal(seq):
        cell = seq
        for _ in range(idx):
            cell = cdr(cell)
        cell.car = value
        return value
    seq[idx] = value
    return value


def _fclpy_setf_fdefinition(name, value):
    """SETF of FDEFINITION (CLHS): writes the *global* function binding --
    see `_fclpy_setf_symbol_function` for why the root environment, and not
    `state.current_environment` (a SETF expansion's own LET*, or a lexical
    FLET that shadows the name without being its target), is where this
    lands."""
    from .utilities_functions import _function_spec_to_key
    key = _function_spec_to_key(name)
    env = state.current_environment
    while env is not None and env.parent is not None:
        env = env.parent
    env.add_function(key, value)
    return value


def _fclpy_setf_get(sym, indicator, value):
    """Set property value in symbol's plist."""
    from .utilities_symbols import _require_symbol
    _require_symbol(sym, 'GET')
    if not _consp_internal(sym.plist):
        sym.plist = lisptype.NIL
    cur = sym.plist
    while _consp_internal(cur) and _consp_internal(cdr(cur)):
        if car(cur) == indicator:
            cdr(cur).car = value
            return value
        cur = cdr(cdr(cur))
    sym.plist = lisptype.lispCons(indicator, lisptype.lispCons(value, sym.plist))
    return value


def _fclpy_setf_getf(plist, indicator, value):
    """The write half of a GETF place (CLHS 5.1.2.6): mutate an existing
    indicator's value cell in place, or build the fresh
    (indicator value) pair prepended to `plist` -- either way the *new*
    plist is returned, and the GETF expansion's LET writes it back through
    the plist place's own storing form. (The old body raised
    LispNotImplementedError: it was the store form GET-SETF-EXPANSION's
    GETF branch generated, and nothing could ever evaluate it -- so INCF
    of a GETF place signalled undefined-function %FCLPY-SETF-GETF while
    SETF of one worked, because only the ladder handled GETF.)
    """
    current = plist
    while _consp_internal(current) and _consp_internal(cdr(current)):
        if car(current) == indicator:
            cdr(current).car = value
            return plist
        current = cdr(cdr(current))
    return lisptype.lispCons(indicator, lisptype.lispCons(value, plist))


def _fclpy_setf_find_class(name, value):
    """Register or unregister a class under a name."""
    import fclpy.classes as _classes
    if isinstance(value, _classes.LispClass):
        _classes.register_class_as(name, value)
    elif _null_internal(value):
        _classes.unregister_class_as(name)
    else:
        raise lisptype.LispError("FIND-CLASS place: value must be a class or NIL")
    return value


def _fclpy_setf_macro_function(sym, value):
    """Install a macro definition for `sym` (SETF of MACRO-FUNCTION) in the
    root environment -- a global binding (CLHS macro-function, like
    symbol-function, has no lexical component). One home for both faces of
    the place: `get_setf_expansion`'s store form and `_place_accessor`'s
    closure setter both call this.

    Deliberately NOT mirrored into the lexical environment: a global
    definition must not be shadowed by a stale copy of the previous one --
    `(defun g ...)` inside a LET followed by `(setf (macro-function g) ...)`
    left the defun's lexical install in the way (macro-function.10).
    """
    if not isinstance(sym, lisptype.LispSymbol):
        raise lisptype.LispError("SETF MACRO-FUNCTION: requires a symbol")
    env = state.current_environment
    while env is not None and env.parent is not None:
        env = env.parent
    env.add_function(sym, value)
    return value


def _fclpy_setf_subseq(seq, start, end, new_seq):
    """(SETF (SUBSEQ seq start [end]) new-seq) -- CLHS 17.1: copy as many of
    new-seq's elements as fit into seq in place; it does not resize seq.
    `end` of NIL means "to the end of seq" (the 2-argument place's expansion
    binds its end temp to NIL). One implementation for both faces of the
    protocol: the `%FCLPY-SETF-SUBSEQ` store form and `_place_accessor`'s
    SUBSEQ closure both call this.
    """
    from .sequence_protocol import seq_elements
    src = seq_elements(new_seq)
    start_idx = int(start)
    if _consp_internal(seq):
        cell = seq
        for _ in range(start_idx):
            if not _consp_internal(cell):
                raise lisptype.LispError("SUBSEQ place: start index out of bounds")
            cell = cdr(cell)
        limit = (end - start_idx) if not _null_internal(end) else len(src)
        i = 0
        while _consp_internal(cell) and i < len(src) and i < limit:
            cell.car = src[i]
            cell = cdr(cell)
            i += 1
        return new_seq
    # Vector (Python list / LispString / LispArray): write in place, never
    # resize -- the old `seq[start:end] = new_seq` slice assignment let a
    # longer new-seq grow the vector, which SUBSEQ's place definition
    # forbids.
    end_idx = end if not _null_internal(end) else len(seq)
    for i in range(start_idx, min(end_idx, start_idx + len(src))):
        seq[i] = src[i - start_idx]
    return new_seq


for _helper_name, _helper_fn in (
    ('%FCLPY-SETF-SYMBOL-VALUE', _fclpy_setf_symbol_value),
    ('%FCLPY-SETF-SYMBOL-FUNCTION', _fclpy_setf_symbol_function),
    ('%FCLPY-SETF-SYMBOL-PLIST', _fclpy_setf_symbol_plist),
    ('%FCLPY-SETF-GETHASH', _fclpy_setf_gethash),
    ('%FCLPY-SETF-NTH-ELEMENT', _fclpy_setf_nth_element),
    ('%FCLPY-SETF-SEQUENCE-ELEMENT', _fclpy_setf_sequence_element),
    ('%FCLPY-SETF-FDEFINITION', _fclpy_setf_fdefinition),
    ('%FCLPY-SETF-GET', _fclpy_setf_get),
    ('%FCLPY-SETF-GETF', _fclpy_setf_getf),
    ('%FCLPY-SETF-FIND-CLASS', _fclpy_setf_find_class),
    ('%FCLPY-SETF-MACRO-FUNCTION', _fclpy_setf_macro_function),
    ('%FCLPY-SETF-SUBSEQ', _fclpy_setf_subseq),
):
    _registry.function_registry[_helper_name] = _registry.RegistryEntry(
        name=_helper_name, py_name=_helper_fn.__name__, kind='function', func=_helper_fn)


def get_setf_expansion(place, env):
    """CLHS 5.1.2.1 GET-SETF-EXPANSION -- the one form-based place protocol.

    Returns (temps, vals, stores, store_form, access_form), all plain
    Python lists / unevaluated Lisp forms. This exists for the cases that
    are genuinely code generation -- a place whose expansion is DEFSETF
    long-form / DEFINE-SETF-EXPANDER-supplied Lisp code, or CLHS 5.1.2.9's
    generic function-call fallback -- because those are extended by a
    *user* writing Lisp code that must see real forms. `_place_accessor`
    handles every other place kind directly as a (getter, setter) closure
    pair without ever building a form, and bridges into this protocol
    (`_accessor_from_expansion`) only for what is left, so there is one
    home for "which mechanism does this place use" rather than a second
    ladder here duplicating SETF's.
    """
    from .misc_packages import _direct_macroexpand_1
    from .utilities_symbols import gensym as _gensym_fn

    if isinstance(place, lisptype.LispSymbol):
        expansion = env.get_symbol_macro(place)
        if expansion is not None:
            return get_setf_expansion(expansion, env)
        store = _gensym_fn()
        return ([], [], [store],
                _setf_form('SETQ', place, store),
                place)

    if not _consp_internal(place):
        raise lisptype.LispError("GET-SETF-EXPANSION: not a valid place")

    op = car(place)
    if not lisptype.is_symbol(op):
        raise lisptype.LispError("GET-SETF-EXPANSION: place operator must be a symbol")
    # A call place names its operator by symbol name -- NIL's included
    # (`(setf (nil) 10)` is a `(SETF NIL)` call, flet.51 exercises it), and
    # the canonical NIL object is not a LispSymbol instance.
    op_name = getattr(op, 'name', None) or 'NIL'
    place_args = _setf_form_args(cdr(place))

    # CLHS 5.1.2.3 -- a place naming several subplaces at once.
    if op_name == 'VALUES':
        temps, vals, stores, store_forms, access_forms = [], [], [], [], []
        for p in place_args:
            t, v, s, sf, af = get_setf_expansion(p, env)
            temps += t; vals += v
            access_forms.append(af)
            if len(s) == 1:
                stores.append(s[0])
                store_forms.append(sf)
            else:
                # CLHS 5.1.2.5 + 5.1.3: the store clause of the outer VALUES
                # place has ONE store variable per *direct* sub-place, and
                # each receives exactly one value of the value form -- a
                # nested VALUES place (whose own expansion has one store per
                # of its own sub-places) is handed that single value as its
                # first store and NIL for the rest. Binding every nested
                # store var directly from the value form flattened the
                # distribution ((setf (values a (values b c)) (values 0 1 2))
                # wrote C when CLHS leaves it NIL) -- the values.20/.21 pair.
                outer = _gensym_fn()
                from .sequence_protocol import make_lisp_list as _mk_list
                bindings = _mk_list([
                    _setf_form(sj, outer if j == 0 else lisptype.NIL)
                    for j, sj in enumerate(s)])
                store_forms.append(
                    _setf_form('LET', bindings, sf))
                stores.append(outer)
        store_form = _setf_form('PROGN', *(store_forms + [_setf_form('VALUES', *stores)]))
        access_form = _setf_form('VALUES', *access_forms)
        return temps, vals, stores, store_form, access_form

    # CLHS 5.1.2.4 -- (THE type place) reads through the assertion; writes
    # go straight to the underlying place.
    if op_name == 'THE' and len(place_args) == 2:
        type_spec, subplace = place_args
        temps, vals, stores, store_form, access_form = get_setf_expansion(subplace, env)
        return temps, vals, stores, store_form, _setf_form('THE', type_spec, access_form)

    # CLHS 5.1.2.5 -- a literal-final-argument spread rewrites to the
    # underlying place outright; a *computed* spread keeps APPLY's shape,
    # storing through the operator's writer applied over the spread (the
    # same semantics `(setf (apply #'aref a i args) v)` has everywhere:
    # `(apply #'(setf aref) v a i args)` -- defgeneric.33).
    if op_name == 'APPLY':
        rewritten = _rewrite_setf_apply(place_args)
        if rewritten is not None:
            return get_setf_expansion(rewritten, env)
        fn_form = place_args[0] if place_args else None
        if (_consp_internal(fn_form) and isinstance(car(fn_form), lisptype.LispSymbol)
                and car(fn_form).name == 'FUNCTION'):
            fn_name = car(cdr(fn_form))
            if isinstance(fn_name, lisptype.LispSymbol):
                temps = [_gensym_fn() for _ in place_args[1:]]
                store = _gensym_fn()
                if _arrays.is_array_place(fn_name.name):
                    # Array-place ops write through the same runtime the
                    # literal shape rewrites to; the spread list is the
                    # temp holding APPLY's last argument.
                    store_form = _setf_form(
                        'PROGN',
                        _setf_form('%FCLPY-ARRAY-PLACE-APPLY',
                                   _setf_form('QUOTE', fn_name), store, *temps),
                        store)
                else:
                    # CLHS 5.1.2.9's writer, applied: the newvalue first,
                    # then the access-form's arguments, then the spread.
                    store_form = _setf_form(
                        'PROGN',
                        _setf_form('APPLY', _setf_form('FUNCTION',
                                   _setf_form('SETF', fn_name)),
                                   store, *temps),
                        store)
                access_form = _setf_form('APPLY', _setf_form('FUNCTION', fn_name), *temps)
                return temps, list(place_args[1:]), [store], store_form, access_form
        raise lisptype.LispNotImplementedError(
            "SETF of APPLY requires a literal quoted (or NIL) final argument list")

    # CLHS 5.1.2.1's compound CAR/CDR accessors -- mirrors `_place_accessor`'s
    # closure-based fast path (via the same real RPLACA/RPLACD functions),
    # so a *macro* place that expands to one of these (setf-macro.1/.3/.4)
    # resolves the same way here as it would going straight through
    # `_place_accessor`, instead of falling all the way to the generic
    # (SETF fn) fallback below, which has no `(SETF CAR)` function to call.
    if op_name in ('CAR', 'FIRST') and len(place_args) == 1:
        temp, store = _gensym_fn(), _gensym_fn()
        store_form = _setf_form('PROGN', _setf_form('RPLACA', temp, store), store)
        return [temp], list(place_args), [store], store_form, _setf_form('CAR', temp)
    if op_name in ('CDR', 'REST') and len(place_args) == 1:
        temp, store = _gensym_fn(), _gensym_fn()
        store_form = _setf_form('PROGN', _setf_form('RPLACD', temp, store), store)
        return [temp], list(place_args), [store], store_form, _setf_form('CDR', temp)
    cxr_match = _CXR_RE.match(op_name)
    if cxr_match and len(place_args) == 1:
        letters = cxr_match.group(1)
        temp, store = _gensym_fn(), _gensym_fn()
        parent_form = temp
        for ch in reversed(letters[1:]):
            parent_form = _setf_form('CDR' if ch == 'D' else 'CAR', parent_form)
        final_op = 'RPLACA' if letters[0] == 'A' else 'RPLACD'
        store_form = _setf_form('PROGN', _setf_form(final_op, parent_form, store), store)
        return [temp], list(place_args), [store], store_form, _setf_form(op, temp)

    # Every other place kind `_place_accessor` knows as a closure needs a
    # real form here too -- GET-SETF-EXPANSION, DEFINE-SETF-EXPANDER and
    # DEFINE-MODIFY-MACRO bodies call this directly on an arbitrary place
    # (e.g. `(aref a (incf i))`), bypassing `_place_accessor`'s fast
    # closures entirely, so the generic (SETF fn) fallback further down
    # would wrongly be tried for all of these otherwise.
    if _arrays.is_array_place(op_name):
        temps = [_gensym_fn() for _ in place_args]
        store = _gensym_fn()
        store_form = _setf_form('%FCLPY-ARRAY-PLACE-SET', _setf_form('QUOTE', op), store, *temps)
        access_form = _setf_form(op, *temps)
        return temps, list(place_args), [store], store_form, access_form

    if op_name == 'GETHASH' and len(place_args) >= 2:
        # *Every* subform gets a temp, the optional default included. CLHS
        # 5.1.1.1 evaluates a place's subforms left to right exactly once, and
        # `place_args[:2]` dropped the default form -- so
        # `(setf (gethash 'x table (incf i)) 'y)` never incremented I
        # (`gethash.5`, `gethash.order.4`). The default takes no part in the
        # *store*, but it is still a subform and must still be evaluated.
        temps = [_gensym_fn() for _ in place_args]
        store = _gensym_fn()
        store_form = _setf_form('PROGN', _setf_form('%FCLPY-SETF-GETHASH', temps[0], temps[1], store), store)
        access_form = _setf_form('GETHASH', *temps)
        return temps, list(place_args), [store], store_form, access_form

    if op_name in ('SYMBOL-VALUE', 'SYMBOL-FUNCTION', 'SYMBOL-PLIST') and len(place_args) == 1:
        setter_name = {'SYMBOL-VALUE': '%FCLPY-SETF-SYMBOL-VALUE',
                        'SYMBOL-FUNCTION': '%FCLPY-SETF-SYMBOL-FUNCTION',
                        'SYMBOL-PLIST': '%FCLPY-SETF-SYMBOL-PLIST'}[op_name]
        temp, store = _gensym_fn(), _gensym_fn()
        store_form = _setf_form('PROGN', _setf_form(setter_name, temp, store), store)
        return [temp], list(place_args), [store], store_form, _setf_form(op, temp)

    # NTH accessor functions (SECOND, THIRD, ..., TENTH) + generic NTH
    _NTH_ACCESSOR_INDEX = {
        'FIRST': 0, 'SECOND': 1, 'THIRD': 2, 'FOURTH': 3, 'FIFTH': 4,
        'SIXTH': 5, 'SEVENTH': 6, 'EIGHTH': 7, 'NINTH': 8, 'TENTH': 9,
    }
    if op_name in _NTH_ACCESSOR_INDEX and op_name != 'FIRST' and len(place_args) == 1:
        temp, store = _gensym_fn(), _gensym_fn()
        idx = _NTH_ACCESSOR_INDEX[op_name]
        store_form = _setf_form('PROGN', _setf_form('%FCLPY-SETF-NTH-ELEMENT', idx, temp, store), store)
        return [temp], list(place_args), [store], store_form, _setf_form(op, temp)

    if op_name == 'NTH' and len(place_args) == 2:
        temp0, temp1, store = _gensym_fn(), _gensym_fn(), _gensym_fn()
        store_form = _setf_form('PROGN', _setf_form('%FCLPY-SETF-NTH-ELEMENT', temp0, temp1, store), store)
        return [temp0, temp1], list(place_args), [store], store_form, _setf_form('NTH', temp0, temp1)

    # CHAR, SCHAR, ELT - sequence element access
    if op_name in ('CHAR', 'SCHAR', 'ELT') and len(place_args) == 2:
        temp0, temp1, store = _gensym_fn(), _gensym_fn(), _gensym_fn()
        store_form = _setf_form('PROGN',
                                _setf_form('%FCLPY-SETF-SEQUENCE-ELEMENT', temp0, temp1, store,
                                           _setf_form('QUOTE', op_name)),
                                store)
        return [temp0, temp1], list(place_args), [store], store_form, _setf_form(op, temp0, temp1)

    # FDEFINITION - function definition
    if op_name == 'FDEFINITION' and len(place_args) == 1:
        temp, store = _gensym_fn(), _gensym_fn()
        store_form = _setf_form('PROGN', _setf_form('%FCLPY-SETF-FDEFINITION', temp, store), store)
        return [temp], list(place_args), [store], store_form, _setf_form(op, temp)

    # MACRO-FUNCTION - install a macro definition. The place is
    # `(MACRO-FUNCTION symbol [environment])` (CLHS 5.1.3); the environment
    # subform is still *evaluated* exactly once -- macro-function.15 counts
    # the evaluation of both subforms -- even though the store installs
    # globally (NIL designates the global environment, and the ansi tests
    # pass NIL explicitly). The 1-argument place form is the same store
    # without the second temp. FDEFINITION above is the same shape one
    # branch earlier.
    if op_name == 'MACRO-FUNCTION' and len(place_args) in (1, 2):
        if len(place_args) == 2:
            temp0, temp1, store = _gensym_fn(), _gensym_fn(), _gensym_fn()
            store_form = _setf_form('PROGN',
                                    _setf_form('%FCLPY-SETF-MACRO-FUNCTION', temp0, store),
                                    store)
            return [temp0, temp1], list(place_args), [store], store_form, _setf_form(op, temp0, temp1)
        temp, store = _gensym_fn(), _gensym_fn()
        store_form = _setf_form('PROGN',
                                _setf_form('%FCLPY-SETF-MACRO-FUNCTION', temp, store),
                                store)
        return [temp], list(place_args), [store], store_form, _setf_form(op, temp)

    # GET - symbol property
    if op_name == 'GET' and len(place_args) >= 2:
        # GET takes (sym indicator &optional default): every subform
        # including the optional default gets a temp, exactly as GETHASH's
        # branch does -- CLHS 5.1.1.1 evaluates a place's subforms left to
        # right exactly once, and place_args[:2] dropped the default. The
        # store ignores the default's value, but the access form keeps it
        # so a read-modify-write op (INCF) sees it.
        temps = [_gensym_fn() for _ in place_args]
        store = _gensym_fn()
        store_form = _setf_form('PROGN',
                                _setf_form('%FCLPY-SETF-GET', temps[0], temps[1], store),
                                store)
        return temps, list(place_args), [store], store_form, _setf_form('GET', *temps)

    # GETF - property list element. The plist is itself a nested place
    # (CLHS 5.1.2.6), so its own expansion is composed in: the new plist is
    # computed once and written back through the plist place's own storing
    # form, and the fresh store variable below is what the caller binds the
    # newvalue to. The indicator and the optional default are GETF's own
    # subforms and are bound like any other temps (CLHS 5.1.1.1 -- the same
    # rule GETHASH's branch applies to its default; setf-getf.order.1/.2
    # count each subform exactly once), and the access form keeps the
    # default so a read-modify-write op (INCF) sees it (incf-getf.1).
    if op_name == 'GETF' and place_args:
        plist_temps, plist_vals, plist_stores, plist_store_form, plist_access = \
            get_setf_expansion(place_args[0], env)
        if len(plist_stores) != 1:
            raise lisptype.LispNotImplementedError(
                "GETF of a plist place with more than one store variable")
        ind_form = place_args[1] if len(place_args) > 1 else lisptype.NIL
        default_form = place_args[2] if len(place_args) > 2 else lisptype.NIL
        ind_t, dv_t, store = _gensym_fn(), _gensym_fn(), _gensym_fn()
        compute = _setf_form('%FCLPY-SETF-GETF', plist_access, ind_t, store)
        write_through = _setf_form(
            'LET', _setf_pylist_to_form([_setf_form(plist_stores[0], compute)]),
            plist_store_form)
        store_form = _setf_form('PROGN', write_through, store)
        return (plist_temps + [ind_t, dv_t],
                plist_vals + [ind_form, default_form],
                [store], store_form,
                _setf_form('GETF', plist_access, ind_t, dv_t))

    # LDB and MASK-FIELD (CLHS 5.1.2.11 -- the bytespec places): the inner
    # place is itself a place, so its own expansion is composed in. The
    # stored integer is the real DPB/DEPOSIT-FIELD computation over the
    # inner place's current value (the same arithmetic the closure pair in
    # `_place_accessor` applies, reached through the arithmetic functions
    # rather than a second copy of the formula), and the fresh store
    # variable is what the caller binds the newvalue to -- SETF of LDB
    # answers the newvalue itself, not the field-dressed integer
    # (ldb.place.1). Subform order (CLHS 5.1.1.1, ldb.place.order.1): the
    # bytespec's subforms, then the inner place's, then the newvalue.
    if op_name in ('LDB', 'MASK-FIELD') and len(place_args) == 2:
        inner_temps, inner_vals, inner_stores, inner_store_form, inner_access = \
            get_setf_expansion(place_args[1], env)
        if len(inner_stores) != 1:
            raise lisptype.LispNotImplementedError(
                f"SETF of ({op_name} ... ) over a multi-store place")
        bp_t, store = _gensym_fn(), _gensym_fn()
        op_fn = 'DPB' if op_name == 'LDB' else 'DEPOSIT-FIELD'
        compute = _setf_form(op_fn, store, bp_t, inner_access)
        write_through = _setf_form(
            'LET', _setf_pylist_to_form([_setf_form(inner_stores[0], compute)]),
            inner_store_form)
        store_form = _setf_form('PROGN', write_through, store)
        return ([bp_t] + inner_temps,
                [place_args[0]] + inner_vals,
                [store], store_form,
                _setf_form(op, bp_t, inner_access))

    # (SETF (SUBSEQ seq start [end]) new-seq) -- CLHS 17.1: copies as many
    # of new-seq's elements as fit into seq in place. The runtime does the
    # write (`_fclpy_setf_subseq`, shared with `_place_accessor`'s closure);
    # the read side is the ordinary SUBSEQ function, which NIL-as-end means
    # "to the end" for -- so a 2-argument place binds its end temp to NIL.
    if op_name == 'SUBSEQ' and place_args:
        end_form = place_args[2] if len(place_args) > 2 else lisptype.NIL
        temps = [_gensym_fn() for _ in range(3)]
        store = _gensym_fn()
        store_form = _setf_form('PROGN',
                                _setf_form('%FCLPY-SETF-SUBSEQ',
                                           temps[0], temps[1], temps[2], store),
                                store)
        access_form = _setf_form('SUBSEQ', *temps)
        return temps, [place_args[0], place_args[1], end_form], \
            [store], store_form, access_form

    # FIND-CLASS - class registry. The optional `environment` argument is a
    # subform like any other (CLHS 5.1.1.1: it must be evaluated exactly
    # once, find-class.16 counts it), so every place argument gets a temp;
    # the store uses only the name.
    if op_name == 'FIND-CLASS' and place_args:
        temps = [_gensym_fn() for _ in place_args]
        store = _gensym_fn()
        store_form = _setf_form('PROGN',
                                _setf_form('%FCLPY-SETF-FIND-CLASS', temps[0], store),
                                store)
        access_form = _setf_form(op, *temps)
        return temps, list(place_args), [store], store_form, access_form

    # LDB and SUBSEQ are handled directly above; what is left falls through
    # to the registered expanders and the two generic writer shapes below.

    # User-registered DEFSETF / DEFINE-SETF-EXPANDER take priority over the
    # operator's own macro definition (setf-macro.2: a DEFSETF overrides a
    # macro of the same name).
    global_env = env
    while global_env.parent is not None:
        global_env = global_env.parent
    expanders = getattr(global_env, 'setf_expanders', None)
    if expanders and op_name in expanders:
        return _expand_registered_setf(expanders[op_name], op, place_args, env)

    # CLHS 5.1.2.7 -- a macro place, including one local to a MACROLET.
    expanded, did_expand = _direct_macroexpand_1(place, env)
    if did_expand:
        return get_setf_expansion(expanded, env)

    # fclpy's SET-<name> writer convention: an operator whose write half is
    # a dedicated `SET-<op>` function (SET-READTABLE-CASE, ...) -- the same
    # lookup the ladder's struct/reader branch made, so e.g.
    # `(setf (readtable-case rt) :upcase)` works here exactly as it did
    # there. Checked for existence at expansion time; an operator without
    # one falls through to the generic `(setf fn)` fallback below.
    setter_sym = lisptype.LispSymbol(f"SET-{op_name}")
    setter_fn = global_env.find_func(setter_sym)
    if setter_fn is None or not callable(setter_fn):
        py_name = _registry.get_function_py_name(f"SET-{op_name}")
        if py_name:
            import fclpy.lispfunc as _lispfunc_mod
            setter_fn = getattr(_lispfunc_mod, py_name, None)
    if callable(setter_fn):
        temps = [_gensym_fn() for _ in place_args]
        store = _gensym_fn()
        store_form = _setf_form('PROGN',
                                _setf_form(setter_sym, *temps, store),
                                store)
        access_form = _setf_form(op, *temps)
        return temps, list(place_args), [store], store_form, access_form

    # CLHS 5.1.2.9's fallback: (setf (fn a1..an) v) => (funcall #'(setf fn) v a1..an)
    temps = [_gensym_fn() for _ in place_args]
    store = _gensym_fn()
    store_form = _setf_form('FUNCALL', _setf_form('FUNCTION', _setf_form('SETF', op)), store, *temps)
    access_form = _setf_form(op, *temps)
    return temps, list(place_args), [store], store_form, access_form


def _expand_registered_setf(entry, op, place_args, env):
    """Build a GET-SETF-EXPANSION 5-tuple from a DEFSETF / DEFINE-SETF-
    EXPANDER registration (`evaluation_core.py`'s DEFSETF/DEFINE-SETF-
    EXPANDER handlers populate `global_env.setf_expanders` with exactly
    the three shapes this switches on).
    """
    from .utilities_symbols import gensym as _gensym_fn

    etype = entry.get('type')

    if etype == 'short':
        temps = [_gensym_fn() for _ in place_args]
        store = _gensym_fn()
        store_form = _setf_form(entry['update_fn'], *(temps + [store]))
        access_form = _setf_form(op, *temps)
        return temps, list(place_args), [store], store_form, access_form

    if etype == 'long':
        # CLHS 5.1.2.2 long form: the body is executed *once per expansion*
        # with each lambda-list parameter bound to a temp *symbol* standing
        # for that argument (not its value) and each store-var bound the
        # same way -- the body's job is to produce the storing *form*,
        # which is why this, unlike the short form above, needs real code
        # execution rather than a fixed template. Reusing
        # `_create_macro_function` gets the destructuring binder, the
        # implicit BLOCK (defsetf.5's RETURN-FROM), and the &ENVIRONMENT/
        # lexical-closure behaviour (defsetf.6) for free instead of a
        # second copy of all three.
        lambda_list = entry['lambda_list']
        store_var_syms = _setf_form_args(entry['store_vars'])
        temps = [_gensym_fn() for _ in place_args]
        stores = [_gensym_fn() for _ in store_var_syms] or [_gensym_fn()]
        store_bindings = lisptype.NIL
        for sv, gs in zip(reversed(store_var_syms), reversed(stores)):
            store_bindings = lisptype.lispCons(_setf_form(sv, _setf_form('QUOTE', gs)), store_bindings)
        let_form = lisptype.lispCons(lisptype.LispSymbol('LET'),
                       lisptype.lispCons(store_bindings, entry['body']))
        wrapped_body = lisptype.lispCons(let_form, lisptype.NIL)
        expander_fn = _create_macro_function(op, lambda_list, wrapped_body, entry['env'])
        store_form = expander_fn(*(temps + [env]))
        access_form = _setf_form(op, *temps)
        return temps, list(place_args), stores, store_form, access_form

    if etype == 'expander':
        # DEFINE-SETF-EXPANDER: the lambda-list binds directly to the raw
        # (unevaluated) place-argument forms, like an ordinary macro's
        # does, and the body computes and returns the five values itself
        # (my-car/my-assoc in define-setf-expander.lsp build their own
        # temps via GENSYM) rather than having them assembled here.
        lambda_list = entry['lambda_list']
        expander_fn = _create_macro_function(op, lambda_list, entry['body'], entry['env'])
        result = expander_fn(*(list(place_args) + [env]))
        values = list(result.values) if isinstance(result, lisptype.MultipleValues) else [result]
        values += [lisptype.NIL] * (5 - len(values))
        temps = _setf_form_args(values[0]) if _consp_internal(values[0]) else []
        vals = _setf_form_args(values[1]) if _consp_internal(values[1]) else []
        stores = _setf_form_args(values[2]) if _consp_internal(values[2]) else []
        return temps, vals, stores, values[3], values[4]

    raise lisptype.LispNotImplementedError(f"SETF: unsupported expander type {etype!r}")


def _accessor_from_expansion(place, env):
    """Bridge GET-SETF-EXPANSION's form-based protocol into the (getter,
    setter) closure pair `_place_accessor`'s callers expect.
    """
    from .evaluation_core import eval as leval

    temps, vals, stores, store_form, access_form = get_setf_expansion(place, env)
    child = lisptype.Environment(parent=env)
    for t, v in zip(temps, vals):
        child.add_variable(t, leval(v, child))

    def getter():
        return leval(access_form, child)

    def setter(v):
        if len(stores) <= 1:
            if stores:
                child.add_variable(stores[0], v)
        else:
            vs = list(v.values) if isinstance(v, lisptype.MultipleValues) else [v]
            vs = vs + [lisptype.NIL] * (len(stores) - len(vs))
            for s, vv in zip(stores, vs):
                child.add_variable(s, vv)
        return leval(store_form, child)

    return getter, setter


def _place_accessor(place_form, env):
    """Evaluate a place form's shared subforms exactly once and return a
    (get, set) pair of closures for reading/writing it.

    Supports plain variables and (CAR x), (CDR x), (AREF arr idx),
    (SVREF arr idx), (GETF plist indicator [default]) place forms --
    enough for ROTATEF/SHIFTF/PUSH/PUSHNEW/INCF's common cases. Other
    place kinds raise LispNotImplementedError.
    """
    from .evaluation_core import eval

    # CLHS 5.1.2.8: a symbol place may name a symbol-macro (SYMBOL-MACROLET,
    # e.g. the bindings WITH-SLOTS/WITH-ACCESSORS establish), in which case
    # every place operator that reaches this function operates on the
    # macro's expansion rather than treating the name as an ordinary
    # variable -- the same resolution SETF/SETQ's own inline loops already
    # apply for their symbol branch (`setf-symbol-macro.*`). Without this,
    # `(incf a)` inside a WITH-ACCESSORS body silently read and wrote a
    # fresh, unrelated lexical/dynamic variable named A instead of the
    # accessor's expansion.
    while isinstance(place_form, lisptype.LispSymbol):
        expansion = env.get_symbol_macro(place_form)
        if expansion is None:
            break
        place_form = expansion

    def _setattr_return(obj, attr, v):
        """setattr returns None; every place setter here must return the
        stored value instead, since SETF's own return value (CLHS 5.1.1)
        is whatever the setter/storing-form yields, and THE/APPLY-as-
        place delegate straight to these closures."""
        setattr(obj, attr, v)
        return v

    def _setitem_return(obj, key, v):
        obj[key] = v
        return v

    if isinstance(place_form, lisptype.LispSymbol):
        sym = place_form

        def _var_setter(v):
            env.set_variable(sym, v)
            return v

        return (lambda: eval(sym, env), _var_setter)

    if _consp_internal(place_form) and isinstance(car(place_form), lisptype.LispSymbol):
        op_name = car(place_form).name
        place_args = cdr(place_form)

        if op_name == '%SPECIAL-REF' and _consp_internal(place_args):
            # A SPECIAL declaration redirects references to the variable
            # through `(%SPECIAL-REF x)` (`binding.special_reference`), so
            # every place operator -- SETQ, SETF, INCF, PUSH, ROTATEF --
            # meets the declaration as a *place*. Read and write are the
            # matched pair in `evaluation_core`, so the two cannot disagree
            # about which cell holds the value.
            from .evaluation_core import _get_special_reference, _set_special_reference
            symbol = car(place_args)
            return (lambda: _get_special_reference(symbol, env),
                    lambda v: _set_special_reference(symbol, v, env))

        if op_name in ('CAR', 'FIRST') and _consp_internal(place_args):
            target = eval(car(place_args), env)
            if not _consp_internal(target):
                raise lisptype.LispError(f"{op_name} place: target is not a cons")
            return (lambda: target.car, lambda v: _setattr_return(target, 'car', v))

        if op_name in ('CDR', 'REST') and _consp_internal(place_args):
            target = eval(car(place_args), env)
            if not _consp_internal(target):
                raise lisptype.LispError(f"{op_name} place: target is not a cons")
            return (lambda: target.cdr, lambda v: _setattr_return(target, 'cdr', v))

        if _CXR_RE.match(op_name) and _consp_internal(place_args):
            obj = eval(car(place_args), env)
            parent, is_car = _cxr_target(op_name, obj)
            if is_car:
                return (lambda: parent.car, lambda v: _setattr_return(parent, 'car', v))
            return (lambda: parent.cdr, lambda v: _setattr_return(parent, 'cdr', v))

        _NTH_ACCESSOR_INDEX = {
            'FIRST': 0, 'SECOND': 1, 'THIRD': 2, 'FOURTH': 3, 'FIFTH': 4,
            'SIXTH': 5, 'SEVENTH': 6, 'EIGHTH': 7, 'NINTH': 8, 'TENTH': 9,
        }
        if op_name in _NTH_ACCESSOR_INDEX and op_name != 'FIRST' and _consp_internal(place_args):
            lst = eval(car(place_args), env)
            n = _NTH_ACCESSOR_INDEX[op_name]
            cell = lst
            for _ in range(n):
                if not _consp_internal(cell):
                    raise lisptype.LispError(f"{op_name} place: list too short")
                cell = cdr(cell)
            if not _consp_internal(cell):
                raise lisptype.LispError(f"{op_name} place: list too short")
            return (lambda: cell.car, lambda v: _setattr_return(cell, 'car', v))

        if op_name == 'NTH' and _consp_internal(place_args) and _consp_internal(cdr(place_args)):
            n = eval(car(place_args), env)
            lst = eval(car(cdr(place_args)), env)
            cell = lst
            for _ in range(n):
                if not _consp_internal(cell):
                    raise lisptype.LispError("NTH place: index out of bounds")
                cell = cdr(cell)
            if not _consp_internal(cell):
                raise lisptype.LispError("NTH place: index out of bounds")
            return (lambda: cell.car, lambda v: _setattr_return(cell, 'car', v))

        if op_name in ('CHAR', 'SCHAR', 'ELT') and _consp_internal(place_args) and _consp_internal(cdr(place_args)):
            seq = eval(car(place_args), env)
            idx = eval(car(cdr(place_args)), env)
            _check_sequence_place_index(seq, idx, op_name)
            if _consp_internal(seq):
                cell = seq
                for _ in range(idx):
                    cell = cdr(cell)
                return (lambda: cell.car, lambda v: _setattr_return(cell, 'car', v))
            # CLHS 13.1.4: CHAR/SCHAR yield a CHARACTER, not a 1-char
            # string. `LispString.__getitem__` returns a Python `str`
            # (sequences need to be hashable etc.); a place accessor
            # must preserve the type the test harness compares against
            # -- otherwise `rotatef.18`/`rotatef.22` see `"b"` where
            # they expected `#\b`.
            if isinstance(seq, lisptype.LispString):
                def _char_get():
                    return lisptype.Character(seq[idx])
                return (_char_get, lambda v: _setitem_return(seq, idx, v))
            return (lambda: seq[idx], lambda v: _setitem_return(seq, idx, v))

        if op_name == 'SYMBOL-VALUE' and _consp_internal(place_args):
            sym = eval(car(place_args), env)
            if not isinstance(sym, lisptype.LispSymbol):
                raise lisptype.LispError("SYMBOL-VALUE place: requires a symbol")
            return (lambda: sym.value, lambda v: _setattr_return(sym, 'value', v))

        if op_name == 'SYMBOL-FUNCTION' and _consp_internal(place_args):
            sym = eval(car(place_args), env)
            if not isinstance(sym, lisptype.LispSymbol):
                raise lisptype.LispError("SYMBOL-FUNCTION place: requires a symbol")

            def _symbol_function_setter(v):
                env.add_function(sym, v)
                return v

            return (lambda: env.find_func(sym), _symbol_function_setter)

        if op_name == 'FDEFINITION' and _consp_internal(place_args):
            from .utilities_functions import _function_spec_to_key
            sym = _function_spec_to_key(eval(car(place_args), env))
            if sym is None:
                raise lisptype.LispError("FDEFINITION place: requires a function name")

            def _fdefinition_setter(v):
                env.add_function(sym, v)
                return v

            return (lambda: env.find_func(sym), _fdefinition_setter)

        if op_name == 'SYMBOL-PLIST' and _consp_internal(place_args):
            sym = eval(car(place_args), env)
            if not isinstance(sym, lisptype.LispSymbol):
                raise lisptype.LispError("SYMBOL-PLIST place: requires a symbol")
            # `LispSymbol.__init__` defaults `plist` to a bare Python `{}`,
            # not NIL -- normalize on read so a fresh symbol's plist is
            # NIL, matching what `(symbol-plist sym)` itself already
            # returns (a separate reader normalizes it there).
            return (lambda: sym.plist if _consp_internal(sym.plist) else lisptype.NIL,
                    lambda v: _setattr_return(sym, 'plist', v))

        if op_name == 'GET' and _consp_internal(place_args):
            sym = eval(car(place_args), env)
            indicator = eval(car(cdr(place_args)), env) if _consp_internal(cdr(place_args)) else lisptype.NIL
            if not isinstance(sym, lisptype.LispSymbol):
                raise lisptype.LispError("GET place: requires a symbol")

            def _get_getter():
                plist = sym.plist if _consp_internal(sym.plist) else lisptype.NIL
                cur = plist
                while _consp_internal(cur) and _consp_internal(cdr(cur)):
                    if car(cur) == indicator:
                        return cdr(cur).car
                    cur = cdr(cdr(cur))
                return lisptype.NIL

            def _get_setter(v):
                if not _consp_internal(sym.plist):
                    sym.plist = lisptype.NIL
                cur = sym.plist
                while _consp_internal(cur) and _consp_internal(cdr(cur)):
                    if car(cur) == indicator:
                        cdr(cur).car = v
                        return v
                    cur = cdr(cdr(cur))
                sym.plist = lisptype.lispCons(indicator, lisptype.lispCons(v, sym.plist))
                return v

            return (_get_getter, _get_setter)

        if op_name == 'GETHASH' and _consp_internal(place_args) and _consp_internal(cdr(place_args)):
            # Both halves go through the hash table model: `table.get(...)`
            # and `table[key] = v` were Python dict operations that ignored
            # the table's test entirely.
            from .misc_hashtables import puthash, gethash as _gethash
            key = eval(car(place_args), env)
            table = eval(car(cdr(place_args)), env)
            return (lambda: lisptype.primary_value(_gethash(key, table)),
                    lambda v: puthash(key, table, v))

        if op_name == 'FIND-CLASS' and _consp_internal(place_args):
            import fclpy.classes as _classes
            place_name = eval(car(place_args), env)
            if not isinstance(place_name, lisptype.LispSymbol):
                raise lisptype.LispError("FIND-CLASS place: name must be a symbol")

            def _find_class_getter():
                found = _classes.find_class(place_name)
                return found if found is not None else lisptype.NIL

            def _find_class_setter(v):
                # (SETF/ROTATEF/... (FIND-CLASS name) class) registers `v`
                # under `name` as an alias -- CLHS: it does not rename the
                # class, it adds another name for it. `v` of NIL instead
                # means `place_name` no longer denotes a class at all
                # (CLHS 7.7) -- the other half of this place, alongside
                # `evaluation_core.py`'s copy of the same two branches.
                if isinstance(v, _classes.LispClass):
                    _classes.register_class_as(place_name, v)
                elif _null_internal(v):
                    _classes.unregister_class_as(place_name)
                else:
                    raise lisptype.LispError("FIND-CLASS place: value must be a class or NIL")
                return v

            return (_find_class_getter, _find_class_setter)

        if op_name == 'MACRO-FUNCTION' and _consp_internal(place_args):
            sym = eval(car(place_args), env)
            if not isinstance(sym, lisptype.LispSymbol):
                raise lisptype.LispError("MACRO-FUNCTION place: requires a symbol")

            def _macro_function_setter(v):
                return _fclpy_setf_macro_function(sym, v)

            return (lambda: env.find_func(sym), _macro_function_setter)

        if op_name == 'LDB' and _consp_internal(place_args) and _consp_internal(cdr(place_args)):
            # (LDB bytespec place) -- CLHS 22.1.3; `(byte size position)`
            # is a plain (size, position) Python tuple here (`math_arithmetic.byte_fn`).
            # Read and write go through the real LDB/DPB functions rather
            # than a second copy of their arithmetic -- the form half of
            # this place (`get_setf_expansion`'s LDB branch) generates
            # calls to the same two functions, so the two faces of the
            # protocol cannot disagree.
            size, pos = eval(car(place_args), env)
            inner_getter, inner_setter = _place_accessor(car(cdr(place_args)), env)

            def _ldb_getter():
                from .math_arithmetic import ldb as _ldb_fn
                return _ldb_fn((size, pos), inner_getter())

            def _ldb_setter(v):
                from .math_arithmetic import dpb as _dpb_fn
                inner_setter(_dpb_fn(v, (size, pos), inner_getter()))
                return v

            return (_ldb_getter, _ldb_setter)

        if op_name == 'MASK-FIELD' and _consp_internal(place_args) and _consp_internal(cdr(place_args)):
            # (MASK-FIELD bytespec place) -- like LDB but the field keeps
            # its original bit position instead of being shifted down; the
            # write is DEPOSIT-FIELD's own computation.
            size, pos = eval(car(place_args), env)
            inner_getter, inner_setter = _place_accessor(car(cdr(place_args)), env)

            def _mask_field_getter():
                from .math_arithmetic import mask_field as _mask_field_fn
                return _mask_field_fn((size, pos), inner_getter())

            def _mask_field_setter(v):
                from .math_arithmetic import deposit_field as _deposit_field_fn
                inner_setter(_deposit_field_fn(v, (size, pos), inner_getter()))
                return v

            return (_mask_field_getter, _mask_field_setter)

        if op_name == 'SUBSEQ' and _consp_internal(place_args):
            # (SETF (SUBSEQ seq start [end]) new-seq) -- CLHS 17.1: copies
            # elements from new-seq into seq in place, only as many as fit
            # between start and end (or seq's own length); it does not
            # resize seq. The *read* side (needed for ROTATEF/PSETF/SHIFTF,
            # which all treat a place as get-then-set) is the ordinary
            # SUBSEQ function -- the window itself, not all of `seq`.
            seq = eval(car(place_args), env)
            rest = cdr(place_args)
            start = eval(car(rest), env) if _consp_internal(rest) else 0
            end_args = cdr(rest) if _consp_internal(rest) else lisptype.NIL
            end = eval(car(end_args), env) if _consp_internal(end_args) else None

            def _subseq_getter():
                from .sequences_compose import subseq as _subseq_fn
                return _subseq_fn(seq, start, end)

            def _subseq_getter():
                from .sequences_compose import subseq as _subseq_fn
                return _subseq_fn(seq, start, end)

            def _subseq_setter(new_seq):
                return _fclpy_setf_subseq(seq, start, end, new_seq)

            return (_subseq_getter, _subseq_setter)

        if op_name == 'GETF' and _consp_internal(place_args):
            # (GETF plist indicator [default]) as a place, CLHS 5.1.2.6:
            # the plist is itself a nested place (recurse through
            # `_place_accessor` rather than assuming a bare variable), and
            # writing either mutates an existing indicator's value cell in
            # place or prepends a fresh (indicator value) pair and writes
            # the new head back through the plist's own place.
            # CLHS 5.1.3: a place's subforms are evaluated exactly once,
            # left to right -- the plist place itself, then the indicator,
            # then the default -- before the new-value form. This used to
            # evaluate indicator and default *before* the place's own
            # subforms, which `setf-getf.order.2` observes directly via a
            # counter incremented in each subform.
            plist_place = car(place_args)
            plist_getter, plist_setter = _place_accessor(plist_place, env)
            indicator = eval(car(cdr(place_args)), env)
            default_args = cdr(cdr(place_args))
            default = eval(car(default_args), env) if _consp_internal(default_args) else lisptype.NIL

            def _getf_get():
                current = plist_getter()
                while _consp_internal(current) and _consp_internal(cdr(current)):
                    if car(current) == indicator:
                        return cdr(current).car
                    current = cdr(cdr(current))
                return default

            def _getf_set(v):
                plist = plist_getter()
                current = plist
                while _consp_internal(current) and _consp_internal(cdr(current)):
                    if car(current) == indicator:
                        cdr(current).car = v
                        return v
                    current = cdr(cdr(current))
                plist_setter(lisptype.lispCons(indicator, lisptype.lispCons(v, plist)))
                return v

            return (_getf_get, _getf_set)

        if _arrays.is_array_place(op_name) and _consp_internal(place_args):
            from .evaluation_core import _eval_args
            values = _eval_args(place_args, env)

            def _array_place_setter(v):
                _arrays.array_place_write(op_name, values, v)
                return v

            return (lambda: _arrays.array_place_read(op_name, values), _array_place_setter)

        if op_name == 'VALUES':
            # CLHS 5.1.2.3: a place naming several subplaces at once --
            # each subplace's own subforms are resolved left to right here
            # (this list comprehension), before the caller ever evaluates
            # the value-form, which is what `setf-values.5` observes.
            sub_accessors = [_place_accessor(p, env) for p in _setf_form_args(place_args)]

            def _values_getter():
                return lisptype.MultipleValues(*[g() for g, _ in sub_accessors])

            def _values_setter(v):
                vs = list(v.values) if isinstance(v, lisptype.MultipleValues) else [v]
                vs = vs + [lisptype.NIL] * (len(sub_accessors) - len(vs))
                for (_, s), vv in zip(sub_accessors, vs):
                    s(vv)
                return v

            return (_values_getter, _values_setter)

        if op_name == 'THE' and _consp_internal(place_args) and _consp_internal(cdr(place_args)):
            # CLHS 5.1.2.4: the type assertion has no bearing on where the
            # value is stored or read from.
            return _place_accessor(car(cdr(place_args)), env)

        if op_name == 'APPLY':
            rewritten = _rewrite_setf_apply(_setf_form_args(place_args))
            if rewritten is not None:
                return _place_accessor(rewritten, env)
            # Falls through to the generic expansion below, which raises
            # LispNotImplementedError for the non-literal-spread shape.

        # Not a place op this function knows directly as a closure -- a
        # user DEFSETF/DEFINE-SETF-EXPANDER expander, a macro place
        # (including one local to a MACROLET, e.g. ansi-aux's
        # `expand-in-current-env`), or CLHS 5.1.2.9's generic function-call
        # fallback all share the one form-based protocol, GET-SETF-
        # EXPANSION, rather than a second ladder duplicating it here.
        return _accessor_from_expansion(place_form, env)

    raise lisptype.LispNotImplementedError(f"place not supported: {place_form}")


def eval_define_modify_macro(form, env):
    """Evaluate DEFINE-MODIFY-MACRO special form.

    (DEFINE-MODIFY-MACRO name lambda-list function [documentation])
    defines `name` as a macro such that `(name place arg*)` reads place's
    old value, applies `function` to it and the (evaluated-later) `arg*`
    forms, and stores the result back -- CLHS 5.1.3. Built directly on
    `get_setf_expansion` rather than a bespoke place ladder, since a
    modify-macro is nothing but SETF's own "read, combine, store" pattern
    with `function` supplying the combine step.
    """
    args = cdr(form)
    if not _consp_internal(args) or not _consp_internal(cdr(args)) or not _consp_internal(cdr(cdr(args))):
        raise lisptype.LispNotImplementedError(
            "DEFINE-MODIFY-MACRO requires a name, a lambda-list and a function")

    name = car(args)
    lambda_list = car(cdr(args))
    function_name = car(cdr(cdr(args)))
    doc_rest = cdr(cdr(cdr(args)))
    doc_string = None
    if _consp_internal(doc_rest):
        d = car(doc_rest)
        if isinstance(d, (str, lisptype.LispString)):
            doc_string = str(d)

    if not isinstance(name, lisptype.LispSymbol):
        raise lisptype.LispNotImplementedError("DEFINE-MODIFY-MACRO: name must be a symbol")

    from .evaluation_core import parse_lambda_list, bind_destructuring_pattern, eval as leval
    from .utilities_symbols import gensym as _gensym_fn

    parsed = parse_lambda_list(lambda_list)
    required_params = parsed.get('required', [])
    optional_params = parsed.get('optional', [])
    rest_param = parsed.get('rest', None)
    def_env = env

    def macro_callable(*call_args):
        expansion_env = def_env
        if call_args and isinstance(call_args[-1], lisptype.Environment):
            expansion_env = call_args[-1]
            call_args = call_args[:-1]
        if not call_args:
            raise lisptype.LispNotImplementedError(f"{name.name}: requires a place argument")

        place = call_args[0]
        rest_args = call_args[1:]
        macro_env = lisptype.Environment(parent=def_env)

        extra_forms = []
        idx = 0
        for p in required_params:
            val = rest_args[idx] if idx < len(rest_args) else lisptype.NIL
            bind_destructuring_pattern(p, val, macro_env)
            extra_forms.append(val)
            idx += 1
        for p in optional_params:
            if _consp_internal(p):
                pname = car(p)
                prest = cdr(p)
                pdefault = car(prest) if _consp_internal(prest) else None
            else:
                pname, pdefault = p, None
            if idx < len(rest_args):
                val = rest_args[idx]
                idx += 1
            else:
                val = leval(pdefault, macro_env) if pdefault is not None else lisptype.NIL
            bind_destructuring_pattern(pname, val, macro_env)
            extra_forms.append(val)
        if rest_param is not None:
            remaining = list(rest_args[idx:])
            bind_destructuring_pattern(rest_param, _setf_pylist_to_form(remaining), macro_env)
            extra_forms.extend(remaining)

        temps, vals, stores, store_form, access_form = get_setf_expansion(place, expansion_env)
        store = stores[0] if stores else _gensym_fn()
        call_form = _setf_form(function_name, access_form, *extra_forms)
        bindings = list(zip(temps, vals)) + [(store, call_form)]
        return _make_let_star(bindings, store_form)

    macro_callable.__is_macro__ = True
    macro_callable.__expects_environment__ = True

    if doc_string:
        if not hasattr(name, 'plist'):
            name.plist = {}
        name.plist['DOCUMENTATION'] = doc_string
        # Also on the callable itself, so `(documentation (macro-function sym)
        # t)` -- the *function object*, not the symbol -- can read it
        # (CLHS 25.1.3; documentation.function.t.4).
        macro_callable.__doc__ = str(doc_string)

    global_env = env
    while global_env.parent is not None:
        global_env = global_env.parent
    global_env.add_function(name, macro_callable)
    if env is not global_env:
        env.add_function(name, macro_callable)

    return name


def _make_let_star(bindings, body_form):
    """Build (LET* ((v1 f1) (v2 f2) ...) body-form), unevaluated."""
    binding_forms = _setf_pylist_to_form([_setf_form(v, f) for v, f in bindings])
    return _setf_form('LET*', binding_forms, body_form)


def eval_defclass(form, env):
    """Evaluate DEFCLASS special form.
    
    DEFCLASS defines a new class (CLOS). The name should not be evaluated,
    but superclasses should be looked up as class objects.
    
    Syntax:
        (defclass name (superclass*) (slot-spec*) option*)
    
    Slot specs:
        (slot-name [:initarg keyword] [:initform form] [:reader name] [:writer name])
    
    Options:
        (:metaclass class)
        (:documentation string)
        (:default-initargs initarg-name form*)
    """
    from .evaluation_core import eval
    import fclpy.classes
    
    # Import the defclass function directly
    from fclpy.lispfunc.classes import defclass
    
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("DEFCLASS requires at least a name and superclass list")
    
    # Get class name (NOT evaluated - it's a symbol to define)
    class_name = car(args)
    if not isinstance(class_name, lisptype.LispSymbol):
        raise lisptype.LispNotImplementedError("DEFCLASS: class name must be a symbol")
    
    # Get superclasses list
    rest = cdr(args)
    if not _consp_internal(rest):
        raise lisptype.LispNotImplementedError("DEFCLASS requires a superclass list")
    
    superclasses_form = car(rest)
    slots_rest = cdr(rest)
    
    # Parse superclasses: convert symbols to class objects
    superclasses_list = []
    if superclasses_form is not None and superclasses_form != lisptype.NIL:
        current = superclasses_form
        while _consp_internal(current):
            sc = car(current)
            if isinstance(sc, lisptype.LispSymbol):
                # CLHS 4.3.7: a superclass that is not defined yet is legal --
                # it becomes a *forward-referenced class*, a real registered
                # class object standing in for the name until some later
                # DEFCLASS fills it in. This used to append the bare symbol,
                # so `direct_superclasses` was part class objects and part
                # symbols and nothing ever resolved the difference.
                superclasses_list.append(
                    fclpy.classes.ensure_forward_referenced_class(sc))
            else:
                superclasses_list.append(sc)
            current = cdr(current)
    
    # Get slot definitions
    slots_form = None
    if _consp_internal(slots_rest):
        slots_form = car(slots_rest)
    
    options_rest = cdr(slots_rest) if _consp_internal(slots_rest) else lisptype.NIL
    
    # Parse slot definitions - convert to format expected by defclass function
    slots_list = []
    if slots_form is not None and slots_form != lisptype.NIL:
        current = slots_form
        while _consp_internal(current):
            slot_spec = car(current)
            # Slot spec can be just a symbol or a list
            if isinstance(slot_spec, lisptype.LispSymbol):
                # Simple slot: just a symbol
                slots_list.append(slot_spec)
            elif _consp_internal(slot_spec):
                # Complex slot: (name :key1 val1 :key2 val2 ...)
                # Convert to list format: [name_symbol, :key1, val1, :key2, val2, ...]
                slot_list = []
                slot_spec_current = slot_spec
                while _consp_internal(slot_spec_current):
                    element = car(slot_spec_current)
                    slot_list.append(element)
                    slot_spec_current = cdr(slot_spec_current)
                slots_list.append(slot_list)
            current = cdr(current)
    
    # Parse options
    metaclass = None
    documentation = None
    default_initargs = []
    # CLHS 7.7: each class option may appear at most once, every option
    # must be a known keyword, and :default-initargs' initarg names must be
    # distinct -- violations are a program-error (ansi-test's
    # defclass-errors.lsp pins all three).
    _DEFCLASS_CLASS_OPTIONS = {'metaclass', 'documentation', 'default-initargs'}
    seen_class_options = set()
    seen_default_initargs = set()
    if _consp_internal(options_rest):
        current = options_rest
        while _consp_internal(current):
            option = car(current)
            if not _consp_internal(option):
                raise lisptype.LispProgramError(
                    f"DEFCLASS: invalid class option: {option}")
            opt_key = car(option)
            if not isinstance(opt_key, lisptype.lispKeyword):
                raise lisptype.LispProgramError(
                    f"DEFCLASS: class option name must be a keyword, got {opt_key}")
            opt_name = opt_key.name.lower()
            if opt_name not in _DEFCLASS_CLASS_OPTIONS:
                raise lisptype.LispProgramError(
                    f"DEFCLASS: unrecognized class option :{opt_name.upper()}")
            if opt_name in seen_class_options:
                raise lisptype.LispProgramError(
                    f"DEFCLASS: duplicate class option :{opt_name.upper()}")
            seen_class_options.add(opt_name)
            if isinstance(opt_key, lisptype.lispKeyword):
                    opt_name = opt_key.name.lower()
                    opt_vals = cdr(option)
                    if opt_name == 'metaclass' and _consp_internal(opt_vals):
                        metaclass = car(opt_vals)
                    elif opt_name == 'documentation' and _consp_internal(opt_vals):
                        documentation = car(opt_vals)
                    elif opt_name == 'default-initargs':
                        # CLHS 7.1.8: an alternating initarg-name/form plist,
                        # stored unevaluated -- each default-value-form is
                        # evaluated fresh by MAKE-INSTANCE, only when the
                        # initarg it names was not supplied to that call.
                        dinit_current = opt_vals
                        while _consp_internal(dinit_current):
                            initarg_key = car(dinit_current)
                            dinit_rest = cdr(dinit_current)
                            if not _consp_internal(dinit_rest):
                                raise lisptype.LispProgramError(
                                    "DEFCLASS: :default-initargs must be an "
                                    "even-length initarg/form plist")
                            if not isinstance(initarg_key, lisptype.lispKeyword):
                                raise lisptype.LispProgramError(
                                    f"DEFCLASS: :default-initargs name must be a "
                                    f"keyword, got {initarg_key}")
                            if initarg_key in seen_default_initargs:
                                raise lisptype.LispProgramError(
                                    f"DEFCLASS: duplicate default initarg {initarg_key}")
                            seen_default_initargs.add(initarg_key)
                            initarg_form = car(dinit_rest)
                            default_initargs.append((initarg_key, initarg_form))
                            dinit_current = cdr(dinit_rest)
            current = cdr(current)
    
    # CLHS 4.3.7: a built-in class cannot be subclassed with defclass --
    # "Attempting to use defclass to define subclasses of a built-in-class
    # signals an error" -- nor redefined by naming it (defclass.error.23
    # and .24 walk *built-in-classes* asserting exactly this for every
    # class whose metaclass is built-in-class). Classes with the other
    # metaclasses (standard-class, structure-class) remain subclassable.
    for sc in superclasses_list:
        if (isinstance(sc, fclpy.classes.LispClass)
                and getattr(sc, 'metaclass_name', 'STANDARD-CLASS') == 'BUILT-IN-CLASS'):
            raise lisptype.LispProgramError(
                f"DEFCLASS: cannot define a subclass of the built-in class "
                f"{sc.name_string}")
    existing_named = fclpy.classes.find_class(
        class_name.name if isinstance(class_name, lisptype.LispSymbol) else str(class_name))
    if (existing_named is not None
            and getattr(existing_named, 'metaclass_name', 'STANDARD-CLASS') == 'BUILT-IN-CLASS'):
        raise lisptype.LispProgramError(
            f"DEFCLASS: {class_name} names a built-in class and cannot be redefined")

    # Call the defclass function to create the class. `definition_env` is
    # threaded through so a slot's :initform is later evaluated where this
    # DEFCLASS lexically appeared (CLHS 7.1.2), and so :reader/:writer/
    # :accessor generic functions are bound in the right global environment
    # rather than whatever `state.current_environment` happens to hold.
    result = defclass(
        class_name,
        direct_superclasses=superclasses_list,
        slots=slots_list,
        metaclass=metaclass,
        documentation=documentation,
        definition_env=env,
        default_initargs=default_initargs,
    )

    # CLHS 7.7: DEFCLASS returns the class object `defclass()` just built
    # and registered, not the name -- see the fuller note at its own
    # `return` statement.
    return result


def _resolve_specializer(param_type, env):
    """Resolve one specialized-lambda-list parameter's type into a dispatch
    specializer (CLHS 7.6.2): a `classes.LispClass`, a `classes.EqlSpecializer`
    (its form is evaluated once, here, not on every dispatch), or None for
    T/unspecialized. A type-name symbol that names no modeled class (e.g.
    INTEGER has a class, but not every CLHS type does) is returned as-is;
    `classes._arg_matches_specializer` asks TYPEP for those, so any CLHS
    type name works as a specializer even though only DEFCLASS-defined and
    the built-in classes are modeled as class objects.
    """
    import fclpy.classes as classes
    if isinstance(param_type, lisptype.LispSymbol):
        if param_type.name.upper() == 'T':
            return None
        found = classes.find_class(param_type.name)
        return found if found is not None else param_type
    if _consp_internal(param_type):
        head = car(param_type)
        if isinstance(head, lisptype.LispSymbol) and head.name.upper() == 'EQL':
            from .evaluation_core import eval as _eval
            value = _eval(car(cdr(param_type)), env)
            return classes.EqlSpecializer(value)
    return param_type


def _parse_defmethod_tail(rest):
    """Parse the (qualifier* specialized-lambda-list . body) tail shared by
    DEFMETHOD and DEFGENERIC's inline :method option (CLHS 7.6.2, 7.7).

    A qualifier is any non-NIL atom, not only a keyword: :before/:after/
    :around happen to be keywords by convention for *standard* method
    combination, but a built-in short-form combination's qualifier is an
    ordinary symbol -- `(:method and ((x integer)) ...)` from CLHS 7.6.6's
    AND/OR/PROGN/+/APPEND/NCONC/LIST/MAX/MIN combinations qualifies with
    the bare symbol `AND`, not `:AND`. Treating only keywords as
    qualifiers left that plain symbol as the "specialized lambda list"
    instead, which is never a cons, so parsing it found no parameters at
    all and fed the *real* lambda list in as the first body form -- where
    it evaluated as a function call and raised "Undefined function" for
    whatever symbol happened to be the parameter name.
    """
    qualifiers = []
    while _consp_internal(rest):
        first = car(rest)
        if _consp_internal(first) or first is lisptype.NIL or isinstance(first, lisptype.lispNull):
            break
        qualifiers.append(first)
        rest = cdr(rest)
    if not _consp_internal(rest):
        raise lisptype.LispNotImplementedError("DEFMETHOD requires a specialized lambda list")
    specialized_lambda_list = car(rest)
    body = cdr(rest)
    return qualifiers, specialized_lambda_list, body


def _parse_specialized_lambda_list(specialized_lambda_list, env):
    """Parse a specialized-lambda-list (CLHS 7.6.2) into
    `(required_params, specializers, optional_lambda_list)`: only required
    parameters may be specialized, so everything from the first lambda-list
    keyword (&optional/&rest/&key/&aux) onward is an *ordinary* lambda-list
    tail -- parsed here by the same `parse_lambda_list` DEFUN uses, and
    later bound by the same `_bind_ordinary_lambda_list_tail` (CLHS 3.4.1).

    This replaced flattening that tail to bare parameter names bound
    positionally with a NIL fallback: it silently discarded every
    &optional/&key default form, never bound a supplied-p variable at all
    (`Unbound variable` the first time a method body read one), and did not
    support &rest/&key/&aux in a method lambda list at all -- ansi-test's
    slot-missing.lsp methods all declare `&optional (new-value nil
    new-value-p)`.
    """
    from .evaluation_core import parse_lambda_list

    required_params = []
    specializers = []
    current = specialized_lambda_list
    while _consp_internal(current):
        param_spec = car(current)
        if isinstance(param_spec, lisptype.LispSymbol) and param_spec.name.startswith('&'):
            break
        if _consp_internal(param_spec):
            param_name = car(param_spec)
            param_type = car(cdr(param_spec))
            required_params.append(param_name)
            specializers.append(_resolve_specializer(param_type, env))
        else:
            required_params.append(param_spec)
            specializers.append(None)
        current = cdr(current)

    optional_lambda_list = parse_lambda_list(current)
    return required_params, specializers, optional_lambda_list


def _lambda_list_param_name(param_spec):
    """The bindable variable name from one &optional/&key/&aux parameter
    spec (CLHS 3.4.1): a bare symbol, `(var [default [supplied-p]])`, or
    &key's `((keyword var) default...)`."""
    if isinstance(param_spec, lisptype.LispSymbol):
        return param_spec
    if _consp_internal(param_spec):
        head = car(param_spec)
        if _consp_internal(head):
            return car(cdr(head))
        return head
    return param_spec


def _make_method_function(required_params, optional_lambda_list, body, captured_env, block_name):
    """Build the callable behind one CLOS method (shared by DEFGENERIC's inline
    :method options and standalone DEFMETHOD -- these used to be two copies of
    identical logic).

    `required_params` bind positionally (CLHS 7.6.2: dispatch already
    checked their specializers). `optional_lambda_list` -- the parsed
    &optional/&rest/&key/&aux tail, ordinary per that same section -- binds
    through `_bind_ordinary_lambda_list_tail`, the same CLHS 3.4.1 mechanism
    DEFUN uses, rather than a second copy that only this method-function
    path had.

    CLHS 7.6.5: each method has an implicit block named after its generic
    function, so a bare (RETURN-FROM gf-name ...) inside the method body
    returns from the method rather than escaping further out.
    """
    from .evaluation_loops_conditionals import _implicit_block_frame
    from .evaluation_core import eval
    from .binding import BindingFrame

    _docstring, declarations, forms = split_function_body(body)
    declaration_body = lisptype.NIL
    for decl in reversed(declarations):
        declaration_body = lisptype.lispCons(decl, declaration_body)
    parameters = list(required_params) + _lambda_list_variables(optional_lambda_list)

    n_required = len(required_params)
    n_optional = len(optional_lambda_list.get('optional', []))
    # &optional bounds the argument count (checked below); only &rest/&key
    # make it unbounded (&key's even-pair structure is the binder's own
    # CLHS 3.4.1.4 check).
    accepts_extra = bool(optional_lambda_list.get('mentions_rest')
                         or optional_lambda_list.get('mentions_key')
                         or optional_lambda_list.get('allow_other_keys'))

    def method_func(*call_args):
        from .evaluation_core import _enter_lisp_call, _leave_lisp_call, ConditionException
        if _enter_lisp_call(block_name or 'method'):
            raise ConditionException(
                lisptype.StorageCondition(
                    message=f"Stack overflow in a method of {block_name}: "
                            f"Lisp recursion exceeded the available stack"),
                recoverable=False)
        try:
            # CLHS 3.5.1: a wrong argument count is a PROGRAM-ERROR -- the
            # dispatch above only checks specializer applicability, not arity,
            # so a method whose lambda list is `(x)` called with two arguments
            # used to silently drop the surplus (defmethod.error.13,
            # make-load-form.error.2).
            if len(call_args) < n_required:
                raise lisptype.LispProgramError(
                    f"method takes {n_required} required argument(s), got {len(call_args)}")
            if not accepts_extra and len(call_args) > n_required + n_optional:
                raise lisptype.LispProgramError(
                    f"method takes {n_required + n_optional} argument(s), got {len(call_args)}")
            method_env = lisptype.Environment(captured_env)
            # A method's parameters obey the same CLHS 3.4.1/11.1.2.1.2 rule as
            # any other function's: one declared SPECIAL binds dynamically and
            # is undone on exit. `BindingFrame` is the one place that decides which.
            frame = BindingFrame(method_env, body=declaration_body,
                                 bound_vars=parameters,
                                 defer_free_declarations=True)
            try:
                arg_index = 0
                for param in required_params:
                    if arg_index < len(call_args):
                        frame.bind(param, call_args[arg_index])
                        arg_index += 1
                    else:
                        frame.bind(param, lisptype.NIL)

                _bind_ordinary_lambda_list_tail(
                    optional_lambda_list, call_args, arg_index, method_env, eval, frame)
                frame.install_free_declarations()

                # CALL-NEXT-METHOD and NEXT-METHOD-P have *indefinite extent*
                # (CLHS 7.6.6.2): a method may return the function itself --
                # `(defmethod f (x) #'call-next-method)` -- and the caller may
                # FUNCALL it long after the method finished. What it closes
                # over is the frame `classes.call_method` pushed for this very
                # invocation; binding them as local functions of the method's
                # environment (which is what a FLET would do) is what makes
                # both the bare-operator and the #'-quoted uses resolve to the
                # frame-capturing closures instead of the frame-less global
                # operator.
                cnm_frame = _clos_classes.current_call_frame()
                if cnm_frame is not None:
                    captured = cnm_frame

                    def _local_call_next_method(*cnm_args):
                        return _clos_classes.call_next_method_in_frame(captured, *cnm_args)

                    def _local_next_method_p():
                        return lisptype.lisp_bool(bool(captured['next']))

                    method_env.add_function(
                        lisptype.intern_symbol('CALL-NEXT-METHOD'),
                        _local_call_next_method)
                    method_env.add_function(
                        lisptype.intern_symbol('NEXT-METHOD-P'),
                        _local_next_method_p)

                # The body loop runs directly in this frame, and the implicit
                # block via `_implicit_block_frame`, whose `with` holds no frame
                # while the body runs -- the same flattening
                # `make_ordinary_function` got (recursion-plan.md Step 3). A
                # recursive generic function (`is-similar*` in the ansi-test
                # helpers) pays these frames per level; the old
                # `_run_with_nil_block(_run_body, ...)` shape cost two.
                blk = _implicit_block_frame(block_name, method_env)
                with blk:
                    result = lisptype.NIL
                    body_current = forms
                    while _consp_internal(body_current):
                        result = eval(car(body_current), method_env)
                        body_current = cdr(body_current)
                if blk.caught:
                    return blk.value
                return result
            finally:
                frame.unwind()
        finally:
            _leave_lisp_call()
    # CLHS 7.6.2: a method body may open with a documentation string.
    # `split_function_body` already extracted it -- attach it to the callable
    # so `eval_defmethod` can store it on the Method object for
    # `(documentation method t)` (CLHS 25.1.3) instead of discarding it.
    method_func.__method_docstring__ = _docstring
    return method_func


def _resolve_method_combination(func_name, option_tail):
    """DEFGENERIC's `(:method-combination name option*)`: none of it is
    evaluated. The name must already denote a combination type -- CLHS
    requires an error otherwise, and silently falling back to standard
    combination is what made every `(:method-combination progn)` generic
    function drop its `progn`-qualified methods."""
    import fclpy.classes as classes

    if not _consp_internal(option_tail):
        raise lisptype.LispProgramError(
            f"DEFGENERIC {func_name.name}: :METHOD-COMBINATION requires a name")
    name = car(option_tail)
    combination_type = classes.find_method_combination_type(name)
    if combination_type is None:
        raise lisptype.LispProgramError(
            f"DEFGENERIC {func_name.name}: {name} does not name a method combination type")
    return classes.MethodCombination(combination_type, _list_elements(cdr(option_tail)))


def _check_argument_precedence_order(func_name, lambda_list, order_tail):
    """CLHS 7.7: :ARGUMENT-PRECEDENCE-ORDER must name each required
    parameter of the generic function's lambda list exactly once.

    Only the *validation* is here. Dispatching in that order is a change to
    method specificity ordering (`classes._specificity_key`), which now has
    a real class precedence list to rank against but always compares
    parameter positions in the generic function's declared left-to-right
    order, never the permutation this option validates -- see plan.md's
    discovered issues rather than adding a second ordering mechanism beside
    it.
    """
    required = []
    for param in _list_elements(lambda_list):
        if isinstance(param, lisptype.LispSymbol) and param.name.startswith('&'):
            break
        required.append(_lambda_list_param_name(param))

    given = _list_elements(order_tail)
    given_names = [p.name for p in given if isinstance(p, lisptype.LispSymbol)]
    required_names = [p.name for p in required if isinstance(p, lisptype.LispSymbol)]

    if sorted(given_names) != sorted(required_names) or len(set(given_names)) != len(given_names):
        raise lisptype.LispProgramError(
            f"DEFGENERIC {func_name.name}: :ARGUMENT-PRECEDENCE-ORDER {given_names} "
            f"is not a permutation of the required parameters {required_names}")


def _congruence_shape(required_count, tail):
    """The four congruence-relevant facts of one lambda list (CLHS 7.6.4),
    from a required-parameter count and the parsed `&optional`/`&rest`/
    `&key`/`&allow-other-keys` tail `parse_lambda_list` already produces.

    `keywords` is `None` when `&key` was not mentioned at all -- distinct
    from mentioning it with no names -- which is exactly what rule 3 below
    needs and a bare `len(tail['keyword'])` cannot tell apart.
    """
    mentions_rest_or_key = tail['mentions_rest'] or tail['mentions_key']
    keywords = None
    if tail['mentions_key']:
        keywords = {keyword_argument_key(_keyword_param_parts(k)[0])
                    for k in tail['keyword']}
    return required_count, len(tail['optional']), mentions_rest_or_key, keywords, tail['allow_other_keys']


def _check_method_congruent(gf_name, gf_lambda_list, required_params, optional_lambda_list):
    """CLHS 7.6.4, "Congruent Lambda-lists for all Methods of a Generic
    Function": every method added to a generic function -- whether via
    DEFGENERIC's inline `:method` options or a standalone DEFMETHOD --
    must agree with the generic function's own lambda list on the number
    of required parameters, the number of optional parameters, and whether
    `&rest`/`&key` is accepted at all; if the generic function's lambda
    list names `&key` arguments, every method must accept all of them
    (by naming them itself, or via `&allow-other-keys`).

    This was entirely unchecked: `(defgeneric g (x) (:method ((x t) (y t))
    ...))` silently added a two-argument method to a one-argument generic
    function instead of signalling the PROGRAM-ERROR CLHS requires
    (`defgeneric.error.9` through `.19`, and `.7`'s inline-:method case).
    """
    from .evaluation_core import parse_lambda_list

    gf_tail = parse_lambda_list(gf_lambda_list)
    gf_req, gf_opt, gf_rk, gf_keys, _ = _congruence_shape(len(gf_tail['required']), gf_tail)
    m_req, m_opt, m_rk, m_keys, m_aok = _congruence_shape(len(required_params), optional_lambda_list)

    name = gf_name.name if isinstance(gf_name, lisptype.LispSymbol) else str(gf_name)

    if m_req != gf_req:
        raise lisptype.LispProgramError(
            f"{name}: method has {m_req} required parameter(s); "
            f"the generic function has {gf_req}")
    if m_opt != gf_opt:
        raise lisptype.LispProgramError(
            f"{name}: method has {m_opt} optional parameter(s); "
            f"the generic function has {gf_opt}")
    if m_rk != gf_rk:
        raise lisptype.LispProgramError(
            f"{name}: method and generic function lambda lists must agree "
            f"on whether &rest or &key is accepted")
    if gf_keys is not None and m_keys is not None and not m_aok:
        missing = gf_keys - m_keys
        if missing:
            raise lisptype.LispProgramError(
                f"{name}: method does not accept keyword argument(s) "
                f"{sorted(name for _package, name in missing)} "
                f"named by the generic function's lambda list")


def eval_defgeneric(form, env):
    """Evaluate DEFGENERIC special form (CLHS 7.7).

    Builds directly on `fclpy.classes`'s one generic-function/method
    mechanism (`ensure_generic_function`/`add_method`) instead of rolling a
    second, disconnected GenericFunction implementation -- which is what
    used to make CALL-NEXT-METHOD inside a DEFMETHOD-defined method raise
    "No next method available" unconditionally: nothing populated the
    next-method context `classes.call_next_method` reads, because the
    dispatcher that actually ran the method never called into
    `classes.call_generic_function` at all (plan.md Finding L).

    Syntax:
        (defgeneric name lambda-list [[option | method-description]]*)

    Supported options:
        (:method qualifiers* specialized-lambda-list body)
        (:documentation string)
        (:method-combination name option*)
        (:argument-precedence-order parameter*)
        (:generic-function-class class) / (:method-class class) / (declare ...)

    An option this implementation does not act on is still *checked*: CLHS
    7.7 requires a PROGRAM-ERROR for an unknown option or a repeated
    :DOCUMENTATION, and the loop this replaced dropped every option but
    :METHOD and :DOCUMENTATION on the floor without a word -- which is how
    `(:method-combination progn)` silently got standard combination and
    every method qualified `progn` then failed to match any of it
    (standing rule 4).
    """
    import fclpy.classes as classes

    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispProgramError("DEFGENERIC requires at least a name and lambda-list")

    func_name = car(args)
    rest = cdr(args)

    # CLHS 7.7: function-name is a symbol or a (SETF symbol) list -- the
    # same designator DEFUN and DEFMETHOD accept, and which EGF-FUN-14's
    # `(defgeneric (setf f) (val x) ...)` exercises. Everything downstream
    # (the registry key, the environment binding) keys on the resolved
    # *symbol*, exactly as `function_name_parts` does for DEFUN.
    from .utilities_functions import _function_spec_to_key as _fsk
    if isinstance(func_name, lisptype.LispSymbol):
        pass
    elif _fsk(func_name) is not None:
        func_name = _fsk(func_name)
    else:
        raise lisptype.LispProgramError("DEFGENERIC: function name must be a symbol or (SETF symbol)")

    if not _consp_internal(rest):
        raise lisptype.LispProgramError("DEFGENERIC requires a lambda-list")

    lambda_list = car(rest)
    options = cdr(rest)

    documentation = None
    method_combination = None
    apo_order = None
    gf_class_name = None
    method_specs = []  # (qualifiers, specializers, params, body)
    seen = set()

    current = options
    while _consp_internal(current):
        option = car(current)
        current = cdr(current)
        if not _consp_internal(option):
            raise lisptype.LispProgramError(
                f"DEFGENERIC {func_name.name}: {option} is not a valid option")
        opt_name = _keyword_name(car(option))
        if opt_name == 'METHOD':
            qualifiers, specialized_lambda_list, method_body = _parse_defmethod_tail(cdr(option))
            required_params, specializers, optional_lambda_list = _parse_specialized_lambda_list(
                specialized_lambda_list, env)
            method_specs.append((qualifiers, specializers, required_params,
                                 optional_lambda_list, method_body,
                                 specialized_lambda_list))
            continue

        # CLHS 7.7 names exactly which options may appear at most once.
        # `(declare ...)` is deliberately *not* among them -- DEFGENERIC.26
        # supplies two of them around a method description -- so a blanket
        # once-only rule rejects conforming code.
        if opt_name != 'DECLARE':
            if opt_name in seen:
                raise lisptype.LispProgramError(
                    f"DEFGENERIC {func_name.name}: option :{opt_name} appears more than once")
            seen.add(opt_name)

        if opt_name == 'DOCUMENTATION':
            doc_rest = cdr(option)
            if not _consp_internal(doc_rest):
                raise lisptype.LispProgramError(
                    f"DEFGENERIC {func_name.name}: :DOCUMENTATION requires a string")
            documentation = car(doc_rest)
        elif opt_name == 'METHOD-COMBINATION':
            method_combination = _resolve_method_combination(func_name, cdr(option))
        elif opt_name == 'ARGUMENT-PRECEDENCE-ORDER':
            apo_order = _list_elements(cdr(option))
            _check_argument_precedence_order(func_name, lambda_list, cdr(option))
        elif opt_name in ('GENERIC-FUNCTION-CLASS', 'METHOD-CLASS', 'DECLARE'):
            # :generic-function-class is recorded below and answers through
            # CLASS-OF/TYPEP; :method-class and (declare ...) still have
            # nothing to select and are accepted without effect. They are
            # not errors, and must not fall through to the unknown-option
            # branch.
            if opt_name == 'GENERIC-FUNCTION-CLASS':
                gf_class_name = car(cdr(option))
            pass
        else:
            raise lisptype.LispProgramError(
                f"DEFGENERIC {func_name.name}: unknown option {car(option)}")

    # CLHS 7.7.1: error if the name is fbound to something that is not a
    # generic function.
    existing = env.find_func(func_name)
    if existing is not None and not isinstance(existing, classes.GenericFunction):
        raise lisptype.LispProgramError(
            f"DEFGENERIC: {func_name} already names a non-generic function")

    # CLHS 7.6.4: re-declaring a generic function's lambda list with a
    # different required-parameter count while *independently added* methods
    # exist makes every one of those methods incongruent with the new
    # definition -- an error, not a silent discard (defgeneric.error.22).
    # Methods the previous DEFGENERIC's own :method options generated are
    # replaced by the re-execution (defgeneric.31 does exactly that with a
    # wider lambda list), so they do not make the redefinition an error.
    # Checked *before* ensure_generic_function runs, because that call's own
    # recovery is exactly the discard this must forbid.
    existing_gf = classes._generic_registry.find_generic(
        classes.generic_function_key(func_name))
    existing_user_methods = [m for m in existing_gf.methods
                             if not getattr(m, 'initial_method', False)] \
        if existing_gf is not None else []
    if (existing_user_methods and existing_gf.lambda_list is not None
            and classes._required_param_count(lambda_list)
            != classes._required_param_count(existing_gf.lambda_list)):
        raise lisptype.LispProgramError(
            f"DEFGENERIC: new lambda list for {func_name} is incongruent "
            f"with its existing methods")

    gf = classes.ensure_generic_function(func_name, documentation=documentation, lambda_list=lambda_list)
    gf.method_combination = method_combination

    # CLHS 7.7: :generic-function-class names the class of the generic
    # function object itself. Record the class on the object: CLASS-OF
    # answers it and TYPEP walks its CPL (defgeneric.30 typep's the
    # generic function against the class and against STANDARD-GENERIC-
    # FUNCTION, both of which must answer T).
    if gf_class_name is not None:
        _gf_cls_name = (gf_class_name.name if isinstance(gf_class_name, lisptype.LispSymbol)
                        else str(gf_class_name))
        _gf_cls = classes.find_class(_gf_cls_name)
        if _gf_cls is not None:
            gf.gf_class = _gf_cls

    # CLHS 7.6.6.1: :argument-precedence-order is a permutation of the
    # lambda list's required parameters naming the order they are compared
    # in when ordering applicable methods. Validation and installation are
    # shared with ENSURE-GENERIC-FUNCTION via
    # `classes.set_argument_precedence_order`; this used to validate the
    # permutation and then throw it away, so the ordering option changed
    # nothing about dispatch (defgeneric.4).
    if apo_order is not None:
        classes.set_argument_precedence_order(gf, lambda_list, apo_order)

    # CLHS defgeneric: re-executing the form replaces the previous
    # definition's own :method options -- the methods it generated -- while
    # leaving methods added independently (DEFMETHOD, ADD-METHOD) alone.
    # defgeneric.32 redefines a generic function with a more general
    # method and requires the redefinition's method to win.
    gf.methods = [m for m in gf.methods
                  if not getattr(m, 'initial_method', False)]

    for qualifiers, specializers, required_params, optional_lambda_list, method_body, raw_lambda_list in method_specs:
        _check_method_congruent(func_name, lambda_list, required_params, optional_lambda_list)
        method_fn = _make_method_function(required_params, optional_lambda_list, method_body, env, func_name)
        classes.add_method(gf, specializers, method_fn, qualifiers=qualifiers,
                           lambda_list=raw_lambda_list)
        # The methods this DEFGENERIC form itself generates are its
        # "initial methods" (CLHS defgeneric): a re-execution of the form
        # replaces them, while methods added by DEFMETHOD/ADD-METHOD stay.
        next(m for m in gf.methods if m.function is method_fn).initial_method = True

    from .misc_macros import install_function_binding
    install_function_binding(func_name, gf, root_environment(env))

    # CLHS 7.7: "new-generic -- The result is the generic function object."
    # This used to return func_name (a symbol), which is what DEFUN/DEFCLASS
    # return -- so `(let ((fn (eval '(defgeneric ...)))) (typep fn
    # 'generic-function))` was NIL regardless of TYPEP's own correctness,
    # because fn was never the object TYPEP was asked about.
    return gf


def eval_defmethod(form, env):
    """Evaluate DEFMETHOD special form (CLHS 7.6.2).

    Syntax:
        (defmethod name specialized-lambda-list body...)
        (defmethod name qualifier* specialized-lambda-list body...)

    Example:
        (defmethod is-similar* ((x number) (y number))
          (and (eq (class-of x) (class-of y))
               (= x y)))
    """
    import fclpy.classes as classes
    from .utilities_functions import _function_spec_to_key

    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("DEFMETHOD requires at least a name")

    func_name_spec = car(args)
    rest = cdr(args)

    # CLHS 7.6.2: function-name is a symbol or (SETF symbol). The implicit
    # per-method block (7.6.5) is named `symbol` in the (SETF symbol) case,
    # not the two-element list -- the same split DEFUN makes for its own
    # implicit block, and DEFMETHOD.6 pins it with a bare
    # `(return-from ,sym ...)` inside a `(setf ,sym)` method body.
    if isinstance(func_name_spec, lisptype.LispSymbol):
        func_name = func_name_spec
        block_name = func_name_spec
    elif _consp_internal(func_name_spec):
        setf_sym = car(func_name_spec)
        setf_rest = cdr(func_name_spec)
        if not (isinstance(setf_sym, lisptype.LispSymbol) and setf_sym.name == 'SETF'
                and _consp_internal(setf_rest) and isinstance(car(setf_rest), lisptype.LispSymbol)):
            raise lisptype.LispNotImplementedError(
                "DEFMETHOD: function name must be a symbol or (SETF symbol)")
        func_name = func_name_spec
        block_name = car(setf_rest)
    else:
        raise lisptype.LispNotImplementedError(
            "DEFMETHOD: function name must be a symbol or (SETF symbol)")

    qualifiers, specialized_lambda_list, method_body = _parse_defmethod_tail(rest)
    required_params, specializers, optional_lambda_list = _parse_specialized_lambda_list(
        specialized_lambda_list, env)

    method_fn = _make_method_function(required_params, optional_lambda_list, method_body, env, block_name)

    gf = classes.ensure_generic_function(func_name)
    # Congruence (CLHS 7.6.4) is only checked against a lambda list the
    # generic function actually declared -- `ensure_generic_function`'s own
    # contract is that DEFMETHOD alone never supplies one, so a GF an
    # earlier DEFMETHOD created implicitly has none to be congruent with
    # yet (CLHS lets the *first* method establish it instead).
    if gf.lambda_list is not None:
        _check_method_congruent(func_name, gf.lambda_list, required_params, optional_lambda_list)
    classes.add_method(gf, specializers, method_fn, qualifiers=qualifiers,
                       lambda_list=specialized_lambda_list)
    new_method = next(m for m in gf.methods if m.function is method_fn)
    # CLHS 7.6.2/25.1.3: the body's documentation string, if any, belongs to
    # the method object `(documentation method t)` reads.
    method_doc = getattr(method_fn, '__method_docstring__', None)
    if method_doc:
        new_method.documentation = str(method_doc)

    from .misc_macros import install_function_binding
    install_function_binding(_function_spec_to_key(func_name), gf,
                             root_environment(env))

    # CLHS 7.6.2: "new-method -- The result is the new method object."
    # Same defect as DEFGENERIC above: this returned func_name, so no
    # `(typep (eval '(defmethod ...)) 'standard-method)` could ever be true.
    return new_method


__all__ = [
    'eval_if',
    'eval_setq',
    'eval_defun',
    'eval_defmacro',
    'eval_macroexpand_1',
    'eval_macro_function',
    'eval_lambda',
    'eval_declare',
    'eval_declaim',
    'eval_defvar',
    'eval_defparameter',
    'eval_defconstant',
    'eval_defstruct',
    'eval_defclass',
    'eval_defgeneric',
    'eval_defmethod',
    'eval_define_method_combination',
    'eval_call_method',
    'eval_make_method',
    '_store_optimization_declaration',
    '_store_special_declaration',
]


def _list_elements(lst):
    """The elements of a Lisp list as a Python list. NIL (in any of its
    three representations -- see CLAUDE.md) is the empty list."""
    out = []
    current = lst
    while _consp_internal(current):
        out.append(car(current))
        current = cdr(current)
    return out


def _keyword_name(obj):
    """The upcased, colon-stripped name of `obj` if it is a symbol, else
    None. Used to read option keywords without caring whether the reader
    produced a `lispKeyword` or a plain symbol."""
    if isinstance(obj, lisptype.LispSymbol):
        return obj.name.upper().lstrip(':')
    return None


def eval_define_method_combination(form, env):
    """DEFINE-METHOD-COMBINATION (CLHS): define a method combination type.

    Both forms are implemented here because both produce the same kind of
    object -- a `classes.MethodCombinationType` in the one registry
    `call_generic_function` consults -- and only the way they compute an
    effective method differs.

    Short form:
        (define-method-combination name &key documentation
                                             identity-with-one-argument
                                             operator)

    Long form:
        (define-method-combination name lambda-list (method-group-spec*)
                                        [(:arguments . lambda-list)]
                                        [(:generic-function var)]
                                        declaration* body)

    None of the subforms are evaluated, which is why this is a special
    operator: the long form's body is a *macro-like* body that computes an
    effective-method form, and it must run per generic-function call rather
    than once here.

    What this replaced created an anonymous Python object, bound it as a
    *variable* under the combination's name, and defined nothing -- so a
    generic function asking for `(:method-combination progn)` got standard
    combination silently, and every method qualified `progn` was dropped
    on the floor at dispatch time (standing rule 4).
    """
    import fclpy.classes as classes

    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispProgramError("DEFINE-METHOD-COMBINATION requires a name")

    name = car(args)
    if not isinstance(name, lisptype.LispSymbol):
        raise lisptype.LispProgramError(
            "DEFINE-METHOD-COMBINATION: name must be a symbol")

    rest = cdr(args)
    # CLHS distinguishes the two forms by the second subform: a keyword (or
    # nothing at all) means the short form's option plist, anything else is
    # the long form's lambda list.
    second = car(rest) if _consp_internal(rest) else None
    is_short = (not _consp_internal(rest)) or isinstance(second, lisptype.lispKeyword)

    if is_short:
        combination = _parse_short_form_combination(name, _list_elements(rest), env)
    else:
        combination = _parse_long_form_combination(name, rest, env)

    classes.register_method_combination_type(combination)
    return name


def _parse_short_form_combination(name, options, env):
    """Build a short-form combination type from its `&key` option plist.
    The option *values* are evaluated (CLHS), unlike the long form's."""
    import fclpy.classes as classes
    from .evaluation_core import eval as _eval

    documentation = None
    operator = None
    identity = False

    i = 0
    while i < len(options):
        key = _keyword_name(options[i])
        if key is None or i + 1 >= len(options):
            raise lisptype.LispProgramError(
                f"DEFINE-METHOD-COMBINATION {name.name}: malformed option list")
        value = options[i + 1]
        if key == 'DOCUMENTATION':
            documentation = _eval(value, env)
        elif key == 'OPERATOR':
            operator = value
        elif key == 'IDENTITY-WITH-ONE-ARGUMENT':
            identity = lisptype.is_truthy(_eval(value, env))
        else:
            raise lisptype.LispProgramError(
                f"DEFINE-METHOD-COMBINATION {name.name}: unknown option {options[i]}")
        i += 2

    doc = str(documentation) if documentation not in (None, lisptype.NIL) else None
    return classes.ShortFormMethodCombination(
        name, operator=operator, identity_with_one_argument=identity, documentation=doc)


# A method-group specifier, parsed once at definition time.
#   name        the variable the matching methods are bound to
#   patterns    qualifier patterns, or None when `predicate` is used instead
#   predicate   a symbol naming a one-argument function of the qualifier list
#   order_form  unevaluated; evaluated per call, since it may name one of the
#               combination's own lambda-list parameters (CLHS)
#   required_form  likewise
class _MethodGroup:
    __slots__ = ('name', 'patterns', 'predicate', 'order_form', 'required_form', 'description')

    def __init__(self, name, patterns, predicate, order_form, required_form, description):
        self.name = name
        self.patterns = patterns
        self.predicate = predicate
        self.order_form = order_form
        self.required_form = required_form
        self.description = description


def _parse_method_group(spec):
    """Parse one method-group specifier (CLHS DEFINE-METHOD-COMBINATION):
        (name {qualifier-pattern+ | predicate}
              [[:description string]] [[:order order]] [[:required bool]])
    """
    elements = _list_elements(spec)
    if not elements:
        raise lisptype.LispProgramError(
            "DEFINE-METHOD-COMBINATION: empty method group specifier")
    group_name = elements[0]
    order_form = None
    required_form = None
    description = None
    patterns = []

    i = 1
    while i < len(elements):
        item = elements[i]
        key = _keyword_name(item) if isinstance(item, lisptype.lispKeyword) else None
        if key in ('DESCRIPTION', 'ORDER', 'REQUIRED'):
            if i + 1 >= len(elements):
                raise lisptype.LispProgramError(
                    f"DEFINE-METHOD-COMBINATION: {item} needs a value")
            if key == 'DESCRIPTION':
                description = elements[i + 1]
            elif key == 'ORDER':
                order_form = elements[i + 1]
            else:
                required_form = elements[i + 1]
            i += 2
            continue
        patterns.append(item)
        i += 1

    # A lone symbol that is not `*` is a predicate, not a pattern: `*` is the
    # wildcard pattern and NIL is the pattern matching an empty qualifier
    # list, so neither can be one.
    predicate = None
    if (len(patterns) == 1 and isinstance(patterns[0], lisptype.LispSymbol)
            and not isinstance(patterns[0], lisptype.lispKeyword)
            and patterns[0].name != '*'
            and not _is_nil(patterns[0])):
        predicate = patterns[0]
        patterns = None

    return _MethodGroup(group_name, patterns, predicate, order_form, required_form,
                        description)


def _is_nil(obj):
    """Is `obj` NIL in any of its representations (CLAUDE.md: Python None,
    the NIL singleton, or a symbol named NIL)?"""
    if obj is None or obj is lisptype.NIL:
        return True
    if isinstance(obj, lisptype.lispNull):
        return True
    return isinstance(obj, lisptype.LispSymbol) and obj.name.upper() == 'NIL'


def _qualifiers_match_pattern(qualifiers, pattern):
    """CLHS: a method matches a qualifier-pattern if its qualifier list is
    EQUAL to the pattern, except that a trailing `*` matches any remaining
    qualifiers and a bare `*` matches every method."""
    if isinstance(pattern, lisptype.LispSymbol) and pattern.name == '*':
        return True
    if _is_nil(pattern):
        return not qualifiers
    expected = _list_elements(pattern)
    if expected and isinstance(expected[-1], lisptype.LispSymbol) and expected[-1].name == '*':
        head = expected[:-1]
        if len(qualifiers) < len(head):
            return False
        return all(_qualifier_equal(a, b) for a, b in zip(qualifiers, head))
    if len(qualifiers) != len(expected):
        return False
    return all(_qualifier_equal(a, b) for a, b in zip(qualifiers, expected))


def _qualifier_equal(a, b):
    an = a.name.upper().lstrip(':') if isinstance(a, lisptype.LispSymbol) else str(a).upper()
    bn = b.name.upper().lstrip(':') if isinstance(b, lisptype.LispSymbol) else str(b).upper()
    return an == bn


def _parse_long_form_combination(name, rest, env):
    """Build a long-form combination type. The body is kept unevaluated and
    run per generic-function call, because it is what computes the
    effective method from the applicable ones."""
    import fclpy.classes as classes
    from .evaluation_core import eval as _eval
    from fclpy.lispfunc.sequence_protocol import make_lisp_list

    lambda_list = car(rest)
    tail = cdr(rest)
    if not _consp_internal(tail):
        raise lisptype.LispProgramError(
            f"DEFINE-METHOD-COMBINATION {name.name}: missing method group specifiers")
    groups = [_parse_method_group(g) for g in _list_elements(car(tail))]
    body = cdr(tail)

    # (:arguments . lambda-list) and (:generic-function var) may precede the
    # body proper; declarations and a documentation string may follow them.
    arguments_ll = None
    gf_var = None
    while _consp_internal(body):
        head = car(body)
        if not _consp_internal(head):
            break
        head_key = _keyword_name(car(head))
        if head_key == 'ARGUMENTS':
            arguments_ll = cdr(head)
        elif head_key == 'GENERIC-FUNCTION':
            gf_var = car(cdr(head))
        elif head_key == 'DOCUMENTATION' or head_key == 'DECLARE':
            pass
        else:
            break
        body = cdr(body)

    def builder(gf, applicable, options, call_args):
        comb_env = lisptype.Environment(env)
        for var, value in _combination_lambda_list_bindings(lambda_list, options, comb_env):
            comb_env.add_variable(var, value)
        if gf_var is not None:
            comb_env.add_variable(gf_var, gf)

        # The effective method is evaluated in a child of the environment the
        # body ran in, so the two can bind the same name to different things
        # -- which is exactly what `:arguments` needs (below).
        eval_env = lisptype.Environment(comb_env)

        if arguments_ll is not None:
            # CLHS: the `:arguments` lambda list gives the body access to the
            # generic function's arguments *as forms it can splice*, not as
            # values. Binding the values directly (the obvious reading, and
            # what this did first) means a `,r1` whose &rest list is
            # `(:z1 4)` splices a live cons into the effective method, where
            # it is evaluated as a call to the function :Z1. So each
            # parameter is bound in the body's environment to its own symbol,
            # and that symbol is bound to the argument value in the
            # environment the resulting form is evaluated in.
            for var, value in _combination_lambda_list_bindings(
                    arguments_ll, call_args, comb_env):
                comb_env.add_variable(var, var)
                eval_env.add_variable(var, value)

        remaining = list(applicable)
        for group in groups:
            matched = []
            still = []
            for m in remaining:
                if _method_in_group(m, group, comb_env):
                    matched.append(m)
                else:
                    still.append(m)
            remaining = still

            order = 'MOST-SPECIFIC-FIRST'
            if group.order_form is not None:
                order = (_keyword_name(_eval(group.order_form, comb_env))
                         or 'MOST-SPECIFIC-FIRST')
            if order == 'MOST-SPECIFIC-LAST':
                matched = list(reversed(matched))

            if group.required_form is not None and lisptype.is_truthy(
                    _eval(group.required_form, comb_env)) and not matched:
                raise classes.MethodCombinationError(
                    f"{name.name}: method group {group.name} is required but no "
                    f"applicable method matched it")

            comb_env.add_variable(group.name, make_lisp_list(matched))

        if remaining:
            raise classes.MethodCombinationError(
                f"{name.name}: {len(remaining)} applicable method(s) match no "
                f"method group of this combination")

        result = lisptype.NIL
        current = body
        while _consp_internal(current):
            result = _eval(car(current), comb_env)
            current = cdr(current)
        return result, eval_env

    return classes.LongFormMethodCombination(name, builder)


def _method_in_group(method, group, comb_env):
    from .evaluation_core import funcall
    from fclpy.lispfunc.sequence_protocol import make_lisp_list
    qualifiers = list(getattr(method, 'qualifiers', []))
    if group.predicate is not None:
        return lisptype.is_truthy(funcall(group.predicate, make_lisp_list(qualifiers)))
    return any(_qualifiers_match_pattern(qualifiers, p) for p in group.patterns)


def _combination_lambda_list_bindings(lambda_list, values, default_env):
    """Match a method combination's lambda list (the one after its name, and
    the `:arguments` one) against `values`, answering `[(var, value), ...]`.

    It answers bindings instead of establishing them because the two lambda
    lists need them established *differently* -- see `:arguments` in
    `_parse_long_form_combination` -- and one matcher with two callers beats
    a second copy that drifts.

    It is an ordinary lambda list -- `&optional`, `&rest` and `&key` all
    appear in ansi-test's long-form combinations -- but a *missing* argument
    is bound to NIL rather than signalling, because CLHS says this lambda
    list is not subject to the usual error checking: a generic function is
    allowed to supply fewer options than the combination names.
    """
    from fclpy.lispfunc.sequence_protocol import make_lisp_list

    params = _list_elements(lambda_list)
    values = list(values)
    bindings = []
    mode = 'REQUIRED'
    index = 0
    for param in params:
        if isinstance(param, lisptype.LispSymbol) and param.name.startswith('&'):
            mode = param.name.upper().lstrip('&')
            continue
        var, default, supplied_var = _combination_parameter_parts(param)
        if mode in ('REST', 'BODY'):
            bindings.append((var, make_lisp_list(values[index:])))
            continue
        if mode == 'KEY':
            bindings.extend(_key_parameter_binding(param, values[index:], default_env))
            continue
        if index < len(values):
            bindings.append((var, values[index]))
            if supplied_var is not None:
                bindings.append((supplied_var, lisptype.T))
            index += 1
        else:
            bindings.append((var, _default_value(default, default_env)))
            if supplied_var is not None:
                bindings.append((supplied_var, lisptype.NIL))
    return bindings


def _combination_parameter_parts(param):
    """`var`, `(var default)` or `(var default supplied-p)` -> the three."""
    if not _consp_internal(param):
        return param, None, None
    parts = _list_elements(param)
    var = parts[0]
    default = parts[1] if len(parts) > 1 else None
    supplied = parts[2] if len(parts) > 2 else None
    return var, default, supplied


def _default_value(default, default_env):
    if default is None:
        return lisptype.NIL
    from .evaluation_core import eval as _eval
    return _eval(default, default_env)


def _key_parameter_binding(param, rest_values, default_env):
    var, default, supplied = _combination_parameter_parts(param)
    keyword_name = var.name.upper().lstrip(':') if isinstance(var, lisptype.LispSymbol) else str(var)
    for i in range(0, len(rest_values) - 1, 2):
        if _keyword_name(rest_values[i]) == keyword_name:
            found = [(var, rest_values[i + 1])]
            if supplied is not None:
                found.append((supplied, lisptype.T))
            return found
    result = [(var, _default_value(default, default_env))]
    if supplied is not None:
        result.append((supplied, lisptype.NIL))
    return result


def eval_call_method(form, env):
    """CALL-METHOD (CLHS 7.6.6.2): `(call-method method [next-method-list])`.

    Neither operand is evaluated: `method` is a method object the method
    combination spliced into the effective-method form (or a
    `(make-method form)` form), and `next-method-list` is a list of the
    same. The *arguments* are the ones the generic function was called
    with -- CALL-METHOD does not name them -- so they come from the
    effective-method context rather than from the form.
    """
    import fclpy.classes as classes

    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispProgramError("CALL-METHOD requires a method")
    method = _resolve_method_designator(car(args), env)
    next_spec = car(cdr(args)) if _consp_internal(cdr(args)) else lisptype.NIL
    next_methods = [_resolve_method_designator(m, env) for m in _list_elements(next_spec)]
    return classes.call_method(method, next_methods, classes.effective_method_arguments())


def _resolve_method_designator(spec, env):
    """A CALL-METHOD operand: a method object, or a `(make-method form)`
    form naming a method whose body is that one form."""
    import fclpy.classes as classes

    if _consp_internal(spec):
        head = car(spec)
        if isinstance(head, lisptype.LispSymbol) and head.name.upper() == 'MAKE-METHOD':
            body_form = car(cdr(spec))
            from .evaluation_core import eval as _eval
            return classes.make_method_from_thunk(lambda *_args: _eval(body_form, env))
    if hasattr(spec, 'function') and callable(getattr(spec, 'function')):
        return spec
    raise lisptype.LispProgramError(
        f"CALL-METHOD: {spec!r} is not a method or a (MAKE-METHOD form) form")


def eval_make_method(form, env):
    """MAKE-METHOD (CLHS 7.6.6.2) is only meaningful as an operand of
    CALL-METHOD, which reads it structurally without evaluating it. Reaching
    it as a form means it was used somewhere it has no meaning."""
    raise lisptype.LispProgramError(
        "MAKE-METHOD is only valid as an argument to CALL-METHOD")


# The standard macros converted from hard-coded eval branches live in one
# module; importing it here (after every helper it needs is defined) is what
# gets their cl_macro registrations into the registry before
# lispenv.setup_standard_environment binds them.
from . import standard_macros as _standard_macros  # noqa: F401
