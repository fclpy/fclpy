"""Special forms: QUOTE, IF, DEFUN, DEFMACRO, LAMBDA, declarations.

This module contains handlers for special forms that don't fall into
control flow, loops/conditionals, or condition handling categories.

DEFSTRUCT: Accept keywords as structure names (v2).
"""

import fclpy.lisptype as lisptype
import fclpy.state as state
from fclpy.lispfunc.core import car, cdr, _consp_internal, cons
from .binding import proclaim_special, root_environment
from . import registry as _registry
from . import arrays as _arrays
import logging
import re
import sys

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

def eval_if(form, env):
    """Evaluate IF special form."""
    # Import eval lazily to avoid circular imports
    from .evaluation_core import eval
    import sys
    
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("IF requires at least 2 arguments")
    
    test_form = car(args)
    then_form = car(cdr(args))
    else_form = car(cdr(cdr(args))) if _consp_internal(cdr(cdr(args))) else None
    
    test_result = eval(test_form, env)
    if test_result is not None and test_result != lisptype.NIL:
        return eval(then_form, env)
    elif else_form is not None:
        return eval(else_form, env)
    else:
        return None


def eval_setq(form, env):
    """Evaluate SETQ special form."""
    from .evaluation_core import eval

    args = cdr(form)
    result = None

    while _consp_internal(args) and _consp_internal(cdr(args)):
        var = car(args)
        value_form = car(cdr(args))

        if not isinstance(var, lisptype.LispSymbol):
            raise lisptype.LispNotImplementedError("SETQ: variable must be a symbol")

        result = eval(value_form, env)
        env.set_variable(var, result)

        args = cdr(cdr(args))

    return result


def eval_psetq(form, env):
    """Evaluate PSETQ special form.

    Syntax: (PSETQ var1 val1 var2 val2 ...)

    Like SETQ, but all value-forms are evaluated first (left to right,
    using the OLD values of any vars they reference), and only then are
    all the vars assigned. Always returns NIL.
    """
    from .evaluation_core import eval

    args = cdr(form)
    pairs = []

    while _consp_internal(args) and _consp_internal(cdr(args)):
        var = car(args)
        value_form = car(cdr(args))

        if not isinstance(var, lisptype.LispSymbol):
            raise lisptype.LispNotImplementedError("PSETQ: variable must be a symbol")

        pairs.append((var, eval(value_form, env)))
        args = cdr(cdr(args))

    for var, value in pairs:
        env.set_variable(var, value)

    return lisptype.NIL


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


def eval_incf(form, env):
    """Evaluate INCF special form - increment a place.
    
    (INCF place) increments place by 1
    (INCF place delta) increments place by delta
    
    Currently only supports simple variable places, not general setf-able places.
    """
    from .evaluation_core import eval
    
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("INCF requires at least 1 argument")
    
    place = car(args)
    
    # Get delta (default 1)
    delta_form = car(cdr(args)) if _consp_internal(cdr(args)) else 1
    if delta_form != 1:
        delta = eval(delta_form, env)
    else:
        delta = 1
    
    # Handle simple variable case
    if isinstance(place, lisptype.LispSymbol):
        # Use find_variable (not lookup) to get the current binding
        if env.has_variable(place):
            current_value = env.find_variable(place)
        else:
            current_value = 0
        new_value = current_value + delta
        env.set_variable(place, new_value)
        return new_value

    # An array place: one shared reader/writer pair (arrays.py), so INCF
    # reaches every subscript of a multi-dimensional array rather than the
    # first one, and an out-of-range index is an error rather than a silent
    # extension of the underlying Python list.
    if _consp_internal(place):
        place_op = car(place)
        place_args = cdr(place)
        if isinstance(place_op, lisptype.LispSymbol) and _arrays.is_array_place(place_op.name):
            from .evaluation_core import _eval_args
            op_name = place_op.name
            values = _eval_args(place_args, env)
            current_value = _arrays.array_place_read(op_name, values)
            try:
                new_value = current_value + delta
            except Exception:
                raise lisptype.LispTypeError(
                    actual_value=current_value, expected_type='number',
                    message="INCF: cannot add delta to place value")
            _arrays.array_place_write(op_name, values, new_value)
            return new_value

    # Any other place `_place_accessor` knows (CAR/CDR/CADR/GETF/...) --
    # shared with PUSH/PUSHNEW/ROTATEF, so a place newly supported there
    # (e.g. GETF, plan.md C16) works for INCF too instead of needing its
    # own copy of the same place logic.
    getter, setter = _place_accessor(place, env)
    current_value = getter()
    try:
        new_value = current_value + delta
    except TypeError:
        raise lisptype.LispTypeError(
            actual_value=current_value, expected_type='number',
            message="INCF: cannot add delta to place value")
    setter(new_value)
    return new_value


def eval_decf(form, env):
    """Evaluate DECF special form - decrement a place.
    
    (DECF place) decrements place by 1
    (DECF place delta) decrements place by delta
    
    Currently only supports simple variable places, not general setf-able places.
    """
    from .evaluation_core import eval
    
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("DECF requires at least 1 argument")
    
    place = car(args)
    
    # Get delta (default 1)
    delta_form = car(cdr(args)) if _consp_internal(cdr(args)) else 1
    if delta_form != 1:
        delta = eval(delta_form, env)
    else:
        delta = 1
    
    # Handle simple variable case
    if isinstance(place, lisptype.LispSymbol):
        # Use find_variable (not lookup) to get the current binding
        if env.has_variable(place):
            current_value = env.find_variable(place)
        else:
            current_value = 0
        new_value = current_value - delta
        env.set_variable(place, new_value)
        return new_value

    # An array place: one shared reader/writer pair (arrays.py), so DECF
    # reaches every subscript of a multi-dimensional array rather than the
    # first one, and an out-of-range index is an error rather than a silent
    # extension of the underlying Python list.
    if _consp_internal(place):
        place_op = car(place)
        place_args = cdr(place)
        if isinstance(place_op, lisptype.LispSymbol) and _arrays.is_array_place(place_op.name):
            from .evaluation_core import _eval_args
            op_name = place_op.name
            values = _eval_args(place_args, env)
            current_value = _arrays.array_place_read(op_name, values)
            try:
                new_value = current_value - delta
            except Exception:
                raise lisptype.LispTypeError(
                    actual_value=current_value, expected_type='number',
                    message="DECF: cannot subtract delta from place value")
            _arrays.array_place_write(op_name, values, new_value)
            return new_value

    # Any other place `_place_accessor` knows (CAR/CDR/CADR/GETF/...) --
    # shared with PUSH/PUSHNEW/ROTATEF/INCF, so a place newly supported
    # there works for DECF too instead of needing its own copy.
    getter, setter = _place_accessor(place, env)
    current_value = getter()
    try:
        new_value = current_value - delta
    except TypeError:
        raise lisptype.LispTypeError(
            actual_value=current_value, expected_type='number',
            message="DECF: cannot subtract delta from place value")
    setter(new_value)
    return new_value


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

    if positional:
        # Output accumulates into the supplied string; the value is the body's.
        stream_form = _cons_from([
            lisptype.LispSymbol('MAKE-STRING-OUTPUT-STREAM'),
        ])
        binding = _cons_from([var, stream_form])
        return _cons_from([
            lisptype.LispSymbol('LET'),
            _cons_from([binding]),
            _progn_of(body),
        ])

    stream_form = _cons_from([lisptype.LispSymbol('MAKE-STRING-OUTPUT-STREAM')])
    binding = _cons_from([var, stream_form])
    get_string = _cons_from([lisptype.LispSymbol('GET-OUTPUT-STREAM-STRING'), var])

    return _cons_from(
        [lisptype.LispSymbol('LET'), _cons_from([binding])]
        + list(body)
        + [get_string]
    )


@_registry.cl_macro('WITH-INPUT-FROM-STRING',
                    documentation='WITH-INPUT-FROM-STRING macro expander')
def with_input_from_string_macro(spec, *body):
    """Macro expander for WITH-INPUT-FROM-STRING (CLHS 21.2).

    Transforms:
      (WITH-INPUT-FROM-STRING (var string &key start end) body...)
    into:
      (LET ((var (MAKE-STRING-INPUT-STREAM string start end)))
        body...)

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
    return _cons_from(
        [lisptype.LispSymbol('LET'), _cons_from([binding])] + list(body)
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
    `place*` is accepted per the grammar and otherwise ignored, matching
    every non-interactive caller of ASSERT in the ANSI suite: on failure
    they want an error signaled, not a prompt.
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
    return _cons_from([lisptype.LispSymbol('IF'), test_form, lisptype.NIL, error_call])


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


def eval_defun(form, env):
    """Evaluate DEFUN special form.
    
    DEFUN defines a function in the GLOBAL environment, not the local one.
    This is standard Common Lisp behavior - DEFUN creates top-level function bindings.
    
    Supports:
    - Required parameters
    - &optional parameters with default values
    - &rest parameter for collecting remaining arguments
    - &key parameters for keyword arguments
    - Function names as symbols or (SETF symbol) for setf functions
    """
    from .evaluation_core import eval, parse_lambda_list, ReturnFromException
    import fclpy.state as state

    args = cdr(form)
    if not _consp_internal(args) or not _consp_internal(cdr(args)):
        raise lisptype.LispNotImplementedError("DEFUN requires at least 2 arguments")

    func_name_spec = car(args)
    param_list = car(cdr(args))
    body = cdr(cdr(args))

    # func_name_spec can be a symbol or (SETF symbol) for setf functions
    if isinstance(func_name_spec, lisptype.LispSymbol):
        # Simple function name
        func_name = func_name_spec
        is_setf = False
        # DEFUN establishes an implicit block named after the function
        block_name_symbol = func_name
    elif _consp_internal(func_name_spec):
        # (SETF symbol) form for setf functions
        setf_sym = car(func_name_spec)
        if not (isinstance(setf_sym, lisptype.LispSymbol) and setf_sym.name == 'SETF'):
            raise lisptype.LispNotImplementedError("DEFUN: function name must be a symbol or (SETF symbol)")
        rest = cdr(func_name_spec)
        if not _consp_internal(rest):
            raise lisptype.LispNotImplementedError("DEFUN: (SETF symbol) requires a symbol")
        actual_func_name = car(rest)
        if not isinstance(actual_func_name, lisptype.LispSymbol):
            raise lisptype.LispNotImplementedError("DEFUN: (SETF symbol) requires symbol as second element")
        # Create a synthetic symbol for the setf function: (SETF |name|)
        # For storage, we create a LispSymbol with a compound name
        func_name = lisptype.LispSymbol(f"(SETF {actual_func_name.name})")
        is_setf = True
        # The implicit block for a (SETF symbol) function is named by symbol
        block_name_symbol = actual_func_name
    else:
        raise lisptype.LispNotImplementedError("DEFUN: function name must be a symbol or (SETF symbol)")
    
    # Extract docstring if present (first form in body can be a string)
    docstring = None
    actual_body = body
    if _consp_internal(body):
        first_form = car(body)
        if isinstance(first_form, (str, lisptype.LispString)):
            docstring = str(first_form)  # Convert to Python str for storage
            actual_body = cdr(body)
    
    # Parse the lambda list
    parsed = parse_lambda_list(param_list)
    required_params = parsed['required']
    optional_params = parsed['optional']
    rest_param = parsed['rest']
    keyword_params = parsed['keyword']
    aux_params = parsed.get('aux', [])
    
    # Create function closure
    # The closure captures the current lexical environment for variable lookups
    def user_function(*call_args):
        # Create new environment for function execution
        func_env = lisptype.Environment(env)
        
        arg_index = 0
        
        # Bind required parameters
        for param in required_params:
            if arg_index < len(call_args):
                func_env.add_variable(param, call_args[arg_index])
                arg_index += 1
            else:
                func_env.add_variable(param, lisptype.NIL)
        
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
                func_env.add_variable(param, call_args[arg_index])
                if supplied_p is not None:
                    func_env.add_variable(supplied_p, lisptype.T)
                arg_index += 1
            else:
                # Use default value if provided, otherwise NIL
                if default_form is not None:
                    default_value = eval(default_form, func_env)
                    func_env.add_variable(param, default_value)
                else:
                    func_env.add_variable(param, lisptype.NIL)
                if supplied_p is not None:
                    func_env.add_variable(supplied_p, lisptype.NIL)
        
        # Collect remaining positional arguments for &rest
        remaining_positional = []
        
        # Find where keyword arguments start
        keyword_start = arg_index
        for i in range(arg_index, len(call_args)):
            if isinstance(call_args[i], lisptype.lispKeyword):
                keyword_start = i
                break
            remaining_positional.append(call_args[i])
            arg_index = i + 1
        
        # Bind &rest parameter if present
        if rest_param:
            # Rest gets all remaining positional args as a list
            if remaining_positional:
                rest_list = lisptype.NIL
                for item in reversed(remaining_positional):
                    rest_list = lisptype.lispCons(item, rest_list)
            else:
                rest_list = lisptype.NIL

            # Support destructuring rest spec: either a symbol or a cons
            if isinstance(rest_param, lisptype.LispSymbol):
                func_env.add_variable(rest_param, rest_list)
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
                    func_env.add_variable(head, first)
                if isinstance(tail, lisptype.LispSymbol):
                    func_env.add_variable(tail, rest_tail)
                else:
                    # If tail isn't a symbol, bind the whole rest_list
                    func_env.add_variable(rest_param, rest_list)
        
        # Bind keyword parameters
        # First, initialize all keyword params to their defaults and supplied-p to NIL
        for param_spec in keyword_params:
            if _consp_internal(param_spec):
                param = car(param_spec)
                rest = cdr(param_spec)
                default_form = car(rest) if _consp_internal(rest) else None
                # Check for supplied-p parameter (third element)
                rest2 = cdr(rest) if _consp_internal(rest) else None
                supplied_p = car(rest2) if _consp_internal(rest2) else None
            else:
                param = param_spec
                default_form = None
                supplied_p = None
            
            # Default value
            if default_form is not None:
                default_value = eval(default_form, func_env)
                func_env.add_variable(param, default_value)
            else:
                func_env.add_variable(param, lisptype.NIL)
            
            # Initialize supplied-p to NIL (not supplied yet)
            if supplied_p is not None:
                func_env.add_variable(supplied_p, lisptype.NIL)
        
        # Now process actual keyword arguments from the call
        i = keyword_start
        while i < len(call_args) - 1:
            key = call_args[i]
            value = call_args[i + 1]
            
            if isinstance(key, lisptype.lispKeyword):
                key_name = key.name.upper()
                # Find matching parameter
                for param_spec in keyword_params:
                    if _consp_internal(param_spec):
                        param = car(param_spec)
                        rest = cdr(param_spec)
                        rest2 = cdr(rest) if _consp_internal(rest) else None
                        supplied_p = car(rest2) if _consp_internal(rest2) else None
                    else:
                        param = param_spec
                        supplied_p = None
                    
                    if isinstance(param, lisptype.LispSymbol) and param.name.upper() == key_name:
                        func_env.add_variable(param, value)
                        # Set supplied-p to T when keyword is provided
                        if supplied_p is not None:
                            func_env.add_variable(supplied_p, lisptype.T)
                        break
                i += 2
            else:
                i += 1
        
        # Bind &aux parameters
        for param_spec in aux_params:
            if _consp_internal(param_spec):
                param = car(param_spec)
                init_form = car(cdr(param_spec))
                init_value = eval(init_form, func_env)
                func_env.add_variable(param, init_value)
            else:
                func_env.add_variable(param_spec, lisptype.NIL)
        
        # Execute body, enclosed in the implicit block DEFUN establishes
        # around the function body (named after the function).
        result = None
        try:
            current_body = actual_body
            while _consp_internal(current_body):
                result = eval(car(current_body), func_env)
                current_body = cdr(current_body)
        except ReturnFromException as e:
            tag = e.tag
            block_match = False
            if tag == block_name_symbol:
                block_match = True
            elif isinstance(tag, lisptype.LispSymbol) and isinstance(block_name_symbol, lisptype.LispSymbol):
                block_match = (tag.name == block_name_symbol.name)
            if block_match:
                result = e.value
            else:
                raise

        return result
    
    # Find the global/root environment for defining the function
    # DEFUN always creates global function bindings
    global_env = env
    while global_env.parent is not None:
        global_env = global_env.parent
    
    # Add function to the GLOBAL environment (not local)
    global_env.add_function(func_name, user_function)
    
    # Also add to the current environment for immediate visibility
    # (this helps when the function is called later in the same file)
    if env is not global_env:
        env.add_function(func_name, user_function)
    
    # Store docstring on the function symbol's property list
    if docstring:
        if not hasattr(func_name, 'plist'):
            func_name.plist = {}
        func_name.plist['DOCUMENTATION'] = docstring
    
    return func_name
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
    from .evaluation_core import eval, parse_lambda_list, bind_destructuring_pattern

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

    # Create the macro callable
    def macro_callable(*call_args):
        # Create a new environment extending the definition environment
        macro_env = lisptype.Environment(parent=env)

        # Detect optional trailing expansion-time Environment argument.
        # The macroexpander may invoke the macro callable with the
        # expansion environment as the last positional argument. If so,
        # use it for &ENVIRONMENT bindings; otherwise use the captured
        # definition environment.
        expansion_env = env
        if len(call_args) > 0 and isinstance(call_args[-1], lisptype.Environment):
            expansion_env = call_args[-1]
            call_args = tuple(call_args[:-1])

        # Normalize NIL symbol arguments to the canonical NIL object
        new_args = []
        for a in call_args:
            if isinstance(a, lisptype.LispSymbol) and a.name.upper() == 'NIL':
                new_args.append(lisptype.NIL)
            else:
                new_args.append(a)
        call_args = tuple(new_args)
        
        arg_idx = 0

        # Handle &WHOLE parameter
        whole_param = parsed_params.get('whole') if isinstance(parsed_params, dict) else None
        if whole_param is not None:
            if len(call_args) > 0:
                macro_env.add_variable(whole_param, call_args[0])
                arg_idx = 1
            else:
                macro_env.add_variable(whole_param, lisptype.NIL)
                arg_idx = 1
        
        # Bind &ENVIRONMENT to the expansion-time environment if provided
        # The macro callable may be invoked with an extra trailing Environment
        # argument by the macroexpander. If so, prefer that; otherwise fall
        # back to the environment captured at definition time.
        if environment_param is not None:
            macro_env.add_variable(environment_param, expansion_env)
        
        # Bind required parameters. A required parameter spec may be a plain
        # symbol or an arbitrary nested destructuring pattern (CLHS 3.4.4,
        # e.g. `(arg1 (&whole w arg2))` or `(&rest vars)`); either shape is
        # handled by the one shared destructuring binder rather than a second
        # case here for every lambda-list-keyword combination that can appear
        # nested.
        for param in required_params:
            val = call_args[arg_idx] if arg_idx < len(call_args) else lisptype.NIL
            bind_destructuring_pattern(param, val, macro_env)
            arg_idx += 1

        # A destructuring-pattern parameter name (in &OPTIONAL/&KEY position)
        # is bound the same way a required one is.
        def _bind_pattern(pat, val):
            bind_destructuring_pattern(pat, val, macro_env)

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
                    macro_env.add_variable(opt_name, val)
                else:
                    _bind_pattern(opt_name, val)
                if supplied_p is not None:
                    macro_env.add_variable(supplied_p, lisptype.T)
                arg_idx += 1
            else:
                if opt_default is not None:
                    default_value = eval(opt_default, macro_env)
                else:
                    default_value = unsupplied_default

                if isinstance(opt_name, lisptype.LispSymbol):
                    macro_env.add_variable(opt_name, default_value)
                else:
                    _bind_pattern(opt_name, default_value)

                if supplied_p is not None:
                    macro_env.add_variable(supplied_p, lisptype.NIL)

        # Bind &rest parameter
        if rest_param:
            remaining_args = call_args[arg_idx:]
            if remaining_args:
                rest_list = lisptype.NIL
                for arg in reversed(remaining_args):
                    rest_list = cons(arg, rest_list)
            else:
                rest_list = lisptype.NIL
            if isinstance(rest_param, lisptype.LispSymbol):
                macro_env.add_variable(rest_param, rest_list)
            elif _consp_internal(rest_param):
                head = car(rest_param)
                tail_sym = _extract_tail_symbol_from_rest(rest_param)
                if _consp_internal(rest_list):
                    first = car(rest_list)
                    rest_tail = cdr(rest_list)
                else:
                    first = lisptype.NIL
                    rest_tail = lisptype.NIL
                if isinstance(head, lisptype.LispSymbol):
                    macro_env.add_variable(head, first)
                if tail_sym is not None:
                    macro_env.add_variable(tail_sym, rest_tail)
        
        # Bind keyword parameters with defaults and supplied-p
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

            _, var_pattern = _kw_parts(param)

            if default_form is not None:
                default_value = eval(default_form, macro_env)
            else:
                default_value = unsupplied_default
            _bind_pattern(var_pattern, default_value)

            if supplied_p is not None:
                macro_env.add_variable(supplied_p, lisptype.NIL)
        
        # Process actual keyword arguments
        keyword_start = arg_idx
        i = keyword_start
        while i < len(call_args) - 1:
            key = call_args[i]
            value = call_args[i + 1]
            
            if isinstance(key, lisptype.lispKeyword):
                key_name = key.name.upper()
                for param_spec in keyword_params:
                    if _consp_internal(param_spec):
                        param = car(param_spec)
                        rest = cdr(param_spec)
                        rest2 = cdr(rest) if _consp_internal(rest) else None
                        supplied_p = car(rest2) if _consp_internal(rest2) else None
                    else:
                        param = param_spec
                        supplied_p = None

                    kw_name, var_pattern = _kw_parts(param)
                    if kw_name == key_name:
                        _bind_pattern(var_pattern, value)
                        if supplied_p is not None:
                            macro_env.add_variable(supplied_p, lisptype.T)
                        break
                i += 2
            else:
                i += 1

        # If no body, return NIL
        if not _consp_internal(actual_body):
            return lisptype.NIL

        # Evaluate the body inside an implicit BLOCK named for the macro.
        # This mirrors DEFUN/DEFMACRO semantics where the function/macro
        # body is implicitly a BLOCK so RETURN-FROM can target the name.
        block_form = lisptype.lispCons(lisptype.LispSymbol('BLOCK'), lisptype.lispCons(macro_name, actual_body))
        return eval(block_form, macro_env)

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
    
    # Add macro to the GLOBAL environment (not local)
    global_env.add_function(macro_name, macro_callable)
    
    # Also add to the current environment for immediate visibility
    if env is not global_env:
        env.add_function(macro_name, macro_callable)
    
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
    
    form_to_expand_raw = car(args)
    
    # If the form is (QUOTE x), evaluate it to get x
    # Otherwise, use the form as-is
    if _consp_internal(form_to_expand_raw) and isinstance(car(form_to_expand_raw), lisptype.LispSymbol) and car(form_to_expand_raw).name == 'QUOTE':
        form_to_expand = eval(form_to_expand_raw, env)
    else:
        form_to_expand = form_to_expand_raw
    
    # Only cons cells can be macro calls
    if not _consp_internal(form_to_expand):
        return form_to_expand
    
    operator = car(form_to_expand)
    if not isinstance(operator, lisptype.LispSymbol):
        return form_to_expand
    
    # Try to find the operator function
    try:
        macro_func = env.find_func(operator)
    except Exception:
        macro_func = None
        logger.error(f"[DEBUG] Error looking up macro function for {operator}", exc_info=True)

    if not macro_func or not callable(macro_func):
        return form_to_expand
    
    # Check if it's actually a macro
    if not getattr(macro_func, '__is_macro__', False):
        return form_to_expand
    
    # Call the macro with unevaluated arguments
    args_list = []
    current = cdr(form_to_expand)
    while _consp_internal(current):
        args_list.append(car(current))
        current = cdr(current)
    
    # If there's a non-nil tail, that's an error, but for now just ignore it
    try:
        expects_whole = getattr(macro_func, '__expects_whole__', False)
        expects_env = getattr(macro_func, '__expects_environment__', False)

        # Build call arguments based on macro function expectations
        call_args = []
        if expects_whole:
            call_args.append(form_to_expand)
        call_args.extend(args_list)

        # If macro expects expansion-time environment, append it as trailing arg
        if expects_env:
            call_args.append(env)

        return macro_func(*call_args)
    except Exception:
        # If macro expansion fails, return form unchanged
        return form_to_expand


def eval_macro_function(form, env):
    """Evaluate MACRO-FUNCTION special form.
    
    (MACRO-FUNCTION symbol) - return the macro function for a symbol, or NIL if not a macro.
    """
    from .evaluation_core import eval, ConditionException

    args = cdr(form)
    if not _consp_internal(args):
        cond = lisptype.ProgramError(message="MACRO-FUNCTION requires 1 argument")
        raise ConditionException(cond, recoverable=False)
    
    symbol_form = car(args)
    
    # The symbol form might be quoted, so we need to evaluate it to get the symbol
    # Or it might already be a symbol
    if isinstance(symbol_form, lisptype.LispSymbol):
        symbol = symbol_form
    else:
        # Try evaluating it
        symbol = eval(symbol_form, env)
    
    if not isinstance(symbol, lisptype.LispSymbol):
        return lisptype.NIL

    # Try to find the function in the environment first
    func = env.find_func(symbol)

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

    # Evaluate the expression to destructure
    expr_val = eval(expr_form, env)

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


def eval_lambda(form, env):
    """Evaluate LAMBDA special form."""
    from .evaluation_core import eval, parse_lambda_list

    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("LAMBDA requires at least 1 argument")

    param_list = car(args)
    body = cdr(args)

    # Parse the lambda list to support &optional, &rest, &key, &aux, &environment
    parsed = parse_lambda_list(param_list)
    required_params = parsed.get('required', [])
    optional_params = parsed.get('optional', [])
    rest_param = parsed.get('rest', None)
    keyword_params = parsed.get('keyword', [])
    aux_params = parsed.get('aux', [])
    environment_param = parsed.get('environment', None)

    # Create function closure
    def lambda_function(*call_args):
        # Create new environment for function execution
        func_env = lisptype.Environment(env)

        # Normalize NIL symbol arguments to canonical NIL
        new_args = []
        for a in call_args:
            if isinstance(a, lisptype.LispSymbol) and a.name.upper() == 'NIL':
                new_args.append(lisptype.NIL)
            else:
                new_args.append(a)
        call_args = tuple(new_args)

        arg_index = 0

        # Bind required parameters
        for param in required_params:
            if arg_index < len(call_args):
                func_env.add_variable(param, call_args[arg_index])
                arg_index += 1
            else:
                func_env.add_variable(param, lisptype.NIL)

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
                func_env.add_variable(param, call_args[arg_index])
                if supplied_p is not None:
                    func_env.add_variable(supplied_p, lisptype.T)
                arg_index += 1
            else:
                if default_form is not None:
                    default_value = eval(default_form, func_env)
                    func_env.add_variable(param, default_value)
                else:
                    func_env.add_variable(param, lisptype.NIL)
                if supplied_p is not None:
                    func_env.add_variable(supplied_p, lisptype.NIL)

        # Collect remaining positional arguments for &rest
        remaining_positional = []

        # Find where keyword arguments start
        keyword_start = arg_index
        for i in range(arg_index, len(call_args)):
            if isinstance(call_args[i], lisptype.lispKeyword):
                keyword_start = i
                break
            remaining_positional.append(call_args[i])
            arg_index = i + 1

        # Bind &rest parameter if present
        if rest_param:
            if remaining_positional:
                rest_list = lisptype.NIL
                for item in reversed(remaining_positional):
                    rest_list = lisptype.lispCons(item, rest_list)
            else:
                rest_list = lisptype.NIL

            if isinstance(rest_param, lisptype.LispSymbol):
                func_env.add_variable(rest_param, rest_list)
            elif _consp_internal(rest_param):
                head = car(rest_param)
                tail_sym = _extract_tail_symbol_from_rest(rest_param)
                if _consp_internal(rest_list):
                    first = car(rest_list)
                    rest_tail = cdr(rest_list)
                else:
                    first = lisptype.NIL
                    rest_tail = lisptype.NIL
                if isinstance(head, lisptype.LispSymbol):
                    func_env.add_variable(head, first)
                if tail_sym is not None:
                    func_env.add_variable(tail_sym, rest_tail)

        # Bind keyword parameters: initialize to defaults and supplied-p to NIL
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

            if default_form is not None:
                default_value = eval(default_form, func_env)
                func_env.add_variable(param, default_value)
            else:
                func_env.add_variable(param, lisptype.NIL)

            if supplied_p is not None:
                func_env.add_variable(supplied_p, lisptype.NIL)

        # Now process actual keyword arguments from the call
        i = keyword_start
        while i < len(call_args) - 1:
            key = call_args[i]
            value = call_args[i + 1]

            if isinstance(key, lisptype.lispKeyword):
                key_name = key.name.upper()
                # Find matching parameter
                for param_spec in keyword_params:
                    if _consp_internal(param_spec):
                        param = car(param_spec)
                        rest = cdr(param_spec)
                        rest2 = cdr(rest) if _consp_internal(rest) else None
                        supplied_p = car(rest2) if _consp_internal(rest2) else None
                    else:
                        param = param_spec
                        supplied_p = None

                    if isinstance(param, lisptype.LispSymbol) and param.name.upper() == key_name:
                        func_env.add_variable(param, value)
                        if supplied_p is not None:
                            func_env.add_variable(supplied_p, lisptype.T)
                        break
                i += 2
            else:
                i += 1

        # Bind &aux parameters
        for param_spec in aux_params:
            if _consp_internal(param_spec):
                param = car(param_spec)
                init_form = car(cdr(param_spec))
                init_value = eval(init_form, func_env)
                func_env.add_variable(param, init_value)
            else:
                func_env.add_variable(param_spec, lisptype.NIL)

        # Bind &environment if requested
        if environment_param is not None:
            func_env.add_variable(environment_param, env)

        # Execute body
        result = None
        current_body = body
        while _consp_internal(current_body):
            result = eval(car(current_body), func_env)
            current_body = cdr(current_body)

        return result

    return lambda_function


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

    PROCLAIM takes declaration specifiers which may need evaluation
    (for example a backquoted form with unquote). Evaluate each spec
    in the current environment and apply it globally (similar to
    DECLAIM but evaluating the spec forms first).
    """
    from .evaluation_core import eval as lisp_eval

    args = cdr(form)

    # PROCLAIM must have at least one specifier
    if args is None or args is lisptype.NIL:
        raise lisptype.LispProgramError("PROCLAIM requires at least one declaration specifier")

    # Get the global/root environment to store effects
    global_env = env
    while global_env.parent is not None:
        global_env = global_env.parent

    while _consp_internal(args):
        spec_expr = car(args)
        # Evaluate the spec expression so backquote/unquote is handled
        spec = lisp_eval(spec_expr, env)

        if _consp_internal(spec):
            spec_type = car(spec)
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

        args = cdr(args)

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

    # Handle documentation string if present (third argument)
    if has_value_form:
        doc_args = cdr(rest_args)
        if _consp_internal(doc_args):
            docstring = car(doc_args)
            if isinstance(docstring, str):
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
        if isinstance(docstring, str):
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
        if isinstance(docstring, str):
            # Store documentation on symbol's property list
            if not hasattr(name, 'plist'):
                name.plist = {}
            name.plist['DOCUMENTATION'] = docstring
            name.plist['VARIABLE-DOCUMENTATION'] = docstring
    
    # Mark as constant in global environment
    if not hasattr(global_env, '_constants'):
        global_env._constants = {}
    global_env._constants[name.name] = True
    
    return name


def eval_defstruct(form, env):
    """Evaluate DEFSTRUCT special form.
    
    (DEFSTRUCT name slot...)
    (DEFSTRUCT (name option...) slot...)
    
    DEFSTRUCT does not evaluate its arguments - they are literal specifications.
    DEFSTRUCT creates GLOBAL function bindings like DEFUN does.
    """
    import fclpy.state as state
    
    # Get current package for interning accessor symbols
    current_pkg = getattr(state, 'current_package', None) or lisptype.COMMON_LISP_USER_PACKAGE
    
    # Find the global/root environment for defining functions
    # DEFSTRUCT always creates global function bindings (like DEFUN)
    global_env = env
    while global_env.parent is not None:
        global_env = global_env.parent
    
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("DEFSTRUCT requires a name")
    
    name_and_options = car(args)
    slot_specs = cdr(args)
    
    # Parse name and options
    if isinstance(name_and_options, lisptype.lispKeyword):
        # Accept keywords as structure names and use their name
        struct_name = name_and_options
        conc_name = struct_name.name + '-'
        constructor_name = 'MAKE-' + struct_name.name
        copier_name = 'COPY-' + struct_name.name
        predicate_name = struct_name.name + '-P'
        include_parent = None
    elif isinstance(name_and_options, lisptype.LispSymbol):
        struct_name = name_and_options
        conc_name = struct_name.name + '-'
        constructor_name = 'MAKE-' + struct_name.name
        copier_name = 'COPY-' + struct_name.name
        predicate_name = struct_name.name + '-P'
        include_parent = None
    elif _consp_internal(name_and_options):
        struct_name = car(name_and_options)
        # Validate that struct_name is a symbol or keyword
        if isinstance(struct_name, lisptype.lispKeyword):
            # Accept keywords and use their name
            pass
        elif not isinstance(struct_name, lisptype.LispSymbol):
            raise lisptype.LispNotImplementedError(f"DEFSTRUCT: structure name must be a symbol, got {type(struct_name)}")
        conc_name = struct_name.name + '-'  # Default prefix
        constructor_name = 'MAKE-' + struct_name.name
        copier_name = 'COPY-' + struct_name.name
        predicate_name = struct_name.name + '-P'
        include_parent = None
        
        # Parse options
        options = cdr(name_and_options)
        while _consp_internal(options):
            opt = car(options)
            if _consp_internal(opt):
                opt_name = car(opt)
                opt_value = car(cdr(opt)) if _consp_internal(cdr(opt)) else None
                
                if isinstance(opt_name, lisptype.LispSymbol):
                    opt_name_str = opt_name.name.upper()
                elif isinstance(opt_name, lisptype.lispKeyword):
                    opt_name_str = opt_name.name.upper()
                else:
                    opt_name_str = str(opt_name).upper()
                
                if opt_name_str == 'CONC-NAME' or opt_name_str == ':CONC-NAME':
                    # Check for NIL value (can be None, the NIL constant, or a symbol named "NIL")
                    is_nil = (opt_value is None or 
                              opt_value == lisptype.NIL or
                              (isinstance(opt_value, lisptype.LispSymbol) and opt_value.name == 'NIL'))
                    if is_nil:
                        conc_name = ''  # No prefix
                    elif isinstance(opt_value, lisptype.LispSymbol):
                        conc_name = opt_value.name
                    else:
                        conc_name = str(opt_value)
                elif opt_name_str == 'CONSTRUCTOR' or opt_name_str == ':CONSTRUCTOR':
                    is_nil = (opt_value is None or 
                              opt_value == lisptype.NIL or
                              (isinstance(opt_value, lisptype.LispSymbol) and opt_value.name == 'NIL'))
                    if is_nil:
                        constructor_name = None
                    elif isinstance(opt_value, lisptype.LispSymbol):
                        constructor_name = opt_value.name
                elif opt_name_str == 'COPIER' or opt_name_str == ':COPIER':
                    is_nil = (opt_value is None or 
                              opt_value == lisptype.NIL or
                              (isinstance(opt_value, lisptype.LispSymbol) and opt_value.name == 'NIL'))
                    if is_nil:
                        copier_name = None
                    elif isinstance(opt_value, lisptype.LispSymbol):
                        copier_name = opt_value.name
                elif opt_name_str == 'PREDICATE' or opt_name_str == ':PREDICATE':
                    is_nil = (opt_value is None or 
                              opt_value == lisptype.NIL or
                              (isinstance(opt_value, lisptype.LispSymbol) and opt_value.name == 'NIL'))
                    if is_nil:
                        predicate_name = None
                    elif isinstance(opt_value, lisptype.LispSymbol):
                        predicate_name = opt_value.name
                elif opt_name_str == 'INCLUDE' or opt_name_str == ':INCLUDE':
                    if isinstance(opt_value, lisptype.LispSymbol):
                        include_parent = opt_value.name
            options = cdr(options)
    else:
        struct_name = name_and_options
        conc_name = str(struct_name) + '-'
        constructor_name = 'MAKE-' + str(struct_name)
        copier_name = 'COPY-' + str(struct_name)
        predicate_name = str(struct_name) + '-P'
        include_parent = None
    
    struct_class_name = struct_name.name if isinstance(struct_name, lisptype.LispSymbol) else str(struct_name)
    
    # Parse slot definitions
    slot_defs = []  # List of (slot_name, default_value)
    while _consp_internal(slot_specs):
        slot = car(slot_specs)
        if isinstance(slot, lisptype.LispSymbol):
            slot_defs.append((slot.name, lisptype.NIL))
        elif _consp_internal(slot):
            slot_name = car(slot)
            if isinstance(slot_name, lisptype.LispSymbol):
                slot_name_str = slot_name.name
            else:
                slot_name_str = str(slot_name)
            default_value = car(cdr(slot)) if _consp_internal(cdr(slot)) else lisptype.NIL
            slot_defs.append((slot_name_str, default_value))
        else:
            slot_defs.append((str(slot), lisptype.NIL))
        slot_specs = cdr(slot_specs)
    
    # Create the structure class
    class StructureInstance:
        def __init__(self, struct_type=struct_class_name, slot_defaults=None, **kwargs):
            self._struct_type = struct_type
            self._slots = {}
            # Initialize with defaults
            if slot_defaults is None:
                slot_defaults = slot_defs
            for slot_name, default_val in slot_defaults:
                self._slots[slot_name] = default_val
            # Override with provided values
            for key, value in kwargs.items():
                key_upper = key.upper()
                for slot_name, _ in slot_defaults:
                    if slot_name.upper() == key_upper:
                        self._slots[slot_name] = value
                        break
        
        def __repr__(self):
            slot_values = ' '.join(f':{k} {v}' for k, v in self._slots.items())
            return f'#S({self._struct_type} {slot_values})'
        
        def get_slot(self, name):
            return self._slots.get(name, lisptype.NIL)
        
        def set_slot(self, name, value):
            self._slots[name] = value
    
    # Store the structure class in a registry
    if not hasattr(state, '_structure_classes'):
        state._structure_classes = {}
    state._structure_classes[struct_class_name] = {
        'class': StructureInstance,
        'slots': slot_defs,
        'conc_name': conc_name
    }
    
    # Create constructor function
    if constructor_name:
        def constructor_wrapper(*args, **kwargs):
            # Convert keyword symbol arguments to kwargs
            result_kwargs = dict(kwargs)
            i = 0
            while i < len(args):
                if i + 1 < len(args):
                    key = args[i]
                    value = args[i + 1]
                    if isinstance(key, lisptype.lispKeyword):
                        result_kwargs[key.name.upper()] = value
                        i += 2
                    else:
                        i += 1
                else:
                    i += 1
            return StructureInstance(struct_class_name, slot_defs, **result_kwargs)
        
        constructor_sym = current_pkg.intern_symbol(constructor_name)
        global_env.add_function(constructor_sym, constructor_wrapper)
    
    # Create copier function
    if copier_name:
        def copy_structure(struct):
            if not isinstance(struct, StructureInstance):
                raise TypeError(f"Not a {struct_class_name}: {struct}")
            new_struct = StructureInstance(struct_class_name, slot_defs)
            new_struct._slots = dict(struct._slots)
            return new_struct
        
        copier_sym = current_pkg.intern_symbol(copier_name)
        global_env.add_function(copier_sym, copy_structure)
    
    # Create predicate function
    if predicate_name:
        def is_structure(obj):
            if hasattr(obj, '_struct_type') and obj._struct_type == struct_class_name:
                return lisptype.T
            return lisptype.NIL
        
        predicate_sym = current_pkg.intern_symbol(predicate_name)
        global_env.add_function(predicate_sym, is_structure)
    
    # Create accessor functions for each slot
    for slot_name, _ in slot_defs:
        accessor_name = conc_name + slot_name
        
        # Create getter
        def make_getter(sn):
            def getter(struct):
                if hasattr(struct, 'get_slot'):
                    return struct.get_slot(sn)
                raise TypeError(f"Not a {struct_class_name}: {struct}")
            return getter
        
        accessor_sym = current_pkg.intern_symbol(accessor_name)
        global_env.add_function(accessor_sym, make_getter(slot_name))
        
        # Create setter (for SETF)
        def make_setter(sn):
            def setter(struct, value):
                if hasattr(struct, 'set_slot'):
                    struct.set_slot(sn, value)
                    return value
                raise TypeError(f"Not a {struct_class_name}: {struct}")
            return setter
        
        setter_name = 'SET-' + accessor_name
        setter_sym = current_pkg.intern_symbol(setter_name)
        global_env.add_function(setter_sym, make_setter(slot_name))
    
    return struct_name


def _pop_expander(place, *_rest):
    """POP macro expander: return an expansion form for (POP place).

    This is the callable returned by (MACRO-FUNCTION 'POP).  It is flagged
    with __is_macro__ so the macro-expander recognises it.

    For a simple symbol place we expand into:
        (LET ((#:tmp (CAR place)))
          (SETQ place (CDR place))
          #:tmp)
    """
    from . import utilities_symbols as _utils

    if isinstance(place, lisptype.LispSymbol):
        tmp = _utils.gensym()
        car_call  = lisptype.lispCons(lisptype.LispSymbol('CAR'),
                        lisptype.lispCons(place, lisptype.NIL))
        binding   = lisptype.lispCons(tmp,
                        lisptype.lispCons(car_call, lisptype.NIL))
        bindings  = lisptype.lispCons(binding, lisptype.NIL)
        cdr_call  = lisptype.lispCons(lisptype.LispSymbol('CDR'),
                        lisptype.lispCons(place, lisptype.NIL))
        setq_call = lisptype.lispCons(lisptype.LispSymbol('SETQ'),
                        lisptype.lispCons(place,
                            lisptype.lispCons(cdr_call, lisptype.NIL)))
        body      = lisptype.lispCons(setq_call,
                        lisptype.lispCons(tmp, lisptype.NIL))
        return lisptype.lispCons(lisptype.LispSymbol('LET'),
                   lisptype.lispCons(bindings, body))

    raise lisptype.LispNotImplementedError(f"POP: unsupported place form: {place}")

# Mark as a proper macro callable and register it so (MACRO-FUNCTION 'POP) returns it.
_pop_expander.__is_macro__ = True
_registry.function_registry['POP'] = _registry.RegistryEntry(
    name='POP',
    py_name='_pop_expander',
    kind='macro',
    func=_pop_expander,
)


def eval_pop(form, env):
    """Evaluate POP special form.

    (POP place) — Remove and return the first element from the list stored
    in place. Goes through `_place_accessor` (shared with ROTATEF/SHIFTF)
    so any place kind it supports works here too, not just a bare
    variable -- `cons/pop.lsp`'s `(pop (aref x i))`-style cases need
    exactly that.
    """
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("POP requires a place argument")

    place = car(args)
    getter, setter = _place_accessor(place, env)
    old_value = getter()
    if old_value is lisptype.NIL or old_value is None:
        # (car nil) and (cdr nil) are both NIL, not an error (CLHS NIL is
        # the empty list) -- `pop.2` pops an already-empty place and
        # expects `(nil nil)` back, not a signaled error.
        setter(lisptype.NIL)
        return lisptype.NIL
    if not _consp_internal(old_value):
        raise lisptype.LispError("POP: place does not hold a cons")

    setter(old_value.cdr)
    return old_value.car


def eval_remf(form, env):
    """Evaluate REMF special form.

    (REMF place indicator) -- CLHS 5.1.3: removes the indicator/value pair
    named by `indicator` from the plist stored in `place`, returning a
    generalized boolean (true if a pair was removed, NIL if the indicator
    was not found). `place`'s subforms are evaluated exactly once, before
    `indicator` (pinned by `remf.order.1`/`.2`), through `_place_accessor`
    (shared with PUSH/POP/PUSHNEW/ROTATEF) so any place kind it supports
    works here too.

    Previously a `cl_function` that received `place` already evaluated to
    a value and unconditionally returned NIL without removing anything --
    a place designator is not a value, and REMF must be able to write the
    shortened list back to arbitrary places, not just read one.
    """
    from .evaluation_core import eval

    args = cdr(form)
    if not _consp_internal(args) or not _consp_internal(cdr(args)):
        raise lisptype.LispNotImplementedError("REMF requires a place and an indicator")

    place = car(args)
    getter, setter = _place_accessor(place, env)
    indicator = eval(car(cdr(args)), env)

    plist = getter()
    if not _consp_internal(plist):
        return lisptype.NIL
    if plist.car is indicator:
        if not _consp_internal(plist.cdr):
            raise lisptype.LispError("REMF: odd-length property list")
        setter(plist.cdr.cdr)
        return lisptype.T

    prev = plist
    if not _consp_internal(prev.cdr):
        raise lisptype.LispError("REMF: odd-length property list")
    current = prev.cdr.cdr
    while _consp_internal(current):
        if not _consp_internal(current.cdr):
            raise lisptype.LispError("REMF: odd-length property list")
        if current.car is indicator:
            prev.cdr.cdr = current.cdr.cdr
            return lisptype.T
        prev = current
        current = current.cdr.cdr
    return lisptype.NIL


def _push_expander(item, place, *_rest):
    """PUSH macro expander: return an expansion form for (PUSH item place).

    This is the callable returned by (MACRO-FUNCTION 'PUSH). It is flagged
    with __is_macro__ so the macro-expander recognises it.

    For a simple symbol place we expand into:
        (SETQ place (CONS item place))
    """
    if isinstance(place, lisptype.LispSymbol):
        cons_call = lisptype.lispCons(lisptype.LispSymbol('CONS'),
                        lisptype.lispCons(item,
                            lisptype.lispCons(place, lisptype.NIL)))
        return lisptype.lispCons(lisptype.LispSymbol('SETQ'),
                   lisptype.lispCons(place,
                       lisptype.lispCons(cons_call, lisptype.NIL)))

    raise lisptype.LispNotImplementedError(f"PUSH: unsupported place form: {place}")

# Mark as a proper macro callable and register it so (MACRO-FUNCTION 'PUSH) returns it.
_push_expander.__is_macro__ = True
_registry.function_registry['PUSH'] = _registry.RegistryEntry(
    name='PUSH',
    py_name='_push_expander',
    kind='macro',
    func=_push_expander,
)


def eval_push(form, env):
    """Evaluate PUSH special form.

    (PUSH item place) — Prepend item to the list stored in place, and store
    the result back in place. CLHS 5.1.3: item is evaluated before place's
    subforms (pinned by `push.order.1`-`.3`); `_place_accessor` (shared with
    ROTATEF/SHIFTF) evaluates those subforms exactly once, so any place kind
    it supports works here too, not just a bare variable.
    """
    from .evaluation_core import eval

    args = cdr(form)
    if not _consp_internal(args) or not _consp_internal(cdr(args)):
        raise lisptype.LispNotImplementedError("PUSH requires an item and a place argument")

    item_form = car(args)
    place = car(cdr(args))

    item = eval(item_form, env)
    getter, setter = _place_accessor(place, env)
    new_value = cons(item, getter())
    setter(new_value)
    return new_value


def eval_pushnew(form, env):
    """Evaluate PUSHNEW special form.

    (PUSHNEW item place &key key test test-not) — CLHS 5.1.3 defines this
    directly in terms of ADJOIN: `(setf place (adjoin item place ...))`,
    with item evaluated first, then place's subforms, then the keyword
    arguments left to right as written (pinned by `pushnew.order.1`-`.3`
    and `pushnew.12`-`.15`).

    Previously a `cl_function` (`sequences_higher.pushnew`) that received
    `place` already evaluated to a value, so it only ever worked when place
    was a plain Python-list variable, and it ignored :test/:key/:test-not
    entirely -- the largest 100%-failing file in the suite (plan.md C16).
    """
    from .evaluation_core import eval
    from .sequences_higher import adjoin

    args = cdr(form)
    if not _consp_internal(args) or not _consp_internal(cdr(args)):
        raise lisptype.LispNotImplementedError("PUSHNEW requires an item and a place argument")

    item_form = car(args)
    place = car(cdr(args))
    kw_forms = cdr(cdr(args))

    item = eval(item_form, env)
    getter, setter = _place_accessor(place, env)
    old_value = getter()

    test = test_not = key = None
    cur = kw_forms
    while _consp_internal(cur):
        kw = car(cur)
        cur = cdr(cur)
        if not _consp_internal(cur):
            raise lisptype.LispError("PUSHNEW: odd number of keyword arguments")
        value = eval(car(cur), env)
        cur = cdr(cur)
        kw_name = kw.name if isinstance(kw, lisptype.LispSymbol) else None
        if kw_name == 'TEST':
            test = value
        elif kw_name == 'TEST-NOT':
            test_not = value
        elif kw_name == 'KEY':
            key = value

    new_value = adjoin(item, old_value, test=test, test_not=test_not, key=key)
    setter(new_value)
    return new_value


def _place_accessor(place_form, env):
    """Evaluate a place form's shared subforms exactly once and return a
    (get, set) pair of closures for reading/writing it.

    Supports plain variables and (CAR x), (CDR x), (AREF arr idx),
    (SVREF arr idx), (GETF plist indicator [default]) place forms --
    enough for ROTATEF/SHIFTF/PUSH/PUSHNEW/INCF's common cases. Other
    place kinds raise LispNotImplementedError.
    """
    from .evaluation_core import eval

    if isinstance(place_form, lisptype.LispSymbol):
        sym = place_form
        return (lambda: eval(sym, env), lambda v: env.set_variable(sym, v))

    if _consp_internal(place_form) and isinstance(car(place_form), lisptype.LispSymbol):
        op_name = car(place_form).name
        place_args = cdr(place_form)

        if op_name in ('CAR', 'FIRST') and _consp_internal(place_args):
            target = eval(car(place_args), env)
            if not _consp_internal(target):
                raise lisptype.LispError(f"{op_name} place: target is not a cons")
            return (lambda: target.car, lambda v: setattr(target, 'car', v))

        if op_name in ('CDR', 'REST') and _consp_internal(place_args):
            target = eval(car(place_args), env)
            if not _consp_internal(target):
                raise lisptype.LispError(f"{op_name} place: target is not a cons")
            return (lambda: target.cdr, lambda v: setattr(target, 'cdr', v))

        if _CXR_RE.match(op_name) and _consp_internal(place_args):
            obj = eval(car(place_args), env)
            parent, is_car = _cxr_target(op_name, obj)
            if is_car:
                return (lambda: parent.car, lambda v: setattr(parent, 'car', v))
            return (lambda: parent.cdr, lambda v: setattr(parent, 'cdr', v))

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
                        return
                    current = cdr(cdr(current))
                plist_setter(lisptype.lispCons(indicator, lisptype.lispCons(v, plist)))

            return (_getf_get, _getf_set)

        if _arrays.is_array_place(op_name) and _consp_internal(place_args):
            from .evaluation_core import _eval_args
            values = _eval_args(place_args, env)
            return (lambda: _arrays.array_place_read(op_name, values),
                    lambda v: _arrays.array_place_write(op_name, values, v))

        # Not a place op this function knows directly -- it may still be a
        # macro call (e.g. ansi-aux's `expand-in-current-env`, which exists
        # specifically so a MACROLET-local macro expands in the *caller's*
        # lexical environment). CLHS 5.1.3 requires place resolution to see
        # through macroexpansion; `pushnew.21`/`push.5` are exactly a place
        # that is a macro form.
        from .misc_packages import _direct_macroexpand_1
        expanded, did_expand = _direct_macroexpand_1(place_form, env)
        if did_expand:
            return _place_accessor(expanded, env)

    raise lisptype.LispNotImplementedError(f"place not supported: {place_form}")


def eval_rotatef(form, env):
    """Evaluate ROTATEF special form.

    (ROTATEF place*) — Evaluates each place's shared subforms exactly
    once (left to right), then rotates their values: place[i] gets the
    old value of place[i+1], with the last place getting the first
    place's old value. Always returns NIL.
    """
    args = cdr(form)

    places = []
    current = args
    while _consp_internal(current):
        places.append(car(current))
        current = cdr(current)

    if not places:
        return lisptype.NIL

    accessors = [_place_accessor(p, env) for p in places]
    old_values = [get() for get, _ in accessors]

    n = len(accessors)
    for i in range(n):
        accessors[i][1](old_values[(i + 1) % n])

    return lisptype.NIL


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
                # Try to look up the class
                sc_class = fclpy.classes.find_class(sc.name)
                if sc_class is not None:
                    superclasses_list.append(sc_class)
                else:
                    # If not found as a class, treat as forward reference
                    superclasses_list.append(sc)
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
    if _consp_internal(options_rest):
        current = options_rest
        while _consp_internal(current):
            option = car(current)
            if _consp_internal(option):
                opt_key = car(option)
                if isinstance(opt_key, lisptype.lispKeyword):
                    opt_name = opt_key.name.lower()
                    opt_vals = cdr(option)
                    if opt_name == 'metaclass' and _consp_internal(opt_vals):
                        metaclass = car(opt_vals)
                    elif opt_name == 'documentation' and _consp_internal(opt_vals):
                        documentation = car(opt_vals)
            current = cdr(current)
    
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
    )
    
    return class_name


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
    """Parse a specialized-lambda-list into (params, specializers) (CLHS
    7.6.2). Only required parameters may be specialized; anything at or
    after a lambda-list keyword (&optional/&rest/&key/...) is bound
    unspecialized, matching this codebase's existing (positional-only)
    method parameter binding in `_make_method_function` -- the lambda-list
    keyword symbol itself is not a parameter and is dropped rather than
    bound, and an &optional/&key parameter's own `(var default...)` form is
    unwrapped down to `var` rather than passed through whole (the defect
    this replaced: binding the literal cons `(Y 10)` as if it were the
    parameter name crashed the first time DEFMETHOD in a real ANSI test
    used &optional, since `add_variable` requires a symbol).
    """
    params = []
    specializers = []
    current = specialized_lambda_list
    seen_lambda_key = False
    while _consp_internal(current):
        param_spec = car(current)
        if isinstance(param_spec, lisptype.LispSymbol) and param_spec.name.startswith('&'):
            seen_lambda_key = True
            current = cdr(current)
            continue
        if seen_lambda_key:
            params.append(_lambda_list_param_name(param_spec))
        elif _consp_internal(param_spec):
            param_name = car(param_spec)
            param_type = car(cdr(param_spec))
            params.append(param_name)
            specializers.append(_resolve_specializer(param_type, env))
        else:
            params.append(param_spec)
            specializers.append(None)
        current = cdr(current)
    return params, specializers


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


def _make_method_function(params, body, captured_env, block_name):
    """Build the callable behind one CLOS method (shared by DEFGENERIC's inline
    :method options and standalone DEFMETHOD -- these used to be two copies of
    identical logic).

    CLHS 7.6.5: each method has an implicit block named after its generic
    function, so a bare (RETURN-FROM gf-name ...) inside the method body
    returns from the method rather than escaping further out.
    """
    from .evaluation_loops_conditionals import _run_with_nil_block
    from .evaluation_core import eval

    def method_func(*call_args):
        method_env = lisptype.Environment(captured_env)
        for i, param in enumerate(params):
            if i < len(call_args):
                method_env.add_variable(param, call_args[i])
            else:
                method_env.add_variable(param, lisptype.NIL)

        def _run_body():
            result = lisptype.NIL
            body_current = body
            while _consp_internal(body_current):
                result = eval(car(body_current), method_env)
                body_current = cdr(body_current)
            return result

        return _run_with_nil_block(_run_body, block_name)
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

    if not isinstance(func_name, lisptype.LispSymbol):
        raise lisptype.LispProgramError("DEFGENERIC: function name must be a symbol")

    if not _consp_internal(rest):
        raise lisptype.LispProgramError("DEFGENERIC requires a lambda-list")

    lambda_list = car(rest)
    options = cdr(rest)

    documentation = None
    method_combination = None
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
            params, specializers = _parse_specialized_lambda_list(specialized_lambda_list, env)
            method_specs.append((qualifiers, specializers, params, method_body))
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
            _check_argument_precedence_order(func_name, lambda_list, cdr(option))
        elif opt_name in ('GENERIC-FUNCTION-CLASS', 'METHOD-CLASS', 'DECLARE'):
            # Accepted and recorded nowhere yet: this implementation has one
            # generic-function class and one method class, so there is
            # nothing for them to select. They are not errors, though, and
            # must not fall through to the unknown-option branch.
            pass
        else:
            raise lisptype.LispProgramError(
                f"DEFGENERIC {func_name.name}: unknown option {car(option)}")

    gf = classes.ensure_generic_function(func_name, documentation=documentation, lambda_list=lambda_list)
    gf.method_combination = method_combination

    for qualifiers, specializers, params, method_body in method_specs:
        method_fn = _make_method_function(params, method_body, env, func_name)
        classes.add_method(gf, specializers, method_fn, qualifiers=qualifiers)

    global_env = env
    while global_env.parent is not None:
        global_env = global_env.parent
    global_env.add_function(func_name, gf)

    return func_name


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

    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("DEFMETHOD requires at least a name")

    func_name = car(args)
    rest = cdr(args)

    if not isinstance(func_name, lisptype.LispSymbol):
        raise lisptype.LispNotImplementedError("DEFMETHOD: function name must be a symbol")

    qualifiers, specialized_lambda_list, method_body = _parse_defmethod_tail(rest)
    params, specializers = _parse_specialized_lambda_list(specialized_lambda_list, env)

    method_fn = _make_method_function(params, method_body, env, func_name)

    gf = classes.ensure_generic_function(func_name)
    classes.add_method(gf, specializers, method_fn, qualifiers=qualifiers)

    global_env = env
    while global_env.parent is not None:
        global_env = global_env.parent
    global_env.add_function(func_name, gf)

    return func_name


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
    'eval_pop',
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
