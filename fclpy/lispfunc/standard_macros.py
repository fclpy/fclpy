"""Standard Common Lisp macros as real macro expanders.

The evaluator (`evaluation_core.eval`) used to dispatch a long ladder of
hard-coded `operator.name == '...'` branches for macros that CLHS defines as
macros. Those branches meant `(macro-function 'when)` answered NIL and
MACROEXPAND could not see the macro -- and each branch was a second copy of
semantics that already existed somewhere else. This module is the one home of
the converted macros: an expander here produces the equivalent expansion, the
macro IS the implementation, and the matching eval branch is deleted.

Every expander has the exact call shape of a CL macro function: it receives
the whole call form and the expansion-time environment, and nothing else. The
wrapper below enforces that arity -- the ansi-test `*.error.*` files call a
MACRO-FUNCTION result with zero, one and three arguments and require a
PROGRAM-ERROR each time, and `evaluation_core.is_arity_mismatch_message`
turns a TypeError with an arity-mismatch message into exactly that.
"""

import fclpy.lisptype as lisptype
import fclpy.state as state
from fclpy.lispfunc import registry as _registry
from fclpy.lispfunc.core import car, cdr, cons, _consp_internal

# The function registry is write-last-wins. evaluation_stubs (ECASE) and
# misc_macros (MULTIPLE-VALUE-BIND, STEP, DECLAIM, ...) register cl_function
# entries for some of the names below, and fclpy.lispfunc's own import order
# reaches them *after* this module -- forcing their import here, before the
# registrations further down, is what makes this module's macro entries the
# ones that survive.
import fclpy.lispfunc.evaluation_stubs as _evaluation_stubs  # noqa: F401
import fclpy.lispfunc.misc_macros as _misc_macros  # noqa: F401


def _sym(name):
    return lisptype.LispSymbol(name)


def _kw(name):
    """Create a keyword symbol (e.g., _kw('DATUM') -> :DATUM)."""
    return lisptype.lispKeyword(name)


def _quoted(x):
    return lisptype.lispCons(_sym('QUOTE'), lisptype.lispCons(x, lisptype.NIL))


def _cons_from(items):
    out = lisptype.NIL
    for item in reversed(items):
        out = lisptype.lispCons(item, out)
    return out


def _form_args(form):
    """The call form's argument forms, as a Python list."""
    args = []
    cur = cdr(form)
    while _consp_internal(cur):
        args.append(car(cur))
        cur = cdr(cur)
    return args


def _list(*items):
    return _cons_from(list(items))


def _gensym():
    from fclpy.lispfunc.utilities_symbols import gensym
    return gensym()


def _standard_macro(lisp_name, documentation=None):
    """Register `fn(form, env) -> expansion` as a real macro.

    The wrapper is what gets bound: it enforces the two-argument macro
    function shape (whole form + environment) so wrong-arity calls signal
    PROGRAM-ERROR, and normalizes a NIL/omitted environment designator to
    the current environment, which is what `(funcall (macro-function 'name)
    form nil)` means.
    """
    def decorator(fn):
        def expander(form, *rest):
            # The evaluator calls [whole-form, *raw-args, environment]; the
            # raw args are redundant (they are form's own tail) and are read
            # off `form` instead. The trailing environment is what tells a
            # real expansion call from a wrong-arity FUNCALL of the
            # MACRO-FUNCTION (zero, one or three arguments -- the ansi
            # *.error.* tests do all three and require a PROGRAM-ERROR): a
            # two-argument FUNCALL of (form nil) is the one non-evaluator
            # shape that is accepted, with NIL meaning the global
            # environment.
            env = rest[-1] if rest else None
            if isinstance(env, lisptype.Environment):
                pass
            elif len(rest) == 1 and (env is None or env is lisptype.NIL):
                env = state.current_environment
            else:
                raise TypeError(
                    f"{fn.__name__}() takes exactly 2 positional arguments "
                    f"({len(rest)} given)")
            return fn(form, env)

        expander.__name__ = fn.__name__
        expander.__doc__ = fn.__doc__
        expander.__is_macro__ = True
        expander.__expects_whole__ = True
        expander.__expects_environment__ = True
        # Carried through from the wrapped expander: `_reuse_definer` sets it,
        # and `misc_packages.macro_expansion_evaluates` reads it off the bound
        # macro function, which is this wrapper and not `fn`.
        expander.__runs_body__ = getattr(fn, '__runs_body__', False)
        _registry.cl_macro(lisp_name, documentation=documentation)(expander)
        return fn
    return decorator


# ---------------------------------------------------------------------------
# WHEN / UNLESS (CLHS 5.3)
# ---------------------------------------------------------------------------

@_standard_macro('WHEN')
def _when_expander(form, env):
    """(when test body...) -> (if test (progn body...) nil).

    The explicit NIL else-branch is what makes `(when nil ...)` answer NIL
    as a value rather than eval_if's Python-None missing-else result.
    """
    args = _form_args(form)
    if not args:
        raise lisptype.LispProgramError("WHEN requires a test form")
    return _cons_from([_sym('IF'), args[0],
                       _cons_from([_sym('PROGN')] + args[1:]),
                       lisptype.NIL])


@_standard_macro('UNLESS')
def _unless_expander(form, env):
    """(unless test body...) -> (if test nil (progn body...))."""
    args = _form_args(form)
    if not args:
        raise lisptype.LispProgramError("UNLESS requires a test form")
    return _cons_from([_sym('IF'), args[0], lisptype.NIL,
                       _cons_from([_sym('PROGN')] + args[1:])])


# ---------------------------------------------------------------------------
# Multiple values and sequencing (CLHS 5.1.3, 5.3)
# ---------------------------------------------------------------------------

@_standard_macro('PROG1')
def _prog1_expander(form, env):
    """(prog1 first body...) -> (let ((#:g first)) body... #:g).

    The LET temp is what returns the *primary* value of `first` while the
    remaining forms still run for effect.
    """
    args = _form_args(form)
    if not args:
        raise lisptype.LispProgramError("PROG1 requires at least one form")
    tmp = _gensym()
    return _cons_from([_sym('LET'), _cons_from([_list(tmp, args[0])])]
                      + args[1:] + [tmp])


@_standard_macro('PROG2')
def _prog2_expander(form, env):
    """(prog2 first second body...) -> (progn first (let ((#:g second)) body... #:g))."""
    args = _form_args(form)
    if len(args) < 2:
        raise lisptype.LispProgramError("PROG2 requires at least two forms")
    tmp = _gensym()
    return _cons_from([_sym('PROGN'), args[0],
                       _cons_from([_sym('LET'), _cons_from([_list(tmp, args[1])])]
                                  + args[2:] + [tmp])])


@_standard_macro('NTH-VALUE')
def _nth_value_expander(form, env):
    """(nth-value n form) -> (nth n (multiple-value-list form)), with the
    pre-conversion special form's contract preserved: a *negative* n
    answers NIL. CLHS leaves a negative n undefined; NTH itself signals
    through NTHCDR, and the unit suite pins NIL (test_nth_value_negative).
    n is still evaluated first, as NTH's own left-to-right order does."""
    args = _form_args(form)
    if len(args) != 2:
        raise lisptype.LispProgramError("NTH-VALUE requires two arguments")
    n_tmp = _gensym()
    tmp = _gensym()
    return _cons_from([
        _sym('LET'),
        _cons_from([_list(n_tmp, args[0]),
                    _list(tmp, _cons_from([_sym('MULTIPLE-VALUE-LIST'), args[1]]))]),
        _cons_from([_sym('IF'), _list(_sym('<'), n_tmp, 0), lisptype.NIL,
                    _list(_sym('NTH'), n_tmp, tmp)])])


@_standard_macro('MULTIPLE-VALUE-LIST')
def _multiple_value_list_expander(form, env):
    """(multiple-value-list form) -> (multiple-value-call #'list form)."""
    args = _form_args(form)
    if len(args) != 1:
        raise lisptype.LispProgramError(
            "MULTIPLE-VALUE-LIST requires exactly one form")
    return _cons_from([_sym('MULTIPLE-VALUE-CALL'),
                       _cons_from([_sym('FUNCTION'), _sym('LIST')]),
                       args[0]])


@_standard_macro('MULTIPLE-VALUE-BIND')
def _multiple_value_bind_expander(form, env):
    """(multiple-value-bind (vars...) value-form body...)
    -> (let ((#:g (multiple-value-list value-form)))
         (let ((var1 (nth 0 #:g)) (var2 (nth 1 #:g)) ...) body...)).

    A dotted variable list's tail is bound to the remaining values as a
    list, via NTHCDR. The inner LET is what gives the body's declarations
    (e.g. `(declare (special x))`) their bindings to govern.
    """
    args = _form_args(form)
    if len(args) < 2:
        raise lisptype.LispProgramError(
            "MULTIPLE-VALUE-BIND requires a variable list and a value form")
    vars_form, value_form = args[0], args[1]
    body = args[2:]

    value_tmp = _gensym()
    var_items = []
    cur = vars_form
    dotted_tail = None
    while _consp_internal(cur):
        var_items.append(car(cur))
        cur = cdr(cur)
    if cur is not lisptype.NIL and cur is not None and not _consp_internal(cur):
        dotted_tail = cur

    bindings = []
    for i, var in enumerate(var_items):
        bindings.append(_list(var, _list(_sym('NTH'), i, value_tmp)))
    if dotted_tail is not None:
        bindings.append(_list(dotted_tail,
                              _list(_sym('NTHCDR'), len(var_items), value_tmp)))
    if not body:
        body = [lisptype.NIL]
    return _cons_from([
        _sym('LET'),
        _cons_from([_list(value_tmp,
                          _cons_from([_sym('MULTIPLE-VALUE-LIST'), value_form]))]),
        _cons_from([_sym('LET'), _cons_from(bindings)] + body),
    ])


def _resolve_place_entry(place_form, env):
    """Resolve one assignment target at expansion time.

    Returns ('var', store_var, store_form) for a plain variable (including
    one reached through a symbol-macro chain) and
    ('place', temps, vals, stores, store_form) for a compound place.
    """
    temps, vals, stores, store_form = _place_expansion(place_form, env)
    if not temps and not vals and len(stores) == 1:
        return ('var', stores[0], store_form)
    return ('place', temps, vals, stores, store_form)


def _place_full(place_form, env):
    """(temps, vals, stores, store_form, access_form) for an assignment target.

    The five-tuple GET-SETF-EXPANSION answers, with the same three special
    cases `_place_expansion` handles: a bare variable -- even one a local
    `(declare (special x))` redirects through the `(%SPECIAL-REF x)`
    symbol-macro -- is read directly and stored with `(SETQ place s)`; a
    `%SPECIAL-REF` place stores the same way. Everything else goes to
    `get_setf_expansion` (CLHS 5.1.2.1), whose store form already binds
    `stores` -- which is what lets INCF/PUSH/ROTATEF's expansions bind those
    same variables to their new values and run the store form unchanged.
    """
    from .evaluation_special_forms import get_setf_expansion

    place = place_form
    while isinstance(place, lisptype.LispSymbol):
        expansion = env.get_symbol_macro(place)
        if expansion is None:
            break
        place = expansion

    if isinstance(place, lisptype.LispSymbol):
        store = _gensym()
        return [], [], [store], _list(_sym('SETQ'), place, store), place

    if (_consp_internal(place) and isinstance(car(place), lisptype.LispSymbol)
            and car(place).name == '%SPECIAL-REF'):
        store = _gensym()
        return [], [], [store], _list(_sym('SETQ'), place_form, store), place_form

    temps, vals, stores, store_form, access = get_setf_expansion(place, env)
    return temps, vals, stores, store_form, access


def _place_expansion(place_form, env):
    """(temps, vals, stores, store_form) for an assignment target.

    A bare variable -- even one a local `(declare (special x))` redirects
    through the `(%SPECIAL-REF x)` symbol-macro -- is stored with `(SETQ
    place s)`: SETQ re-resolves symbol-macros at runtime and lands the
    value in the right cell. Handing such a target to
    `get_setf_expansion` instead would resolve `%SPECIAL-REF` into the
    generic `(funcall #'(setf %special-ref) ...)` fallback, for which no
    writer function exists.
    """
    temps, vals, stores, store_form, _access = _place_full(place_form, env)
    return temps, vals, stores, store_form


@_standard_macro('MULTIPLE-VALUE-SETQ')
def _multiple_value_setq_expander(form, env):
    """(multiple-value-setq (vars...) form) -> assign vars from form's values.

    -> (let* ((t1 v1)...          every place's subforms first, left to right
              (#:g (multiple-value-list form))          then the value form
              (s1 (nth 0 #:g)) ...)                     then the store values
         <store-form 1> ... <store-form n> (nth 0 #:g))

    Each var consumes as many of form's values as its place has store
    variables, assignments happen in source order, and the form's primary
    value is returned.
    """
    args = _form_args(form)
    if len(args) != 2:
        raise lisptype.LispProgramError(
            "MULTIPLE-VALUE-SETQ requires a variable list and a value form")
    vars_form, value_form = args

    entries = []
    cur = vars_form
    while _consp_internal(cur):
        entries.append(_resolve_place_entry(car(cur), env))
        cur = cdr(cur)

    bindings = []
    for entry in entries:
        if entry[0] == 'place':
            _, temps, vals, _stores, _sf = entry
            for t, v in zip(temps, vals):
                bindings.append(_list(t, v))
    value_tmp = _gensym()
    bindings.append(_list(value_tmp,
                          _cons_from([_sym('MULTIPLE-VALUE-LIST'), value_form])))

    body_forms = []
    idx = 0
    for entry in entries:
        if entry[0] == 'var':
            _, store, store_form = entry
            bindings.append(_list(store,
                                  _list(_sym('NTH'), idx, value_tmp)))
            idx += 1
        else:
            _, _temps, _vals, stores, store_form = entry
            for j, s in enumerate(stores):
                bindings.append(_list(s,
                                      _list(_sym('NTH'), idx + j, value_tmp)))
            idx += len(stores)
        body_forms.append(store_form)

    return _cons_from([_sym('LET*'), _cons_from(bindings)]
                      + body_forms
                      + [_list(_sym('NTH'), 0, value_tmp)])


@_standard_macro('PSETQ')
def _psetq_expander(form, env):
    """(psetq var1 form1 var2 form2 ...) -> parallel assignment, returns NIL.

    -> (let* ((t1 v1)... (#!g1 form1) (t2 v2)... (#!g2 form2) ...
              (s1 #!g1) (s2 #!g2) ...)
         <store-form 1> ... <store-form n> nil)

    CLHS 5.1.3's left-to-right interleave is preserved: each var's place
    subforms are evaluated, then that var's value form, then the next
    var's place (psetq.7) -- and every assignment happens only after all
    the value forms have run (psetq.3).
    """
    from .evaluation_special_forms import get_setf_expansion
    args = _form_args(form)
    if len(args) % 2 != 0:
        raise lisptype.LispProgramError(
            "PSETQ requires an even number of argument forms")

    bindings = []
    body_forms = []
    for i in range(0, len(args), 2):
        var, value_form = args[i], args[i + 1]
        if not isinstance(var, lisptype.LispSymbol):
            raise lisptype.LispProgramError("PSETQ: variable must be a symbol")
        temps, vals, stores, store_form = _place_expansion(var, env)
        for t, v in zip(temps, vals):
            bindings.append(_list(t, v))
        value_tmp = _gensym()
        bindings.append(_list(value_tmp,
                              _cons_from([_sym('MULTIPLE-VALUE-LIST'),
                                          value_form])))
        for j, s in enumerate(stores):
            bindings.append(_list(s,
                                  _list(_sym('NTH'), j, value_tmp)))
        body_forms.append(store_form)

    if not body_forms:
        return lisptype.NIL
    return _cons_from([_sym('LET*'), _cons_from(bindings)]
                      + body_forms + [lisptype.NIL])


# ---------------------------------------------------------------------------
# AND / OR (CLHS 5.3)
# ---------------------------------------------------------------------------

@_standard_macro('AND')
def _and_expander(form, env):
    """(and form...) -> (if f1 (and f2...) nil); a one-argument AND is the
    form itself and the empty AND is T.

    IF is the whole mechanism, and it is the right one: its test is a
    single-value context (the truthiness the pre-conversion branch applied
    to every non-final form), while its branches return whatever values the
    selected form yields -- so `(and (values 1 nil) (values nil 2))` still
    answers NIL and 2 (AND.8) and `(and (values 'a 'b 'c))` still answers
    all three values (AND.4). The explicit NIL else-branch is the same
    guard WHEN's expander has: without it a falsy first form would let
    eval_if's missing-else Python None surface as the AND's value.
    """
    args = _form_args(form)
    if not args:
        return lisptype.T
    if len(args) == 1:
        return args[0]
    return _cons_from([_sym('IF'), args[0],
                       _cons_from([_sym('AND')] + args[1:]),
                       lisptype.NIL])


@_standard_macro('OR')
def _or_expander(form, env):
    """(or form...) -> (let ((#:g f1)) (if #:g #:g (or f2...))).

    The gensym is what evaluates the first form exactly once, and it is
    also what pins OR.6's contract: a form that yields *true* contributes
    only its primary value (bound into the gensym), while a form that is
    the OR's last one is returned untouched -- all of its values (OR.4,
    OR.7, OR.9). The empty OR is NIL.
    """
    args = _form_args(form)
    if not args:
        return lisptype.NIL
    if len(args) == 1:
        return args[0]
    tmp = _gensym()
    return _cons_from([_sym('LET'), _cons_from([_list(tmp, args[0])]),
                       _cons_from([_sym('IF'), tmp, tmp,
                                   _cons_from([_sym('OR')] + args[1:])])])


# ---------------------------------------------------------------------------
# COND (CLHS 5.3)
# ---------------------------------------------------------------------------

@_standard_macro('COND')
def _cond_expander(form, env):
    """(cond clause...) -> nested IF, with no TAGBODY anywhere near it.

    A clause with body forms becomes `(if test (progn body...) <rest>)`; a
    clause with *no* body forms becomes `(let ((#:g test)) (if #:g #:g
    <rest>))` -- the gensym evaluates the test exactly once and answers its
    primary value (COND.9: `(cond ((values 'a 'b 'c)))` is A, not all three
    values), because the LET's binding is the single-value context CLHS
    5.3's "the value of the test" means here. The expansion contains no
    TAGBODY, so a `(go 10)` inside a clause body still finds the
    *program's* enclosing TAGBODY rather than one the macro introduced
    (COND.15, the "no implicit tagbody" rule).
    """
    args = _form_args(form)
    if not args:
        return lisptype.NIL

    def rest_form(clauses):
        return _cons_from([_sym('COND')] + clauses) if clauses else lisptype.NIL

    def build(clauses):
        clause = clauses[0]
        rest = clauses[1:]
        if not _consp_internal(clause):
            raise lisptype.LispProgramError(
                "COND: each clause must be a list")
        test = car(clause)
        body = []
        cur = cdr(clause)
        while _consp_internal(cur):
            body.append(car(cur))
            cur = cdr(cur)
        tail = rest_form(rest)
        if not body:
            tmp = _gensym()
            return _cons_from([_sym('LET'), _cons_from([_list(tmp, test)]),
                               _cons_from([_sym('IF'), tmp, tmp, tail])])
        return _cons_from([_sym('IF'), test,
                           _cons_from([_sym('PROGN')] + body),
                           tail])

    return build(args)


# ---------------------------------------------------------------------------
# CASE, CCASE, ECASE, TYPECASE, CTYPECASE, ETYPECASE (CLHS 5.2)
# ---------------------------------------------------------------------------

def _is_catchall_name(keys):
    """Is `keys` the T or OTHERWISE designator of a CASE/TYPECASE catch-all?"""
    return (isinstance(keys, lisptype.LispSymbol)
            and keys.name.upper() in ('T', 'OTHERWISE'))


def _is_empty_keys_designator(keys):
    """A bare NIL in the keys position designates an *empty* list of keys
    (CLHS 5.2's list designator rule), never a singleton NIL key -- so
    `(case nil (nil 'a) (t 'b))` answers B (case.6)."""
    return (keys is lisptype.NIL or keys is None
            or (isinstance(keys, lisptype.LispSymbol)
                and keys.name.upper() == 'NIL'))


def _case_clauses(args, operator):
    """Parse `args` into (keys-or-type, body-forms) pairs, unevaluated."""
    clauses = []
    for clause in args:
        if not _consp_internal(clause):
            raise lisptype.LispProgramError(
                f"{operator}: each clause must be a list")
        keys = car(clause)
        body = []
        cur = cdr(clause)
        while _consp_internal(cur):
            body.append(car(cur))
            cur = cdr(cur)
        clauses.append((keys, body))
    return clauses


def _body_form(body):
    return _cons_from([_sym('PROGN')] + body) if body else lisptype.NIL


def _eql_cond_clauses(clauses, tmp, allow_catchall, all_keys):
    """COND clauses matching `tmp` with EQL over each clause's keys."""
    out = []
    for keys, body in clauses:
        body_form = _body_form(body)
        if allow_catchall and _is_catchall_name(keys):
            out.append(_cons_from([lisptype.T, body_form]))
            continue
        if _is_empty_keys_designator(keys):
            continue
        if _consp_internal(keys):
            cur = keys
            while _consp_internal(cur):
                all_keys.append(car(cur))
                cur = cdr(cur)
            test = _list(_sym('MEMBER'), tmp, _quoted(keys))
        else:
            all_keys.append(keys)
            test = _list(_sym('EQL'), tmp, _quoted(keys))
        out.append(_cons_from([test, body_form]))
    return out


def _typep_cond_clauses(clauses, tmp, allow_catchall, all_types):
    """COND clauses matching `tmp` with TYPEP against each type-spec."""
    out = []
    for type_spec, body in clauses:
        body_form = _body_form(body)
        if allow_catchall and _is_catchall_name(type_spec):
            out.append(_cons_from([lisptype.T, body_form]))
            continue
        all_types.append(type_spec)
        out.append(_cons_from([_list(_sym('TYPEP'), tmp, _quoted(type_spec)),
                               body_form]))
    return out


def _member_type_form(keys):
    """The `(MEMBER k1 k2 ...)` type over every key, as a quoted literal."""
    return _quoted(_cons_from([_sym('MEMBER')] + list(keys)))


def _or_type_form(type_specs):
    return _quoted(_cons_from([_sym('OR')] + list(type_specs)))


def _type_error_form(tmp, expected_type):
    return _list(_sym('ERROR'), _quoted(_sym('TYPE-ERROR')),
                 _kw('DATUM'), tmp,
                 _kw('EXPECTED-TYPE'), expected_type)


def _keyform_form(key_form, tmp, cond_clauses):
    return _cons_from(
        [_sym('LET'), _cons_from([_list(tmp, key_form)]),
         _cons_from([_sym('COND')] + cond_clauses)
         if cond_clauses else lisptype.NIL])


def _keyplace_form(operator, place, cond_clauses, expected_type, env, tmp=None):
    """CCASE/CTYPECASE: match, and on no match signal a *correctable*
    TYPE-ERROR whose STORE-VALUE restart stores back into the place and
    retries -- the protocol ccase.31/ctypecase.12 exercise through
    `(store-value new c)` in a HANDLER-BIND. The place's subforms run once
    (ccase.25), the place is *re-read* on every retry, and the store
    variables are the ones GET-SETF-EXPANSION's own store form names, so
    arbitrary places work unchanged."""
    temps, vals, stores, store_form, access = _place_full(place, env)
    if tmp is None:
        tmp = _gensym()
    retry_tag = _gensym()
    store_tmp = _gensym()

    store_bindings = _cons_from(
        [_list(stores[0], store_tmp)] if stores else [])
    store_action = _cons_from([_sym('LET'), store_bindings, store_form]) \
        if stores else store_form

    inner = _cons_from(
        [_sym('LET'), _cons_from([_list(tmp, access)]),
         _cons_from([_sym('COND')]
                    + cond_clauses
                    + [_cons_from(
                        [lisptype.T,
                         _cons_from(
                             [_sym('RESTART-CASE'),
                              _type_error_form(tmp, expected_type),
                              _cons_from(
                                  [_sym('STORE-VALUE'),
                                   _cons_from([store_tmp]),
                                   _cons_from([store_action,
                                               _list(_sym('GO'), retry_tag)])])])])])])
    bindings = _cons_from([_list(t, v) for t, v in zip(temps, vals)])
    block_tag = _gensym()
    return _cons_from([_sym('LET*'), bindings,
                       _cons_from([_sym('BLOCK'), block_tag,
                                   _cons_from([_sym('TAGBODY'),
                                               retry_tag,
                                               _list(_sym('RETURN-FROM'), block_tag, inner)])])])


@_standard_macro('CASE')
def _case_expander(form, env):
    """(case keyform clause...) -> (let ((#:g keyform)) (cond ...)).

    Matching is EQL: a single key becomes `(eql #:g 'key)`, a keys *list*
    becomes `(member #:g '(k1 k2))` over the literal list, and a keys
    designator of T or OTHERWISE becomes the catch-all `(t body...)`. Keys
    are syntax -- the quote around each keys literal is what keeps them
    unevaluated. No clause matching just falls off the COND as NIL.
    """
    args = _form_args(form)
    if not args:
        raise lisptype.LispProgramError("CASE requires a keyform")
    clauses = _case_clauses(args[1:], 'CASE')
    all_keys = []
    tmp = _gensym()
    cond_clauses = _eql_cond_clauses(clauses, tmp, True, all_keys)
    return _keyform_form(args[0], tmp, cond_clauses)


@_standard_macro('ECASE')
def _ecase_expander(form, env):
    """(ecase keyform clause...) -> CASE matching that signals a TYPE-ERROR
    when nothing matches. T and OTHERWISE are ordinary keys here, not
    catch-alls, and the expected type is the MEMBER type over every key
    across all clauses."""
    args = _form_args(form)
    if not args:
        raise lisptype.LispProgramError("ECASE requires a keyform")
    clauses = _case_clauses(args[1:], 'ECASE')
    all_keys = []
    tmp = _gensym()
    cond_clauses = _eql_cond_clauses(clauses, tmp, False, all_keys)
    cond_clauses.append(_cons_from(
        [lisptype.T,
         _type_error_form(tmp, _member_type_form(all_keys))]))
    return _keyform_form(args[0], tmp, cond_clauses)


@_standard_macro('CCASE')
def _ccase_expander(form, env):
    """(ccase keyplace clause...) -> ECASE matching whose TYPE-ERROR is
    correctable: the STORE-VALUE restart stores into the place and retries
    the match."""
    args = _form_args(form)
    if not args:
        raise lisptype.LispProgramError("CCASE requires a keyplace")
    clauses = _case_clauses(args[1:], 'CCASE')
    all_keys = []
    tmp = _gensym()
    cond_clauses = _eql_cond_clauses(clauses, tmp, False, all_keys)
    return _keyplace_form('CCASE', args[0], cond_clauses,
                          _member_type_form(all_keys), env, tmp)


@_standard_macro('TYPECASE')
def _typecase_expander(form, env):
    """(typecase keyform clause...) -> (let ((#:g keyform)) (cond ...)),
    each clause's test being `(typep #:g 'type-spec)`. Type-specs are
    syntax, and T/OTHERWISE are catch-alls; no match answers NIL."""
    args = _form_args(form)
    if not args:
        raise lisptype.LispProgramError("TYPECASE requires a keyform")
    clauses = _case_clauses(args[1:], 'TYPECASE')
    all_types = []
    tmp = _gensym()
    cond_clauses = _typep_cond_clauses(clauses, tmp, True, all_types)
    return _keyform_form(args[0], tmp, cond_clauses)


@_standard_macro('ETYPECASE')
def _etypecase_expander(form, env):
    """(etypecase keyform clause...) -> TYPECASE matching that signals a
    TYPE-ERROR whose expected type is `(or t1 t2 ...)` when nothing
    matches. T and OTHERWISE are ordinary type-specs here."""
    args = _form_args(form)
    if not args:
        raise lisptype.LispProgramError("ETYPECASE requires a keyform")
    clauses = _case_clauses(args[1:], 'ETYPECASE')
    all_types = []
    tmp = _gensym()
    cond_clauses = _typep_cond_clauses(clauses, tmp, False, all_types)
    cond_clauses.append(_cons_from(
        [lisptype.T,
         _type_error_form(tmp, _or_type_form(all_types))]))
    return _keyform_form(args[0], tmp, cond_clauses)


@_standard_macro('CTYPECASE')
def _ctypecase_expander(form, env):
    """(ctypecase keyplace clause...) -> ETYPECASE matching whose TYPE-ERROR
    is correctable: the STORE-VALUE restart stores into the place and
    retries (ctypecase.12)."""
    args = _form_args(form)
    if not args:
        raise lisptype.LispProgramError("CTYPECASE requires a keyplace")
    clauses = _case_clauses(args[1:], 'CTYPECASE')
    all_types = []
    tmp = _gensym()
    cond_clauses = _typep_cond_clauses(clauses, tmp, False, all_types)
    return _keyplace_form('CTYPECASE', args[0], cond_clauses,
                          _or_type_form(all_types), env, tmp)


# ---------------------------------------------------------------------------
# Place-modifying macros (CLHS 5.1.3)
# ---------------------------------------------------------------------------

def _store_bindings(stores, value_form):
    """One (store-var value-form) binding per store variable. Every place
    expansion hands back more than one store variable only for a VALUES
    place, where each store variable receives the same form's primary
    value -- the same single-value write the pre-conversion setter did."""
    return [_list(s, value_form) for s in stores]


@_standard_macro('INCF')
def _incf_expander(form, env):
    """(incf place [delta]) -> (let* ((t v)... (#:d delta) (s (+ access #:d)))
    <store-form>).

    CLHS 5.1.3's own expansion, which is also what `incf.order.4` pins: the
    place's subforms run first, then delta, and only then is the place
    read -- `(incf x (setf x 1))` answers 2. The place is read through the
    GET-SETF-EXPANSION access form, so the subforms are evaluated exactly
    once (incf.order.2), and INCF's value is the store form's, i.e. the
    new value."""
    args = _form_args(form)
    if not args:
        raise lisptype.LispProgramError("INCF requires a place")
    if len(args) > 2:
        raise lisptype.LispProgramError("INCF requires at most two arguments")
    return _incdec_expansion(args[0], args[1] if len(args) == 2 else 1,
                             _sym('+'), env)


@_standard_macro('DECF')
def _decf_expander(form, env):
    """(decf place [delta]) -> INCF's expansion with - in place of +."""
    args = _form_args(form)
    if not args:
        raise lisptype.LispProgramError("DECF requires a place")
    if len(args) > 2:
        raise lisptype.LispProgramError("DECF requires at most two arguments")
    return _incdec_expansion(args[0], args[1] if len(args) == 2 else 1,
                             _sym('-'), env)


def _incdec_expansion(place, delta_form, op, env):
    temps, vals, stores, store_form, access = _place_full(place, env)
    delta = _gensym()
    bindings = [_list(t, v) for t, v in zip(temps, vals)]
    bindings.append(_list(delta, delta_form))
    bindings.extend(_store_bindings(
        stores, _list(op, access, delta)))
    return _cons_from([_sym('LET*'), _cons_from(bindings), store_form])


@_standard_macro('PUSH')
def _push_expander(form, env):
    """(push item place) -> (let* ((#:g item) (t v)... (s (cons #:g access)))
    <store-form>).

    `item` is evaluated *before* the place's subforms (push.order.1-.3),
    which is why its binding leads the LET*; the place is read through the
    access form once, and PUSH's value is the cons just stored."""
    args = _form_args(form)
    if len(args) != 2:
        raise lisptype.LispProgramError("PUSH requires an item and a place")
    temps, vals, stores, store_form, access = _place_full(args[1], env)
    item = _gensym()
    bindings = [_list(item, args[0])]
    bindings.extend(_list(t, v) for t, v in zip(temps, vals))
    bindings.extend(_store_bindings(stores, _list(_sym('CONS'), item, access)))
    return _cons_from([_sym('LET*'), _cons_from(bindings), store_form])


@_standard_macro('PUSHNEW')
def _pushnew_expander(form, env):
    """(pushnew item place &key key test test-not) -> PUSH's expansion with
    ADJOIN in place of CONS. The keyword forms are spliced into the ADJOIN
    call verbatim, so they are evaluated where CLHS 5.1.3 puts them: after
    the item and the place's subforms, left to right (pushnew.order.*)."""
    args = _form_args(form)
    if len(args) < 2:
        raise lisptype.LispProgramError("PUSHNEW requires an item and a place")
    item_form, place = args[0], args[1]
    kw_forms = args[2:]
    temps, vals, stores, store_form, access = _place_full(place, env)
    item = _gensym()
    adjoin_args = [item, access] + list(kw_forms)
    bindings = [_list(item, item_form)]
    bindings.extend(_list(t, v) for t, v in zip(temps, vals))
    bindings.extend(_store_bindings(stores, _cons_from(
        [_sym('ADJOIN')] + adjoin_args)))
    return _cons_from([_sym('LET*'), _cons_from(bindings), store_form])


@_standard_macro('POP')
def _pop_expander(form, env):
    """(pop place) -> (let* ((t v)... (#:old access) (s (cdr #:old)))
    <store-form> (car #:old)).

    The old value is read exactly once (pop.order.1) into one gensym: its
    CDR is what gets stored back and its CAR is POP's result -- so popping
    an empty place stores NIL and answers NIL (pop.2), never an error."""
    args = _form_args(form)
    if len(args) != 1:
        raise lisptype.LispProgramError("POP requires a place")
    temps, vals, stores, store_form, access = _place_full(args[0], env)
    old = _gensym()
    bindings = [_list(t, v) for t, v in zip(temps, vals)]
    bindings.append(_list(old, access))
    bindings.extend(_store_bindings(stores, _list(_sym('CDR'), old)))
    return _cons_from([_sym('LET*'), _cons_from(bindings),
                       store_form, _list(_sym('CAR'), old)])


@_standard_macro('REMF')
def _remf_expander(form, env):
    """(remf place indicator) -> walk the place's plist two cells at a time.

    The place's subforms run first and the indicator second (remf.order.1);
    the plist is read once, after both (remf.order.3). A matching pair is
    removed by *mutating* the tail before it (`(setf (cddr prev) (cddr
    cur))`), or, when the match is the first pair, by storing `(cddr cur)`
    back through the place's own store form -- the same two cases
    eval_remf handled. Returns T when a pair was removed, NIL when the
    walk ran off the end."""
    args = _form_args(form)
    if len(args) != 2:
        raise lisptype.LispProgramError("REMF requires a place and an indicator")
    temps, vals, stores, store_form, access = _place_full(args[0], env)
    ind = _gensym()
    plist = _gensym()
    cur = _gensym()
    prev = _gensym()
    store_tmp = _gensym()

    store_action = _cons_from(
        [_sym('LET'), _cons_from(_store_bindings(stores, _list(_sym('CDDR'), cur))),
         store_form]) if stores else store_form
    removed = _cons_from(
        [_sym('IF'), prev,
         _list(_sym('SETF'), _list(_sym('CDDR'), prev), _list(_sym('CDDR'), cur)),
         store_action])

    bindings = [_list(t, v) for t, v in zip(temps, vals)]
    bindings.append(_list(ind, args[1]))
    bindings.append(_list(plist, access))
    walk = _cons_from(
        [_sym('DO'), _cons_from([_list(cur, plist, _list(_sym('CDDR'), cur)),
                                 _list(prev, lisptype.NIL, cur)]),
         _cons_from([_list(_sym('ATOM'), cur), lisptype.NIL]),
         _cons_from([_sym('WHEN'), _list(_sym('EQ'), _list(_sym('CAR'), cur), ind),
                     _cons_from([_sym('RETURN'),
                                 _cons_from([_sym('PROGN'), removed,
                                             lisptype.T])])])])
    return _cons_from([_sym('LET*'), _cons_from(bindings), walk])


@_standard_macro('ROTATEF')
def _rotatef_expander(form, env):
    """(rotatef place*) -> (let* ((t v)... (g access)... (s g_next)...)
    <store-1> ... <store-n> nil).

    Every place's subforms run left to right, then every place is read -- in
    order, into one access gensym each -- then the stores happen, the order
    the pre-conversion branch used and what rotatef-order.1/.2 count. Place
    i receives place i+1's old value (the last place receiving place 1's),
    by binding each place's own store variables to the next access gensym
    and running its store form unchanged."""
    args = _form_args(form)
    entries = []
    bindings = []
    for place in args:
        temps, vals, stores, store_form, access = _place_full(place, env)
        bindings.extend(_list(t, v) for t, v in zip(temps, vals))
        tmp = _gensym()
        bindings.append(_list(tmp, access))
        entries.append((stores, store_form, tmp))
    body_forms = []
    for i, (stores, store_form, tmp) in enumerate(entries):
        next_tmp = entries[(i + 1) % len(entries)][2]
        bindings.extend(_store_bindings(stores, next_tmp))
        body_forms.append(store_form)
    body_forms.append(lisptype.NIL)
    return _cons_from([_sym('LET*'), _cons_from(bindings)] + body_forms)


@_standard_macro('SHIFTF')
def _shiftf_expander(form, env):
    """(shiftf place+ newvalue) -> (let* ((t v)... (g access)...
    (#:new newvalue) (s g_next)...) <store-1> ... <store-n> g1).

    Every place's subforms run, then every place is read (in order, into
    one access gensym each), and only then is `newvalue` evaluated
    (shiftf-order.1/.2). Place i receives place i+1's old value, the last
    place receives `newvalue`, and SHIFTF's value is place 1's old value --
    the first access gensym."""
    args = _form_args(form)
    if len(args) < 2:
        raise lisptype.LispProgramError(
            "SHIFTF requires at least one place and a new-value form")
    place_args, value_form = args[:-1], args[-1]
    entries = []
    bindings = []
    for place in place_args:
        temps, vals, stores, store_form, access = _place_full(place, env)
        bindings.extend(_list(t, v) for t, v in zip(temps, vals))
        tmp = _gensym()
        bindings.append(_list(tmp, access))
        entries.append((stores, store_form, tmp))
    new_value = _gensym()
    bindings.append(_list(new_value, value_form))
    body_forms = []
    for i, (stores, store_form, tmp) in enumerate(entries):
        value_form_i = (entries[i + 1][2] if i + 1 < len(entries)
                        else new_value)
        bindings.extend(_store_bindings(stores, value_form_i))
        body_forms.append(store_form)
    body_forms.append(entries[0][2])
    return _cons_from([_sym('LET*'), _cons_from(bindings)] + body_forms)


# ---------------------------------------------------------------------------
# TIME, STEP, IGNORE-ERRORS, DECLAIM, LAMBDA (CLHS 25.1.3, 25.1.1, 22.1, 3.2,
# 3.1.2.1.2.4)
# ---------------------------------------------------------------------------

def _interned(name):
    """The interned COMMON-LISP symbol `name`.

    Expansions that *reference a variable* must use the interned symbol:
    a global variable lives in the value cell of the symbol the bootstrap
    interned, and lookup reads that cell -- a bare `LispSymbol(name)`
    would be a different object with an empty cell.
    """
    return lisptype.COMMON_LISP_PACKAGE.intern_symbol(name)


@_standard_macro('TIME')
def _time_expander(form, env):
    """(time form) -> time `form`, report to *TRACE-OUTPUT*, return all of
    `form`'s values.

    The report is a FORMAT to *trace-output* of elapsed internal-time
    units, both clocks read as ordinary functions -- the same *stream* the
    pre-conversion branch wrote through. UNWIND-PROTECT with a done flag
    keeps the report running when `form` exits non-locally, and
    MULTIPLE-VALUE-PROG1 keeps every value, so `(time (values 1 2 3))`
    still answers all three. The reported numbers are timing measurements;
    only the report's *existence* is specified (environment/time.lsp
    asserts the captured text is non-empty)."""
    args = _form_args(form)
    if len(args) != 1:
        raise lisptype.LispProgramError("TIME requires exactly one form")
    start_real = _gensym()
    start_cpu = _gensym()
    done = _gensym()

    def report():
        return _list(
            _sym('FORMAT'), _interned('*TRACE-OUTPUT*'),
            lisptype.LispString(
                "Evaluation took:~%  ~A seconds of real time~%"
                "  ~A seconds of total run time~%"),
            _list(_sym('/'),
                  _list(_sym('-'), _list(_sym('GET-INTERNAL-REAL-TIME')),
                        start_real),
                  _interned('INTERNAL-TIME-UNITS-PER-SECOND')),
            _list(_sym('/'),
                  _list(_sym('-'), _list(_sym('GET-INTERNAL-RUN-TIME')),
                        start_cpu),
                  _interned('INTERNAL-TIME-UNITS-PER-SECOND')))

    bindings = _cons_from([
        _list(start_real, _list(_sym('GET-INTERNAL-REAL-TIME'))),
        _list(start_cpu, _list(_sym('GET-INTERNAL-RUN-TIME'))),
        _list(done, lisptype.NIL),
    ])
    return _cons_from(
        [_sym('LET'), bindings,
         _cons_from([_sym('UNWIND-PROTECT'),
                     _cons_from([_sym('MULTIPLE-VALUE-PROG1'),
                                 _cons_from([_sym('PROGN')] + args),
                                 _list(_sym('SETQ'), done, lisptype.T),
                                 report()]),
                     _cons_from([_sym('UNLESS'), done, report()])])])


@_standard_macro('STEP')
def _step_expander(form, env):
    """(step form+) -> (progn form+).

    CLHS allows STEP to be exactly this where the implementation offers no
    debugger stepping: the forms are evaluated in order and the last one's
    values are the result."""
    args = _form_args(form)
    if not args:
        raise lisptype.LispProgramError("STEP requires at least one form")
    return _cons_from([_sym('PROGN')] + args)


@_standard_macro('IGNORE-ERRORS')
def _ignore_errors_expander(form, env):
    """(ignore-errors form*) -> (handler-case (progn form*)
                                 (error (c) (values nil c))).

    This *is* CLHS's own definition of the macro, so the expansion is the
    definition rather than a reimplementation: the body's values come
    through untouched, and an error yields the two values NIL and the
    condition object -- while control transfers (a RETURN-FROM through the
    body) still pass through, because HANDLER-CASE catches conditions, not
    exits."""
    args = _form_args(form)
    return _cons_from(
        [_sym('HANDLER-CASE'),
         _cons_from([_sym('PROGN')] + args),
         _cons_from([_sym('ERROR'), _cons_from([_sym('C')]),
                     _list(_sym('VALUES'), lisptype.NIL, _sym('C'))])])


@_standard_macro('DECLAIM')
def _declaim_expander(form, env):
    """(declaim spec*) -> (progn (proclaim 'spec) ...).

    DECLAIM's specs are syntax; quoting each one hands it to PROCLAIM,
    which is the one interpreter of declarations (the same store the
    pre-conversion branch wrote directly). The value is NIL either way."""
    args = _form_args(form)
    return _cons_from(
        [_sym('PROGN')]
        + [_list(_sym('PROCLAIM'), _quoted(spec)) for spec in args])


@_standard_macro('LAMBDA')
def _lambda_expander(form, env):
    """(lambda lambda-list body...) -> (function (lambda lambda-list
    body...)) -- CLHS 3.1.2.1.2.4's own macro definition. The FUNCTION
    special form builds the function (via `make_ordinary_function`, the
    one ordinary-lambda-list constructor), and LAMBDA in the operator
    position evaluates through this same expansion."""
    return _cons_from([_sym('FUNCTION'), form])


@_standard_macro('FORMATTER')
def _formatter_expander(form, env):
    """(formatter control-string) -> (%formatter 'control-string) --
    CLHS 22.3.1: FORMATTER is a macro, and control-string is the literal
    object appearing in the form (never evaluated); quoting it here is
    what keeps it that way across the macroexpansion boundary, matching
    LAMBDA/IN-PACKAGE above."""
    args = _form_args(form)
    if len(args) != 1:
        raise lisptype.LispProgramError(
            "FORMATTER requires exactly one argument")
    return _list(_sym('%FORMATTER'), _quoted(args[0]))


@_standard_macro('IN-PACKAGE')
def _in_package_expander(form, env):
    """(in-package name) -> (%in-package 'name) -- CLHS 11.2: IN-PACKAGE
    is a macro, and its name argument is a package designator that is
    never evaluated (so `(in-package cl-test)` names the package
    CL-TEST, not whatever CL-TEST is bound to). `%IN-PACKAGE` in
    `utilities_symbols.py` is the runtime primitive that actually
    switches `*PACKAGE*`; quoting the argument here is what keeps it
    unevaluated across the macroexpansion boundary."""
    args = _form_args(form)
    if len(args) != 1:
        raise lisptype.LispProgramError(
            "IN-PACKAGE requires exactly one argument")
    return _list(_sym('%IN-PACKAGE'), _quoted(args[0]))


def _build_prog_expansion(form, let_symbol_name):
    """(prog[*] (var*) declare* body...) -> (block nil (let[*] (var*)
    declare* (tagbody . body))) -- CLHS 5.3's own macro definition. Leading
    `(DECLARE ...)` forms belong to the LET/LET*'s own bindings (e.g. to
    mark a variable SPECIAL), not to the TAGBODY, so they are hoisted
    ahead of it rather than left in the tagbody body, where DECLARE has no
    meaning."""
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispProgramError(
            f"{let_symbol_name} requires a variable list")
    varlist = car(args)
    body = cdr(args)

    declare_forms = []
    while _consp_internal(body):
        candidate = car(body)
        if (_consp_internal(candidate)
                and isinstance(car(candidate), lisptype.LispSymbol)
                and car(candidate).name == 'DECLARE'):
            declare_forms.append(candidate)
            body = cdr(body)
        else:
            break

    tagbody_form = cons(_sym('TAGBODY'), body)
    let_body = cons(tagbody_form, lisptype.NIL)
    for declare_form in reversed(declare_forms):
        let_body = cons(declare_form, let_body)
    let_form = cons(_sym(let_symbol_name), cons(varlist, let_body))
    return cons(_sym('BLOCK'), cons(lisptype.NIL, cons(let_form, lisptype.NIL)))


@_standard_macro('PROG')
def _prog_expander(form, env):
    """(prog (var*) body...) -> (block nil (let (var*) (tagbody . body)))."""
    return _build_prog_expansion(form, 'LET')


@_standard_macro('PROG*')
def _prog_star_expander(form, env):
    """(prog* (var*) body...) -> (block nil (let* (var*) (tagbody . body)))."""
    return _build_prog_expansion(form, 'LET*')


def _build_package_iteration_expansion(form, kind_name):
    """(do-symbols/do-external-symbols (var [package [result]]) declare*
    . body) -> (dolist (var (%package-symbol-list package 'kind) result)
    declare* . body). DOLIST already has the BindingFrame/implicit-NIL-
    block iteration machinery this needs; reusing it here replaces two of
    the three near-identical hand-rolled loops in
    `evaluation_loops_conditionals.py` with the same one DOLIST already
    exercises everywhere else. `package` stays an unevaluated form spliced
    into the DOLIST list-form, so it is still evaluated exactly once, in
    the outer (non-loop) environment, matching CLHS 6.1.2.1.7 -- the same
    place DOLIST's own list-form is evaluated."""
    args = cdr(form)
    if not _consp_internal(args):
        return lisptype.NIL
    var_clause = car(args)
    body = cdr(args)
    if not _consp_internal(var_clause):
        raise lisptype.LispNotImplementedError(
            f"DO-{kind_name} requires a (var [package]) clause")
    var = car(var_clause)
    rest = cdr(var_clause)
    package_form = car(rest) if _consp_internal(rest) else lisptype.NIL
    result_form = (car(cdr(rest))
                   if _consp_internal(rest) and _consp_internal(cdr(rest))
                   else lisptype.NIL)
    list_form = _list(_sym('%PACKAGE-SYMBOL-LIST'), package_form,
                       _quoted(_sym(kind_name)))
    dolist_clause = _list(var, list_form, result_form)
    return cons(_sym('DOLIST'), cons(dolist_clause, body))


@_standard_macro('DO-SYMBOLS')
def _do_symbols_expander(form, env):
    """(do-symbols (var [package [result]]) declare* . body) -- CLHS
    6.1.2.1.7: iterates every symbol *accessible* in package (its own,
    plus the externals of every package it uses)."""
    return _build_package_iteration_expansion(form, 'SYMBOLS')


@_standard_macro('DO-EXTERNAL-SYMBOLS')
def _do_external_symbols_expander(form, env):
    """(do-external-symbols (var [package [result]]) declare* . body) --
    CLHS 6.1.2.1.7: iterates only the symbols package *exports*."""
    return _build_package_iteration_expansion(form, 'EXTERNAL-SYMBOLS')


@_registry.cl_function('%PACKAGE-SYMBOL-LIST')
def _package_symbol_list_primitive(pkg_designator, kind):
    """Runtime primitive behind DO-SYMBOLS/DO-EXTERNAL-SYMBOLS: the
    package's symbol set named by `kind` ('SYMBOLS' or
    'EXTERNAL-SYMBOLS'), as a proper Lisp list for DOLIST to walk. Reuses
    `misc_packages.package_symbols`, the one enumerator LOOP's
    `for x being the symbols of p` also goes through."""
    from .misc_packages import package_symbols
    kind_str = kind.name.lower() if isinstance(kind, lisptype.LispSymbol) else str(kind).lower()
    result = lisptype.NIL
    for sym in reversed(list(package_symbols(pkg_designator, kind_str))):
        result = lisptype.lispCons(sym, result)
    return result


@_standard_macro('DO-ALL-SYMBOLS')
def _do_all_symbols_expander(form, env):
    """(do-all-symbols (var [result]) declare* . body) -> (dolist (var
    (%all-symbols-list) result) declare* . body) -- CLHS 6.1.2.1.7:
    iterates every symbol in every registered package (no package
    argument, unlike its two siblings above)."""
    args = cdr(form)
    if not _consp_internal(args):
        return lisptype.NIL
    var_clause = car(args)
    body = cdr(args)
    if not _consp_internal(var_clause):
        raise lisptype.LispNotImplementedError(
            "DO-ALL-SYMBOLS requires a (var) clause")
    var = car(var_clause)
    result_form = (car(cdr(var_clause))
                   if _consp_internal(cdr(var_clause)) else lisptype.NIL)
    list_form = _list(_sym('%ALL-SYMBOLS-LIST'))
    dolist_clause = _list(var, list_form, result_form)
    return cons(_sym('DOLIST'), cons(dolist_clause, body))


@_registry.cl_function('%ALL-SYMBOLS-LIST')
def _all_symbols_list_primitive():
    """Runtime primitive behind DO-ALL-SYMBOLS: every symbol in every
    uniquely-registered package (`state.packages` holds the same package
    under more than one name), as a proper Lisp list."""
    unique_packages = {id(p): p for p in state.packages.values()}
    all_syms = []
    for pkg in unique_packages.values():
        for sym in pkg.symbols.values():
            all_syms.append(sym)
    result = lisptype.NIL
    for sym in reversed(all_syms):
        result = lisptype.lispCons(sym, result)
    return result


@_standard_macro('DEFINE-MODIFY-MACRO')
def _define_modify_macro_expander(form, env):
    """(define-modify-macro name lambda-list function [doc]) -- CLHS
    5.1.3: defines `name` as a macro such that `(name place arg*)` reads
    place's old value, applies `function`, and stores the result back.

    `eval_define_modify_macro` (evaluation_special_forms.py) already
    builds and installs the resulting macro closure -- a Python closure
    over the *lexical* environment DEFINE-MODIFY-MACRO was itself
    evaluated in, for the new macro's own `&optional` default-value
    forms -- which cannot be represented as a printable Lisp expansion
    form the way WHEN/DECLAIM/... above are. Reusing that installer
    directly here (rather than duplicating its ~90 lines to build an
    equivalent expansion) is what replaced the old `operator.name ==
    'DEFINE-MODIFY-MACRO'` ladder branch; only the trigger moved, from
    the ladder to the macro path, so `(macro-function
    'define-modify-macro)` is no longer NIL. The nominal expansion is
    just `name` quoted, matching what DEFINE-MODIFY-MACRO CLHS-specifies
    as its value."""
    from .evaluation_special_forms import eval_define_modify_macro
    name = eval_define_modify_macro(form, env)
    return _quoted(name)


def _reuse_definer(worker_name, module_name='evaluation_special_forms'):
    """A macro expander that runs an existing `eval_defxxx(form, env)`
    definer immediately (installing whatever it installs, in the exact
    same `env` the evaluator would have handed the old ladder branch --
    no fidelity is lost, since nothing here defers the work into a form
    evaluated later) and quotes its CLHS-specified return value as the
    nominal expansion. See `_define_modify_macro_expander` above for why
    this is the right shape for a *definer*: its return value is always
    a name (or similar small, side-effect-free datum), not the arbitrary,
    possibly-effectful result of running a body -- the closure/lexical-
    environment problem an expansion-form-building macro would otherwise
    hit is exactly what installing immediately, rather than building a
    form for later, sidesteps."""
    def expander(form, env):
        import importlib
        worker_module = importlib.import_module(f'.{module_name}', package='fclpy.lispfunc')
        worker = getattr(worker_module, worker_name)
        return _quoted(worker(form, env))
    expander.__name__ = worker_name
    expander.__doc__ = f"Definer macro reusing `{worker_name}` -- see _reuse_definer."
    # This family's "expansion" *is* evaluation: the worker runs the form and
    # the result is quoted. Anything that macroexpands only to inspect a
    # form's shape must therefore stop here rather than expand -- see
    # `misc_packages.macro_expansion_evaluates`, and the RESTART-CASE
    # protected-form inspector that reads it.
    expander.__runs_body__ = True
    return expander


_standard_macro('DEFVAR')(_reuse_definer('eval_defvar'))
_standard_macro('DEFPARAMETER')(_reuse_definer('eval_defparameter'))
_standard_macro('DEFCONSTANT')(_reuse_definer('eval_defconstant'))
_standard_macro('DEFUN')(_reuse_definer('eval_defun'))
_standard_macro('DEFMACRO')(_reuse_definer('eval_defmacro'))
# DEFCLASS/DEFGENERIC/DEFMETHOD return the class/generic-function/method
# *object* (CLHS 7.7/7.7/7.6.2), not a name -- `_quoted` holds any Python
# object as a literal just as well as a symbol, and the COMPILE-FILE
# externalizer's `_walk_quoted` already has a dedicated branch for a
# quoted `LispClass` payload (turns it into `(FIND-CLASS 'name)`), so this
# is not a new shape for that path to handle.
_standard_macro('DEFCLASS')(_reuse_definer('eval_defclass'))
_standard_macro('DEFGENERIC')(_reuse_definer('eval_defgeneric'))
_standard_macro('DEFMETHOD')(_reuse_definer('eval_defmethod'))
_standard_macro('DEFINE-METHOD-COMBINATION')(_reuse_definer('eval_define_method_combination'))
# eval_define_condition lives in evaluation_conditions.py, not
# evaluation_special_forms.py -- the only one of this family that does.
_standard_macro('DEFINE-CONDITION')(
    _reuse_definer('eval_define_condition', module_name='evaluation_conditions'))
_standard_macro('DEFTYPE')(_reuse_definer('eval_deftype'))
_standard_macro('DEFSETF')(_reuse_definer('eval_defsetf'))
_standard_macro('DEFINE-SETF-EXPANDER')(_reuse_definer('eval_define_setf_expander'))
# DESTRUCTURING-BIND is the one _reuse_definer user whose "return value" is
# not a name/object but whatever its *body* computes -- an ordinary value,
# not a form, so quoting it is still correct once evaluated. The purity
# question this raises for the other members of this family (would a bare
# `(macroexpand-1 '(destructuring-bind ...))`, never itself evaluated,
# wrongly run the body as a side effect of "just expanding"?) has no test
# anywhere in ansi-test for this macro specifically (checked: no
# `macroexpand`+`destructuring-bind` co-occurrence in the suite, and
# destructuring-bind.error.7/.8/.9 only funcall the macro-function at the
# wrong arity, which `_standard_macro`'s wrapper already turns into
# PROGRAM-ERROR before `eval_destructuring_bind` ever runs). Every real
# call site immediately evaluates the expansion anyway (this interpreter
# has no separate compile-then-later-run phase for ordinary EVAL/LOAD), so
# there is no *observable* behavior change from the special-form version.
_standard_macro('DESTRUCTURING-BIND')(_reuse_definer('eval_destructuring_bind'))
# DO/DO*/DOLIST/DOTIMES: same reasoning as DESTRUCTURING-BIND above --
# their return value is whatever the loop's result-form(s) compute (or
# NIL), an ordinary value rather than a name. `eval_do`/`eval_do_star`/
# `eval_dolist`/`eval_dotimes` (evaluation_loops_conditionals.py) already
# implement CLHS's binding/stepping/implicit-NIL-block semantics via the
# one shared `BindingFrame` mechanism; reusing them keeps that mechanism
# exactly as-is (no rewrite into a BLOCK/LET/TAGBODY/PSETQ expansion,
# which would replace working, previously-hard-won iteration code for no
# behavioral gain), and only moves the trigger from the evaluator's
# hardcoded ladder to the macro path.
_standard_macro('DO')(
    _reuse_definer('eval_do', module_name='evaluation_loops_conditionals'))
_standard_macro('DO*')(
    _reuse_definer('eval_do_star', module_name='evaluation_loops_conditionals'))
_standard_macro('DOLIST')(
    _reuse_definer('eval_dolist', module_name='evaluation_loops_conditionals'))
_standard_macro('DOTIMES')(
    _reuse_definer('eval_dotimes', module_name='evaluation_loops_conditionals'))
# HANDLER-BIND/HANDLER-CASE/RESTART-BIND/RESTART-CASE/WITH-CONDITION-
# RESTARTS: same reuse-and-quote pattern again, and deliberately NOT a
# rewrite into HANDLER-BIND/BLOCK/LET (CLHS's own reference expansion for
# HANDLER-CASE, mirroring IGNORE-ERRORS's expansion above) -- these five
# have hard-won edge-case handling (a :no-error clause, an in-transit
# RestartCaseTransfer, an in-transit THROW with a live catch further out,
# a ConditionException backstop for a condition raised without being
# signaled) that a from-scratch CLHS expansion would have to reproduce
# exactly to avoid a silent regression in the condition system. Reusing
# the existing `eval_xxx` unchanged carries none of that risk: only the
# trigger moves, from the ladder to the macro path.
_standard_macro('RESTART-CASE')(
    _reuse_definer('eval_restart_case', module_name='evaluation_conditions'))
_standard_macro('RESTART-BIND')(
    _reuse_definer('eval_restart_bind', module_name='evaluation_conditions'))
_standard_macro('WITH-CONDITION-RESTARTS')(
    _reuse_definer('eval_with_condition_restarts', module_name='evaluation_conditions'))
# HANDLER-BIND/HANDLER-CASE: converting these first exposed a real
# mechanism bug rather than a test-vs-code mismatch -- discovered live
# (2026-08-30). RT's own `auxiliary/ansi-aux-macros.lsp` DEFMACROs a
# *shadowed* `handler-bind`/`handler-case` in CL-TEST
# (`(shadow '(handler-case handler-bind ...))`), expanding to `(let ()
# (cl:handler-bind ,handlers (normally (progn ,@body))))`.
# `Environment.find_func` used to resolve by symbol *name string* only,
# not package-qualified identity, so once HANDLER-BIND was reachable
# through the macro path, the `cl:handler-bind` call *inside that
# macro's own expansion* resolved back to the same CL-TEST macro instead
# of the real one -- infinite self-expansion, one extra `(normally
# (progn ...))` wrapper per pass, a RecursionError before `init.lsp`
# finished loading. The special-form ladder never hit this because
# `operator.name == 'HANDLER-BIND'` also matched by name only, but
# ladder dispatch ran *before* any `find_func` lookup, so CL-TEST's
# DEFMACRO was simply never reached -- harmless dead code, not a working
# shadow. The real fix is in `lisptype_extended.py`'s `Environment`: an
# identity-keyed overlay (`_function_map_by_symbol`) that `find_func`
# checks before the name-only cache, so a genuinely distinct (e.g.
# shadowed-in-another-package) symbol object gets its *own* binding
# instead of colliding with same-named ones -- while a fresh, uninterned
# symbol built just to *name* an operator in generated code (this file's
# own `_sym(...)` calls) still falls through to the name-only cache
# exactly as before. That is what makes HANDLER-BIND/HANDLER-CASE safe
# to convert below.
_standard_macro('HANDLER-BIND')(
    _reuse_definer('eval_handler_bind', module_name='evaluation_conditions'))
_standard_macro('HANDLER-CASE')(
    _reuse_definer('eval_handler_case', module_name='evaluation_conditions'))
# DEFSTRUCT / LOOP / PPRINT-LOGICAL-BLOCK: the last three operators whose
# ladder branch was already a one-line delegation to an `eval_xxx(form,
# env)` worker, so converting them is purely a change of *trigger* -- the
# same function runs, with the same `env`, on the same unevaluated form.
# Each of the three is also a case where rebuilding a CLHS expansion from
# scratch would be actively wrong to attempt here: DEFSTRUCT's is not
# specified as a macro expansion at all (CLHS 3.4.6 leaves the generated
# constructor/accessor/copier/predicate set implementation-defined, and
# this one's BOA-lambda-list handling was hard-won -- see plan.md's
# history); LOOP has the single iteration engine CLAUDE.md's architecture
# map names as having exactly one home, which a second, expansion-shaped
# parse would immediately duplicate; and PPRINT-LOGICAL-BLOCK drives the
# pretty-printer's own engine, whose dynamic per-block state is
# established by that engine rather than by a surrounding form.
_standard_macro('DEFSTRUCT')(_reuse_definer('eval_defstruct'))
_standard_macro('LOOP')(
    _reuse_definer('eval_loop', module_name='evaluation_loops_conditionals'))
_standard_macro('PPRINT-LOGICAL-BLOCK')(
    _reuse_definer('eval_pprint_logical_block'))
# SETF / PSETF / DEFPACKAGE: the last three, and the only ones whose ladder
# branch was a large *inline* block rather than a delegation, so converting
# them needed the block extracted first. That extraction is deliberately a
# pure move -- the workers live in `evaluation_core` itself
# (`eval_setf`/`eval_psetf`/`eval_defpackage`), not in a "better" module, so
# every free name in those several hundred lines resolves to exactly what it
# resolved to inline and no place-resolution behaviour can shift underneath
# the conversion.
#
# SETF in particular is *not* rebuilt here into its CLHS 5.1.2 expansion
# (`(let* ((tmp sub)...) (let ((store val)) store-form))` via
# GET-SETF-EXPANSION). That rewrite is real work with real risk -- the
# ladder's place handling covers VALUES/THE/APPLY places, symbol-macro
# places, the LDB/MASK-FIELD/SUBSEQ/GETF place-in-place cases and their
# subform-evaluation *order* (which `setf-getf.order.*`,
# `ldb.place.order.1` and `mask-field.place.order.1` each count directly),
# plus the known left-to-right gap in the legacy branches that plan.md
# already tracks. Doing it properly is the M5 place-protocol milestone,
# whose whole point is to make `_place_accessor`/`get_setf_expansion` the
# single mechanism the ladder's remaining branches currently bypass. M4's
# question is a narrower one -- is `(macro-function 'setf)` non-NIL, does
# the operator dispatch through the macro path -- and that is what this
# answers, without spending M5's risk budget early.
_standard_macro('SETF')(
    _reuse_definer('eval_setf', module_name='evaluation_core'))
_standard_macro('PSETF')(
    _reuse_definer('eval_psetf', module_name='evaluation_core'))
_standard_macro('DEFPACKAGE')(
    _reuse_definer('eval_defpackage', module_name='evaluation_core'))


# ---------------------------------------------------------------------------
# PPRINT-POP / PPRINT-EXIT-IF-LIST-EXHAUSTED (CLHS 22.2.2)
# ---------------------------------------------------------------------------
#
# Both are specified as *macros* taking no arguments, and were registered
# here as ordinary zero-argument functions. At zero arity the difference is
# invisible to every test ansi-test actually runs -- which is exactly why
# this is worth fixing rather than leaving: CLAUDE.md's standard is that a
# defect the suite happens to miss is still a defect. What a caller can
# observe is `(macro-function 'pprint-pop)` (must be non-NIL) and
# `#'pprint-pop` (must *not* name a function), neither of which the
# function registration can satisfy.
#
# The expansion is a call to the `%`-prefixed runtime in `io_write.py`,
# which keeps the frame-stack mechanism (`_current_pprint_frame`, the
# `*PRINT-LENGTH*` ordering that `pprint-pop.5`/`.6` and
# `pprint-exit-if-list-exhausted.1`/`.3` pin down between them) entirely
# untouched -- only the operator's *kind* changes.

def _nullary_runtime_macro(runtime_name):
    """A zero-argument macro expanding to `(runtime_name)`.

    Shared by the three CLHS operators that are specified as macros taking
    no arguments but were registered here as a function or a special
    operator: PPRINT-POP, PPRINT-EXIT-IF-LIST-EXHAUSTED and LOOP-FINISH.

    Arity is checked at expansion time rather than left to the runtime's
    Python signature: CLHS makes a wrong-argument-count macro call a
    PROGRAM-ERROR, and a Python TypeError from the underlying function is
    not a condition (CLAUDE.md's `signal_file_error` note makes the same
    point for FILE-ERROR).
    """
    def expander(form, env):
        args = _form_args(form)
        if args:
            raise lisptype.LispProgramError(
                f"{runtime_name.lstrip('%')} takes no arguments")
        return _list(_sym(runtime_name))
    # A distinct `__name__` per operator, not cosmetic: `_standard_macro`
    # copies it onto the registered wrapper and the registry stores it as
    # `py_name`, which `test_no_duplicate_python_bindings` uses to detect
    # one Python callable serving several Lisp operators -- exactly the
    # standing-rule-3 defect class. Three closures all named `expander`
    # are indistinguishable from that, and would mask a genuine one.
    expander.__name__ = '_%s_expander' % runtime_name.lstrip('%').lower().replace('-', '_')
    return expander


_standard_macro('PPRINT-POP')(_nullary_runtime_macro('%PPRINT-POP'))
_standard_macro('PPRINT-EXIT-IF-LIST-EXHAUSTED')(
    _nullary_runtime_macro('%PPRINT-EXIT-IF-LIST-EXHAUSTED'))
# LOOP-FINISH (CLHS 6.2) is the same shape -- a zero-argument macro that
# was registered as a special operator. Unlike the two above, ansi-test
# does test this one directly: `loop-finish.error.1` reaches for
# `(macro-function 'loop-finish env)` from inside a MACROLET and FUNCALLs
# it at three wrong arities, each of which must signal PROGRAM-ERROR.
_standard_macro('LOOP-FINISH')(_nullary_runtime_macro('%LOOP-FINISH'))


@_standard_macro('DEFINE-SYMBOL-MACRO')
def _define_symbol_macro_expander(form, env):
    """(define-symbol-macro symbol expansion) ->
    (%define-symbol-macro 'symbol 'expansion) -- CLHS 24.2.2: establishes
    a symbol macro with *global* scope (unlike SYMBOL-MACROLET's lexical
    one), and `expansion` is never evaluated -- it is substituted
    verbatim wherever `symbol` is referenced afterward, so it is quoted
    here rather than passed through as code. There was no implementation
    of this at all before (not a special form, not a macro, not even a
    stub) -- `(define-symbol-macro x 1)` signalled UNDEFINED-FUNCTION."""
    args = _form_args(form)
    if len(args) != 2:
        raise lisptype.LispProgramError(
            "DEFINE-SYMBOL-MACRO requires exactly two arguments")
    name, expansion = args
    if not isinstance(name, lisptype.LispSymbol):
        raise lisptype.LispProgramError(
            "DEFINE-SYMBOL-MACRO: name must be a symbol")
    return _list(_sym('%DEFINE-SYMBOL-MACRO'), _quoted(name), _quoted(expansion))


@_registry.cl_function('%DEFINE-SYMBOL-MACRO')
def _define_symbol_macro_primitive(name, expansion):
    """Runtime primitive behind the `DEFINE-SYMBOL-MACRO` macro: installs
    the symbol-macro in the *global* environment (CLHS 24.2.2 gives it
    global scope, unlike SYMBOL-MACROLET), found by walking up from the
    current environment the same way DEFVAR/DEFUN reach the global scope
    to install a binding that must outlive the form that created it."""
    env = state.current_environment
    if env is None:
        raise lisptype.LispError(
            "DEFINE-SYMBOL-MACRO: no environment available")
    global_env = env
    while global_env.parent is not None:
        global_env = global_env.parent
    global_env.add_symbol_macro(name, expansion)
    return name

