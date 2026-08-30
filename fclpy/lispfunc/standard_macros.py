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
    return _list(_sym('ERROR'), _sym('TYPE-ERROR'),
                 _sym(':DATUM'), tmp,
                 _sym(':EXPECTED-TYPE'), expected_type)


def _keyform_form(key_form, tmp, cond_clauses):
    return _cons_from(
        [_sym('LET'), _cons_from([_list(tmp, key_form)]),
         _cons_from([_sym('COND')] + cond_clauses)
         if cond_clauses else lisptype.NIL])


def _keyplace_form(operator, place, cond_clauses, expected_type, env):
    """CCASE/CTYPECASE: match, and on no match signal a *correctable*
    TYPE-ERROR whose STORE-VALUE restart stores back into the place and
    retries -- the protocol ccase.31/ctypecase.12 exercise through
    `(store-value new c)` in a HANDLER-BIND. The place's subforms run once
    (ccase.25), the place is *re-read* on every retry, and the store
    variables are the ones GET-SETF-EXPANSION's own store form names, so
    arbitrary places work unchanged."""
    temps, vals, stores, store_form, access = _place_full(place, env)
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
    return _cons_from([_sym('LET*'), bindings,
                       _cons_from([_sym('TAGBODY'),
                                   retry_tag,
                                   inner])])


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
                          _member_type_form(all_keys), env)


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
                          _or_type_form(all_types), env)


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

