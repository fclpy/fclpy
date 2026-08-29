"""Error handling, conditions, warnings, and restart operations."""

import fclpy.lisptype as lisptype
from fclpy.lispfunc import registry as _registry


# --- Condition definition and handling ---
## `DEFINE-CONDITION` is a special form handled by the evaluator;
## do not register it as a regular function here.
def define_condition(name, parent_types, slot_specs, *options):
    """Define condition type (stub kept for reference)."""
    return name


# Re-export PACKAGE-ERROR-PACKAGE: the function lives in
# `misc_packages` (its native module per CLHS 11.2's section on the
# PACKAGE-ERROR class and its accessors), but the re-export machinery
# in `lispfunc.utilities` and `lispfunc.utilities_misc` does
# `from .utilities_errors import *` and expects the symbol to be a
# member of this module's namespace. Without the re-export every
# import of fclpy fails with
# `AttributeError: module 'fclpy.lispfunc.utilities_errors' has no
# attribute 'package_error_package'`, taking the whole interpreter
# down before the first test can run.
from .misc_packages import package_error_package  # noqa: F401, E402


@_registry.cl_function('MAKE-CONDITION')
def make_condition(type_designator, *args):
    """Create and return a condition object (CLHS 9.2 MAKE-CONDITION).

    type_designator names a condition type; the remaining args are
    alternating initarg keyword/value pairs, exactly like MAKE-INSTANCE.
    Previously a stub that returned type_designator itself -- the bare type
    symbol, not a condition -- so (error (make-condition 'simple-error ...))
    signaled an unmatchable non-condition object (plan.md Finding E) instead
    of a real one.
    """
    from fclpy.lispfunc.evaluation_conditions import make_condition_of_type

    built = make_condition_of_type(type_designator, list(args))
    if built is not None:
        return built

    raise lisptype.LispTypeError(
        f"MAKE-CONDITION: {type_designator!r} does not designate a known condition type",
        expected_type='condition-type-designator', actual_value=type_designator)


@_registry.cl_function('SIGNAL')
def signal_fn(datum, *arguments):
    """SIGNAL as a function designator (used by FUNCALL/APPLY/#'SIGNAL).

    Delegates to the same signaling core as the SIGNAL special form, so both
    build the condition the same way and walk the same handler stack. This used
    to be `return None` -- it notified nothing and signaled nothing, so
    (funcall #'signal ...) silently did nothing at all.
    """
    from fclpy.lispfunc.evaluation_conditions import build_condition, signal_condition_object

    condition = build_condition(datum, list(arguments), lisptype.SimpleCondition)
    return signal_condition_object(condition)


@_registry.cl_function('ERROR')
def error_fn(datum, *arguments):
    """ERROR as a function designator (used by FUNCALL/APPLY/#'ERROR).

    Delegates to the same signaling core as the ERROR special form. This used
    to raise a bare Python `Exception` carrying a formatted message: no
    condition object existed, so no HANDLER-BIND/HANDLER-CASE clause could
    match it -- not even (ERROR (C) ...) -- and it escaped every handler
    (plan.md Finding E). The ANSI suite reaches this path constantly, because
    RT's own `report-error` calls (apply #'error args).
    """
    from fclpy.lispfunc.evaluation_conditions import build_condition, signal_error_object

    condition = build_condition(datum, list(arguments), lisptype.SimpleError)
    return signal_error_object(condition)


@_registry.cl_function('WARN')
def warn_fn(datum, *arguments):
    """Warn about condition (function-designator entry point, used by
    FUNCALL/APPLY/#'WARN). Delegates to the same warning-signaling logic as
    the WARN special form (evaluation_conditions.eval_warn) so there is one
    implementation, not two that can silently drift apart.
    """
    from fclpy.lispfunc.evaluation_conditions import signal_warning
    return signal_warning(datum, list(arguments))


@_registry.cl_function('MUFFLE-WARNING')
def muffle_warning(condition=None):
    """MUFFLE-WARNING function designator (CLHS 9.1): finds and invokes the
    MUFFLE-WARNING restart applicable to `condition`, signalling
    CONTROL-ERROR if none is active. `_invoke_named_restart` is the one
    place this "find by name, invoke if found" shape is written, shared with
    ABORT/CONTINUE/USE-VALUE/STORE-VALUE below and with the WARN special
    form's own implicit restart (`evaluation_conditions._signal_warning_object`).
    """
    from fclpy.lispfunc.evaluation_conditions import _invoke_named_restart
    return _invoke_named_restart('MUFFLE-WARNING', condition, (), error_if_missing=True)


# --- Restart operations ---
@_registry.cl_function('COMPUTE-RESTARTS')
def compute_restarts(condition=None):
    """COMPUTE-RESTARTS (CLHS 9.1): every active restart applicable to
    `condition`, as a real Lisp list (plan.md Finding M -- a Python list
    here is a *vector*, and `(loop for r in (compute-restarts) ...)` needs a
    list to iterate)."""
    from fclpy.lispfunc.evaluation_conditions import compute_restarts_list
    from fclpy.lispfunc.sequence_protocol import make_lisp_list
    return make_lisp_list(compute_restarts_list(condition))


@_registry.cl_function('FIND-RESTART')
def find_restart(identifier, condition=None):
    """FIND-RESTART (CLHS 9.1)."""
    from fclpy.lispfunc.evaluation_conditions import find_restart_obj
    restart = find_restart_obj(identifier, condition)
    return restart if restart is not None else lisptype.NIL


@_registry.cl_function('INVOKE-RESTART')
def invoke_restart(restart, *arguments):
    """INVOKE-RESTART function designator (CLHS 9.1), used by FUNCALL/APPLY/
    #'INVOKE-RESTART. The direct-call syntax `(invoke-restart ...)` goes
    through the `eval_invoke_restart` special form instead (evaluation_core's
    dispatch reaches it first), but both resolve and invoke through the same
    `find_restart_obj`/`invoke_restart_obj`, since the registry had already
    evaluated `restart` and `arguments` here exactly as that special form
    does by hand.
    """
    from fclpy.lispfunc.evaluation_conditions import (
        find_restart_obj, invoke_restart_obj, signal_error_object)
    if isinstance(restart, lisptype.Restart):
        target = restart
    else:
        target = find_restart_obj(restart)
        if target is None:
            name = restart.name if isinstance(restart, lisptype.LispSymbol) else str(restart)
            return signal_error_object(lisptype.ControlError(
                message=f"No restart named {name} is currently active."))
    return invoke_restart_obj(target, list(arguments))


@_registry.cl_function('INVOKE-RESTART-INTERACTIVELY')
def invoke_restart_interactively(restart):
    """INVOKE-RESTART-INTERACTIVELY (CLHS 9.1): if `restart` has an
    :interactive-function, call it (with no arguments) to produce the
    argument list; otherwise invoke with no arguments."""
    from fclpy.lispfunc.evaluation_conditions import find_restart_obj, invoke_restart_obj
    from fclpy.lispfunc.evaluation_core import funcall
    target = restart if isinstance(restart, lisptype.Restart) else find_restart_obj(restart)
    if target is None:
        name = restart.name if isinstance(restart, lisptype.LispSymbol) else str(restart)
        from fclpy.lispfunc.evaluation_conditions import signal_error_object
        return signal_error_object(lisptype.ControlError(
            message=f"No restart named {name} is currently active."))
    args = []
    if target.interactive_function is not None:
        produced = funcall(target.interactive_function)
        cur = produced
        while isinstance(cur, lisptype.lispCons):
            args.append(cur.car)
            cur = cur.cdr
    return invoke_restart_obj(target, args)


@_registry.cl_function('RESTART-NAME')
def restart_name(restart):
    """RESTART-NAME (CLHS 9.1): the restart's name, or NIL if anonymous."""
    return restart.name


# --- Condition and restart utilities ---
@_registry.cl_function('ABORT')
def abort(condition=None):
    """ABORT function designator (CLHS 9.1)."""
    from fclpy.lispfunc.evaluation_conditions import _invoke_named_restart
    return _invoke_named_restart('ABORT', condition, (), error_if_missing=True)


@_registry.cl_function('CONTINUE')
def continue_fn(condition=None):
    """CONTINUE (CLHS 9.1): finds and invokes the CONTINUE restart applicable
    to `condition`, returning NIL if none is active (unlike ABORT/
    MUFFLE-WARNING, CONTINUE does not signal CONTROL-ERROR -- CLHS 9.1's
    description of CONTINUE, USE-VALUE and STORE-VALUE all end with "If it
    is not found, nil is returned").

    This replaces a stub in misc_macros.py that just returned NIL
    unconditionally -- it never looked at the restart stack at all, so
    `(cerror "" 'simple-error)` handled with `(continue c)` never actually
    resumed anything (plan.md's CERROR gap).
    """
    from fclpy.lispfunc.evaluation_conditions import _invoke_named_restart
    return _invoke_named_restart('CONTINUE', condition, (), error_if_missing=False)


@_registry.cl_function('STORE-VALUE')
def store_value(value, condition=None):
    """STORE-VALUE (CLHS 9.1): finds and invokes the STORE-VALUE restart
    applicable to `condition` with `value`, returning NIL if none is active."""
    from fclpy.lispfunc.evaluation_conditions import _invoke_named_restart
    return _invoke_named_restart('STORE-VALUE', condition, (value,), error_if_missing=False)


@_registry.cl_function('USE-VALUE')
def use_value(value, condition=None):
    """USE-VALUE (CLHS 9.1): finds and invokes the USE-VALUE restart
    applicable to `condition` with `value`, returning NIL if none is active."""
    from fclpy.lispfunc.evaluation_conditions import _invoke_named_restart
    return _invoke_named_restart('USE-VALUE', condition, (value,), error_if_missing=False)


@_registry.cl_macro('WITH-SIMPLE-RESTART')
def with_simple_restart_macro(binding, *body):
    """Macro expander for WITH-SIMPLE-RESTART (CLHS 9.1).

    (WITH-SIMPLE-RESTART (name format-control format-arg*) form*) expands
    into a RESTART-CASE with one clause of that name, whose body simply
    returns (VALUES NIL T) -- not a second restart-establishing mechanism,
    just RESTART-CASE used the way it already handles any clause that
    produces a value without re-entering the body. `name` may be NIL, for an
    anonymous restart (with-simple-restart.8).

    Was a plain `cl_function` that evaluated (and discarded) its body eagerly
    before establishing anything -- the same registry defect CLAUDE.md
    documents for WITH-STANDARD-IO-SYNTAX: a form with unevaluated syntax
    (the binding's `name`, and the body that must run *after* the restart is
    established) cannot be a `cl_function`.
    """
    name = binding.car if isinstance(binding, lisptype.lispCons) else lisptype.NIL
    rest = binding.cdr if isinstance(binding, lisptype.lispCons) else lisptype.NIL
    format_control = rest.car if isinstance(rest, lisptype.lispCons) else lisptype.LispString("")
    format_args = []
    cur = rest.cdr if isinstance(rest, lisptype.lispCons) else lisptype.NIL
    while isinstance(cur, lisptype.lispCons):
        format_args.append(cur.car)
        cur = cur.cdr

    def _list_from(seq):
        result = lisptype.NIL
        for element in reversed(list(seq)):
            result = lisptype.lispCons(element, result)
        return result

    stream_sym = lisptype.LispSymbol('STREAM')
    report_lambda = _list_from([
        lisptype.LispSymbol('LAMBDA'), _list_from([stream_sym]),
        _list_from([lisptype.LispSymbol('FORMAT'), stream_sym, format_control, *format_args]),
    ])
    values_form = _list_from([lisptype.LispSymbol('VALUES'), lisptype.NIL, lisptype.T])
    clause = _list_from([name, lisptype.NIL,
                          lisptype.intern_keyword('REPORT'), report_lambda,
                          values_form])
    protected = lisptype.lispCons(lisptype.LispSymbol('PROGN'), _list_from(body))
    return _list_from([lisptype.LispSymbol('RESTART-CASE'), protected, clause])


@_registry.cl_function('INVOKE-DEBUGGER')
def invoke_debugger(condition):
    """INVOKE-DEBUGGER (CLHS 9.1): calls `*DEBUGGER-HOOK*` with `(condition
    hook)`, with the hook itself rebound to NIL for the call (so a hook that
    invokes the debugger recursively sees no hook, CLHS 9.1); a hook that
    does not accept exactly two arguments signals PROGRAM-ERROR "prior to
    entry to the standard debugger" (invoke-debugger.error.3-5), which
    `evaluation_core.funcall`'s own arity-mismatch-to-PROGRAM-ERROR
    conversion already gives for free. With no hook bound, there is no
    interactive debugger to enter here, so the condition is signalled as an
    (unhandled) error instead of being silently dropped.
    """
    from fclpy.lispfunc.evaluation_core import funcall
    from fclpy.lispfunc.evaluation_conditions import signal_error_object
    from fclpy.lispfunc.binding import dynamic_value, set_dynamic_value

    hook_symbol = lisptype.COMMON_LISP_PACKAGE.intern_symbol('*DEBUGGER-HOOK*')
    hook = dynamic_value(hook_symbol, lisptype.NIL)
    if hook in (None, lisptype.NIL):
        if isinstance(condition, lisptype.Condition):
            return signal_error_object(condition)
        return signal_error_object(lisptype.Error(message=str(condition)))

    previous = hook
    set_dynamic_value(hook_symbol, lisptype.NIL)
    try:
        return funcall(hook, condition, previous)
    finally:
        set_dynamic_value(hook_symbol, previous)


# --- Method combination and condition errors ---
# DEFINE-METHOD-COMBINATION lives in `evaluation_special_forms.py` and is
# dispatched by the evaluator, because none of its subforms may be
# evaluated. The second implementation that used to sit here -- a
# `cl_function` building a private one-off MethodCombination class and
# binding it as a *variable* -- was reachable through `SYMBOL-FUNCTION` and
# `FUNCALL`, defined nothing, and disagreed with the special form about
# what the operator even produces (standing rule 3).


@_registry.cl_function('METHOD-COMBINATION-ERROR')
def method_combination_error(format_control, *format_arguments):
    """METHOD-COMBINATION-ERROR (CLHS 7.6.6): signal that the applicable
    methods cannot be combined. It *signals*; returning NIL (what this did)
    let a method combination whose own body detected an inconsistency carry
    on and produce an answer anyway."""
    from fclpy.lispfunc.io_write import format_fn
    import fclpy.classes as classes
    message = format_fn(lisptype.NIL, format_control, *format_arguments)
    raise classes.MethodCombinationError(str(message))


@_registry.cl_function('INVALID-METHOD-ERROR')
def invalid_method_error(method, format_control, *format_args):
    """Invalid method error."""
    raise lisptype.LispNotImplementedError('INVALID-METHOD-ERROR')


@_registry.cl_function('PACKAGE-ERROR-PACKAGE')
def package_error_package(condition):
    """Get package from package error condition."""
    return None


@_registry.cl_function('TYPE-ERROR-DATUM')
def type_error_datum(*args):
    """Get the datum (offending value) from a type-error condition."""
    if len(args) != 1:
        raise lisptype.LispProgramError(
            f"TYPE-ERROR-DATUM: wrong number of arguments (got {len(args)}, expected 1)"
        )
    condition = args[0]
    # If condition is a Python-level LispTypeError, get its actual_value attribute
    if isinstance(condition, lisptype.LispTypeError):
        return getattr(condition, 'actual_value', lisptype.NIL)
    # If condition is the Lisp Condition TypeError (from lisptype_extended),
    # retrieve the 'datum' slot
    try:
        if isinstance(condition, lisptype.TypeError):
            return condition._slots.get('datum', lisptype.NIL)
    except Exception:
        pass
    # If it's a string representation of an error, return NIL
    return lisptype.NIL


@_registry.cl_function('TYPE-ERROR-EXPECTED-TYPE')
def type_error_expected_type(*args):
    """Get the expected type from a type-error condition."""
    if len(args) != 1:
        raise lisptype.LispProgramError(
            f"TYPE-ERROR-EXPECTED-TYPE: wrong number of arguments (got {len(args)}, expected 1)"
        )
    condition = args[0]
    # If condition is a LispTypeError, get its expected_type attribute
    if isinstance(condition, lisptype.LispTypeError):
        return getattr(condition, 'expected_type', lisptype.NIL)
    # If condition is the Lisp Condition TypeError, retrieve the 'expected-type' slot
    try:
        if isinstance(condition, lisptype.TypeError):
            return condition._slots.get('expected-type', lisptype.NIL)
    except Exception:
        pass
    # If it's a string representation of an error, return NIL
    return lisptype.NIL


@_registry.cl_function('CELL-ERROR-NAME')
def cell_error_name(*args):
    """Return the name associated with a CELL-ERROR condition.

    Usage: (CELL-ERROR-NAME condition)
    Returns the value of the 'name' slot if present, otherwise NIL.
    """
    if len(args) != 1:
        raise lisptype.LispProgramError(
            f"CELL-ERROR-NAME: wrong number of arguments (got {len(args)}, expected 1)"
        )
    condition = args[0]
    # If it's a lisptype.Condition, use its get_slot method
    try:
        if isinstance(condition, lisptype.Condition):
            name = condition.get_slot('name')
            return name if name is not None else lisptype.NIL
    except Exception:
        pass
    # If it's a legacy LispError with attributes, try common keys
    try:
        if isinstance(condition, lisptype.LispError):
            return getattr(condition, 'name', lisptype.NIL)
    except Exception:
        pass
    return lisptype.NIL


@_registry.cl_function('UNBOUND-SLOT-INSTANCE')
def unbound_slot_instance(condition):
    """CLHS UNBOUND-SLOT-INSTANCE: the INSTANCE slot of an UNBOUND-SLOT condition.

    Returns the object whose slot was unbound, or NIL if not an UNBOUND-SLOT condition.
    """
    if not isinstance(condition, lisptype.UnboundSlot):
        return lisptype.NIL
    instance = condition.get_slot('instance')
    return instance if instance is not None else lisptype.NIL


@_registry.cl_function('STREAM-ERROR-STREAM')
def stream_error_stream(condition):
    """CLHS STREAM-ERROR-STREAM: the STREAM slot of a STREAM-ERROR (and its
    subtypes END-OF-FILE, READER-ERROR).

    Every reader entry point (READ, READ-FROM-STRING, LOAD's per-form loop)
    signals end-of-file as `lisptype.LispEndOfFileError`, a legacy Python
    exception with a `.stream` attribute set to the actual stream being read
    -- not the real `lisptype.Condition` class in lisptype_extended.py,
    which predates it. This reads whichever shape `condition` turns out to
    be, so ansi-test's own `signals-error` fixture -- which asserts
    `(streamp (stream-error-stream c))` for every END-OF-FILE/STREAM-ERROR/
    READER-ERROR test, not just the ones this session touched -- gets a
    real stream back either way.
    """
    stream = getattr(condition, 'stream', None)
    if stream is None and isinstance(condition, lisptype.Condition):
        stream = condition.get_slot('stream')
    return stream if stream is not None else lisptype.NIL


@_registry.cl_macro('CHECK-TYPE')
def check_type_macro(place, type_spec, *string):
    """Macro expander for CHECK-TYPE (CLHS 7.9).

    (CHECK-TYPE place type-spec [string]) expands into a loop that signals a
    TYPE-ERROR (via ERROR, so RESTART-CASE's auto-association -- CLHS 9.1 --
    associates the STORE-VALUE restart below with the very condition it
    signals) until `place` satisfies `type-spec`, SETFing `place` to
    whatever STORE-VALUE supplies. CLHS 9.1's own description of STORE-VALUE
    names CHECK-TYPE as the reason it exists, so once RESTART-CASE's
    auto-association was real this needed only the expansion, no new
    restart-mechanism code of its own.
    """
    def _l(seq):
        result = lisptype.NIL
        for element in reversed(list(seq)):
            result = lisptype.lispCons(element, result)
        return result

    report_text = str(string[0]) if string and isinstance(string[0], (str, lisptype.LispString)) \
        else "Supply a new value."
    quoted_type = _l([lisptype.LispSymbol('QUOTE'), type_spec])
    error_form = _l([lisptype.LispSymbol('ERROR'),
                     _l([lisptype.LispSymbol('QUOTE'), lisptype.LispSymbol('TYPE-ERROR')]),
                     lisptype.intern_keyword('DATUM'), place,
                     lisptype.intern_keyword('EXPECTED-TYPE'), quoted_type])
    store_clause = _l([
        lisptype.LispSymbol('STORE-VALUE'), _l([lisptype.LispSymbol('V')]),
        lisptype.intern_keyword('REPORT'), lisptype.LispString(report_text),
        lisptype.LispSymbol('V')])
    restart_case_form = _l([lisptype.LispSymbol('RESTART-CASE'), error_form, store_clause])
    setf_form = _l([lisptype.LispSymbol('SETF'), place, restart_case_form])
    typep_form = _l([lisptype.LispSymbol('TYPEP'), place, quoted_type])
    when_form = _l([lisptype.LispSymbol('WHEN'), typep_form,
                    _l([lisptype.LispSymbol('RETURN'), lisptype.NIL])])
    return _l([lisptype.LispSymbol('LOOP'), when_form, setf_form])


__all__ = [
    'define_condition',
    'make_condition',
    'signal_fn',
    'error_fn',
    'warn_fn',
    'muffle_warning',
    'compute_restarts',
    'find_restart',
    'invoke_restart',
    'invoke_restart_interactively',
    'restart_name',
    'abort',
    'continue_fn',
    'store_value',
    'use_value',
    'with_simple_restart_macro',
    'check_type_macro',
    'invoke_debugger',
    'method_combination_error',
    'invalid_method_error',
    'package_error_package',
    'type_error_datum',
    'type_error_expected_type',
]
