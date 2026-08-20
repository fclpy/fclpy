"""Condition handling: SIGNAL, ERROR, restarts, multiple values."""

import fclpy.state as state
import fclpy.lisptype as lisptype
from .core import car, cdr, cons, _consp_internal
from . import registry as _registry
from .evaluation_core import (
    ConditionException, ThrowException, ReturnFromException, GoException,
    HandlerCaseTag, HandlerCaseTransfer)
import fclpy.lispfunc as lispfunc


# User-defined condition types (CLHS DEFINE-CONDITION), keyed by CL type name
# (upper-case). Each value is a Python class dynamically created by
# `eval_define_condition`, subclassing `lisptype.Condition` (or whichever
# condition classes its :DEFINE-CONDITION parent list named) exactly like the
# built-in condition classes in lisptype_extended.py -- so TYPEP, SUBTYPEP and
# HANDLER-BIND/HANDLER-CASE dispatch, which all already work via `isinstance`
# against that hierarchy, need no separate case for a user-defined type.
_USER_CONDITION_CLASSES = {}

_MISSING = object()


def _condition_class_for_name(name):
    """Map a CL condition type name (e.g. "TYPE-ERROR") to its Python class
    in lisptype (e.g. lisptype.TypeError) or `_USER_CONDITION_CLASSES` (e.g. a
    DEFINE-CONDITION type), or None if that name does not designate a
    condition type.

    The camel-cased lookup is a naming convention over lisptype's namespace,
    which also contains plenty of non-condition classes (`Package`,
    `Environment`, ...). Requiring the result to be a `Condition` subclass
    keeps a type name like PACKAGE or ENVIRONMENT from resolving to an
    unrelated class that some future caller might then `isinstance` against
    or -- worse, for MAKE-CONDITION -- instantiate as though it were a
    condition type.
    """
    camel = ''.join(part.capitalize() for part in name.replace('_', '-').split('-') if part)
    candidate = getattr(lisptype, camel, None)
    if isinstance(candidate, type) and issubclass(candidate, lisptype.Condition):
        return candidate
    return _USER_CONDITION_CLASSES.get(name.upper())


def _normalize_initarg(symbol):
    """The keyword->kwarg-key normalization every MAKE-CONDITION caller uses,
    so a DEFINE-CONDITION slot's declared :INITARG matches what MAKE-CONDITION
    builds from the call's keyword arguments."""
    return symbol.name.lower().replace('-', '_')


def _raw_initarg_pairs(arguments):
    """Normalize an evaluated MAKE-CONDITION argument list into ordered
    (initarg-key, value) pairs, preserving call order and duplicates.

    Order matters: CLHS 7.1.2 says that when more than one initialization
    argument in the list is associated with a slot -- whether because the same
    keyword was repeated or because two different declared :INITARGs for one
    slot were both supplied -- the *leftmost* one supplied wins. Collapsing
    into a dict first (as MAKE-CONDITION used to for every condition type)
    destroys that order and lets Python's last-write-wins rule pick the wrong
    one, backwards from ANSI's leftmost-wins rule.
    """
    it = iter(arguments)
    for key in it:
        value = next(it, lisptype.NIL)
        if isinstance(key, (lisptype.LispSymbol, lisptype.lispKeyword)):
            yield _normalize_initarg(key), value


class _ConditionSlotSpec:
    """One DEFINE-CONDITION slot: its name and the (unevaluated) forms that
    fill it in -- the declared :INITARGs, in declaration order, and the
    :INITFORM, evaluated fresh per MAKE-CONDITION call rather than baked in
    at DEFINE-CONDITION time (condition-8/condition-20 pin this: their
    initforms have side effects that must run once per instance, not once
    ever)."""

    __slots__ = ('name', 'initargs', 'initform')

    def __init__(self, name, initargs, initform):
        self.name = name
        self.initargs = initargs
        self.initform = initform


def _lisp_list_items(form):
    items = []
    cur = form
    while _consp_internal(cur):
        items.append(car(cur))
        cur = cdr(cur)
    return items


def _parse_condition_slot(spec):
    """Parse one DEFINE-CONDITION slot-specifier (CLHS 9.4) into a
    `_ConditionSlotSpec` plus the :READER symbol to register, if any."""
    if isinstance(spec, lisptype.LispSymbol):
        return _ConditionSlotSpec(spec.name, [], None), None
    if not _consp_internal(spec):
        raise lisptype.LispError(f"DEFINE-CONDITION: invalid slot specifier {spec!r}")

    items = _lisp_list_items(spec)
    slot_name_sym = items[0]
    if not isinstance(slot_name_sym, lisptype.LispSymbol):
        raise lisptype.LispError("DEFINE-CONDITION: slot name must be a symbol")

    initargs = []
    initform = None
    reader = None
    i = 1
    while i < len(items):
        key = items[i]
        value = items[i + 1] if i + 1 < len(items) else lisptype.NIL
        if isinstance(key, lisptype.lispKeyword):
            key_name = key.name.upper()
            if key_name == 'INITARG' and isinstance(value, (lisptype.LispSymbol, lisptype.lispKeyword)):
                initargs.append(_normalize_initarg(value))
            elif key_name == 'INITFORM':
                initform = value
            elif key_name in ('READER', 'ACCESSOR') and isinstance(value, lisptype.LispSymbol):
                reader = value
            # :TYPE, :WRITER, :ALLOCATION, :DOCUMENTATION are accepted but not
            # modeled -- no measured test needs type checking, a writer, or
            # per-slot allocation/documentation on a condition slot.
        i += 2
    return _ConditionSlotSpec(slot_name_sym.name, initargs, initform), reader


def _condition_all_slots(cls):
    """All slots for `cls`, base classes first so a subclass redefining a
    same-named slot overrides its ancestor's definition (ordinary CLOS slot
    inheritance)."""
    merged = {}
    for klass in reversed(cls.__mro__):
        for slot in klass.__dict__.get('_direct_condition_slots', ()):
            merged[slot.name] = slot
    return merged


def _condition_all_default_initargs(cls):
    """All :DEFAULT-INITARGS for `cls`, base classes first so a subclass's own
    :DEFAULT-INITARGS overrides its ancestor's for the same initarg."""
    merged = {}
    for klass in reversed(cls.__mro__):
        merged.update(klass.__dict__.get('_direct_default_initargs', {}))
    return merged


def _condition_instance_init(self, _raw_initargs=()):
    """The shared `__init__` for every DEFINE-CONDITION-created class.

    One implementation for every user-defined condition type, rather than one
    generated per class, because the slot-filling algorithm (leftmost-supplied
    initarg wins, then :DEFAULT-INITARGS, then :INITFORM) does not depend on
    which class is being built -- only on that class's merged slot/default
    table, which `_condition_all_slots`/`_condition_all_default_initargs`
    compute from `type(self)`.
    """
    lisptype.Condition.__init__(self)
    cls = type(self)
    env = state.current_environment

    ordered = list(_raw_initarg_pairs(_raw_initargs))
    supplied_keys = {key for key, _ in ordered}

    from .evaluation_core import eval as _eval

    # :DEFAULT-INITARGS only supplies a value for an initarg genuinely absent
    # from the call -- evaluated lazily so a side-effecting default form (as
    # in condition-20) does not run when its initarg was actually supplied.
    for key, form in _condition_all_default_initargs(cls).items():
        if key not in supplied_keys:
            ordered.append((key, _eval(form, env)))

    for slot_name, spec in _condition_all_slots(cls).items():
        value = _MISSING
        for key, val in ordered:
            if key in spec.initargs:
                value = val
                break
        if value is _MISSING and spec.initform is not None:
            value = _eval(spec.initform, env)
        if value is not _MISSING:
            self._slots[slot_name] = value


def _make_condition_reader(slot_name):
    """Build the accessor a DEFINE-CONDITION :READER registers.

    Keyed purely by slot name, not by condition class: CLHS lets two unrelated
    condition types declare the same :READER name for their own same-named
    slot (condition-27a/condition-27b both do, on purpose, to test that the
    reader behaves like a generic function), and since every condition stores
    its slots the same way (`Condition._slots`), one reader per name already
    reads the right slot for whichever condition instance it is handed --
    no per-class method table is needed to get that right.

    Marked with `_condition_reader_generic` so TYPEP can answer
    `(typep #'reader 'generic-function)` truthfully (CLHS 9.4: a :READER
    defines an ordinary generic function) without this codebase's separate
    CLOS `GenericFunction`, which is not wired into FUNCALL/APPLY at all
    (plan.md Finding L) and would need to be to serve as a real accessor here.
    """
    def reader(condition):
        if not isinstance(condition, lisptype.Condition):
            raise lisptype.LispTypeError(
                f"{slot_name}: not a condition: {condition!r}",
                expected_type='condition', actual_value=condition)
        value = condition.get_slot(slot_name)
        return value if value is not None else lisptype.NIL
    reader._condition_reader_generic = True
    reader.__name__ = slot_name
    return reader


def eval_define_condition(form, env):
    """Evaluate DEFINE-CONDITION (CLHS 9.4).

    DEFINE-CONDITION used to record its name/parents/slots in a dict that
    nothing else ever read, so `(make-condition 'my-error ...)` always failed
    with "does not designate a known condition type" and every condition-type
    name it introduced was invisible to TYPEP/SUBTYPEP/HANDLER-CASE (plan.md
    C9, "DEFINE-CONDITION creates no class"). This builds a real Python class,
    a peer of the built-in ones in lisptype_extended.py, so every consumer of
    `_condition_class_for_name` (TYPEP, SUBTYPEP, HANDLER-CASE's matching,
    MAKE-CONDITION) picks it up through the one mechanism instead of a second
    one carved out for user-defined types.
    """
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispError("DEFINE-CONDITION requires a name")
    name = car(args)
    if not isinstance(name, lisptype.LispSymbol):
        raise lisptype.LispError("DEFINE-CONDITION: name must be a symbol")

    rest = cdr(args)
    parent_form = car(rest) if _consp_internal(rest) else lisptype.NIL
    rest2 = cdr(rest) if _consp_internal(rest) else lisptype.NIL
    slots_form = car(rest2) if _consp_internal(rest2) else lisptype.NIL
    option_forms = cdr(rest2) if _consp_internal(rest2) else lisptype.NIL

    parent_classes = []
    for pname in _lisp_list_items(parent_form):
        if not isinstance(pname, lisptype.LispSymbol):
            raise lisptype.LispError("DEFINE-CONDITION: parent type must be a symbol")
        pcls = _condition_class_for_name(pname.name)
        if pcls is None:
            raise lisptype.LispError(
                f"DEFINE-CONDITION: {pname.name} does not name a known condition type")
        parent_classes.append(pcls)
    if not parent_classes:
        parent_classes = [lisptype.Condition]

    slot_specs = []
    readers = []
    for slot_form in _lisp_list_items(slots_form):
        spec, reader_sym = _parse_condition_slot(slot_form)
        slot_specs.append(spec)
        if reader_sym is not None:
            readers.append((spec.name, reader_sym))

    report_spec = None
    default_initargs = {}
    documentation = None
    for opt_form in _lisp_list_items(option_forms):
        if not _consp_internal(opt_form):
            continue
        opt_items = _lisp_list_items(opt_form)
        opt_key = opt_items[0]
        if not isinstance(opt_key, lisptype.lispKeyword):
            continue
        opt_name = opt_key.name.upper()
        if opt_name == 'REPORT':
            report_value = opt_items[1] if len(opt_items) > 1 else lisptype.NIL
            if isinstance(report_value, (str, lisptype.LispString)):
                report_spec = ('string', str(report_value))
            elif isinstance(report_value, lisptype.LispSymbol):
                report_spec = ('function', report_value)
            elif _consp_internal(report_value):
                from .evaluation_core import eval as _eval
                report_spec = ('function', _eval(report_value, env))
            else:
                raise lisptype.LispError(
                    f"DEFINE-CONDITION: unsupported :REPORT value {report_value!r}")
        elif opt_name == 'DEFAULT-INITARGS':
            pairs = opt_items[1:]
            i = 0
            while i < len(pairs):
                key = pairs[i]
                value_form = pairs[i + 1] if i + 1 < len(pairs) else lisptype.NIL
                if isinstance(key, (lisptype.LispSymbol, lisptype.lispKeyword)):
                    default_initargs.setdefault(_normalize_initarg(key), value_form)
                i += 2
        elif opt_name == 'DOCUMENTATION':
            doc_value = opt_items[1] if len(opt_items) > 1 else None
            if isinstance(doc_value, (str, lisptype.LispString)):
                documentation = str(doc_value)
        # :WRITER and other slot-option-like clauses at the option level are
        # not part of CLHS 9.4 and are ignored, matching DEFINE-CONDITION's
        # existing tolerance of unrecognized options.

    class_dict = {
        '__init__': _condition_instance_init,
        '_direct_condition_slots': tuple(slot_specs),
        '_direct_default_initargs': default_initargs,
        '__doc__': documentation,
    }
    # Only set _report_spec when this class declares its own :REPORT --
    # leaving it unset lets a subclass with no :REPORT of its own inherit its
    # nearest ancestor's through ordinary Python attribute/MRO lookup, the
    # same inheritance CLOS gives PRINT-OBJECT methods.
    if report_spec is not None:
        class_dict['_report_spec'] = report_spec

    new_cls = type(name.name, tuple(parent_classes), class_dict)
    _USER_CONDITION_CLASSES[name.name.upper()] = new_cls

    global_env = env
    while global_env.parent is not None:
        global_env = global_env.parent
    for slot_name, reader_sym in readers:
        global_env.add_function(reader_sym, _make_condition_reader(slot_name))

    return name


def condition_report_text(condition):
    """Render a condition's :REPORT (CLHS 9.1.3), or None if it (and none of
    its ancestors) declared one -- the caller then falls back to the
    condition's plain message.

    A string :REPORT is used verbatim; a function (or lambda) :REPORT is
    called as `(report condition stream)`, exactly as CLHS specifies, using a
    real string-output stream so a report that calls FORMAT/WRITE-STRING on
    its stream argument (as condition-17/condition-18's do) writes into
    something this can read back, rather than a plain string the report
    function has no stream protocol to write through.
    """
    spec = getattr(type(condition), '_report_spec', None)
    if spec is None:
        return None
    kind, payload = spec
    if kind == 'string':
        return payload
    from .evaluation_core import coerce_to_function
    from .streams import StringOutputStream
    fn = coerce_to_function(payload, 'PRINT-OBJECT')
    stream = StringOutputStream()
    fn(condition, stream)
    return stream.peek_string()


def make_condition_of_type(type_designator, arguments):
    """Build an instance of the condition type named by `type_designator`
    from already-evaluated alternating keyword/value init-args, or return
    None if the designator does not name a condition type.

    This is the single condition-type-designator constructor. It replaces the
    two that used to exist side by side -- one taking unevaluated init-arg
    forms plus an environment, one taking evaluated arguments -- which were
    the same logic written twice (plan.md Finding L). Every caller
    (ERROR/SIGNAL/CERROR/WARN via build_condition, and MAKE-CONDITION) has
    already evaluated its arguments by the time it needs a condition, so the
    evaluated form is the only one actually required.
    """
    if not isinstance(type_designator, (lisptype.LispSymbol, lisptype.lispKeyword)):
        return None
    condition_class = _condition_class_for_name(type_designator.name)
    if condition_class is None:
        return None

    if '_direct_condition_slots' in condition_class.__dict__ or any(
            '_direct_condition_slots' in base.__dict__ for base in condition_class.__mro__):
        # A DEFINE-CONDITION-created class: build it from the raw ordered
        # arguments so duplicate/aliased initargs resolve leftmost-first
        # (CLHS 7.1.2), not last-write-wins as a Python dict would.
        return _apply_report(condition_class(_raw_initargs=arguments))

    kwargs = {}
    for key, value in _raw_initarg_pairs(arguments):
        # Leftmost occurrence wins here too (CLHS 7.1.2): a built-in
        # condition class's __init__ takes named Python parameters, so a
        # repeated keyword can only pick one value to pass through.
        kwargs.setdefault(key, value)
    return _apply_report(condition_class(**kwargs))


def _condition_matches(handler_type, error):
    """Check whether `error` (a signaled condition object) is of the type
    denoted by a HANDLER-BIND binding's / HANDLER-CASE clause's type
    specifier.

    For real condition objects this simply asks TYPEP. CLHS 9.1.4.1 says a
    handler's type is an ordinary *type specifier*, so it may be a compound
    form like (OR ERROR WARNING) or (NOT ERROR), or a class object such as
    (FIND-CLASS 'ERROR) -- not just a type-name symbol. TYPEP already
    understands all three, and already has a Condition branch that resolves
    condition type names to their Python classes and uses isinstance(), so
    routing through it means condition-type dispatch has exactly one
    implementation instead of a second, weaker copy here that understood only
    bare symbols (plan.md Finding E: "build the lattice once, use it twice").

    `handler_type` may also be a plain Python str, which some internal
    callers pass; TYPEP accepts that directly.
    """
    if isinstance(error, lisptype.Condition):
        from .comparison import typep
        return typep(error, handler_type) == lisptype.T

    # Legacy LispError-style exceptions predate real condition objects at
    # some raise sites (e.g. argument-validation code that raises
    # lisptype.LispTypeError directly rather than going through SIGNAL/
    # ERROR); keep matching those against the same three names as before.
    handler_type_name = (handler_type.upper() if isinstance(handler_type, str)
                         else getattr(handler_type, 'name', str(handler_type)).upper())
    if isinstance(error, lisptype.LispProgramError):
        return handler_type_name in ('PROGRAM-ERROR', 'ERROR', 'CONDITION', 'T')
    elif isinstance(error, lisptype.LispTypeError):
        return handler_type_name in ('TYPE-ERROR', 'ERROR', 'CONDITION', 'T')
    elif isinstance(error, lisptype.LispEndOfFileError):
        return handler_type_name in ('END-OF-FILE', 'STREAM-ERROR', 'ERROR', 'CONDITION', 'T')
    elif isinstance(error, lisptype.LispStreamError):
        return handler_type_name in ('STREAM-ERROR', 'ERROR', 'CONDITION', 'T')
    elif isinstance(error, lisptype.LispError):
        return handler_type_name in ('ERROR', 'CONDITION', 'T')
    return False


# --- Signaling: the handler stack, walked before unwinding (CLHS 9.1.4) ---

def signal_condition(condition):
    """Present `condition` to the active handlers, innermost first, without
    unwinding. This is the one place handlers are ever invoked.

    Returns None if every applicable handler declined by returning normally
    (which is how a handler says "not mine"); does not return at all if a
    handler transfers control, because the handler's non-local exit --
    RETURN-FROM, THROW, a HANDLER-CASE transfer, invoking a restart -- simply
    propagates out of here as a Python exception, unwinding the signaler's
    frames at that point and not before.

    Running handlers *here*, at the signal point, is the whole difference
    from the previous implementation, which called them from a Python
    `except` clause in HANDLER-BIND. By the time such an `except` runs, the
    protected form's frames are gone, so a handler's (THROW 'DONE ...) had no
    surviving CATCH frame to reach and a handler could never invoke a restart
    established inside the protected form (plan.md Finding E; ANSI test
    HANDLER-BIND.13 is the minimal case).
    """
    from .evaluation_core import funcall

    stack = state.handler_stack
    index = len(stack) - 1
    while index >= 0:
        cluster = stack[index]
        for handler_type, handler in cluster:
            if not _condition_matches(handler_type, condition):
                continue
            # CLHS 9.1.4.1: while a handler runs, the cluster that established
            # it and every cluster established inside that one are
            # disestablished, so a handler that re-signals the same condition
            # cannot re-enter itself (ANSI test HANDLER-BIND.6 relies on this:
            # its handler calls (ERROR C) again and must reach the *outer*
            # handler, not loop). Restored on the way out so the establishing
            # forms' own pops stay balanced whether the handler declines or
            # exits non-locally.
            disestablished = stack[index:]
            del stack[index:]
            try:
                funcall(handler, condition)
            finally:
                stack.extend(disestablished)
        index -= 1
    return None


def _resolve_handler(designator, env):
    """Resolve a handler's *function designator* against the lexical
    environment where the establishing form appeared.

    CLHS: HANDLER-BIND's handler is a function designator, so (handler-bind
    ((simple-error 'my-handler)) ...) -- a quoted symbol rather than a
    function object -- is legal (ANSI test HANDLER-BIND.8). A symbol with a
    lexical function binding is resolved here; anything else is handed to
    FUNCALL unchanged, which already resolves global function names and
    signals UNDEFINED-FUNCTION for names that have none, so there is no
    second designator-resolution path to keep in sync.
    """
    if isinstance(designator, lisptype.LispSymbol) and env is not None:
        lexical = env.find_func(designator)
        if lexical is not None:
            return lexical
    return designator


def _condition_of(exc):
    """The condition object carried by an unwinding exception.

    ConditionException wraps its condition in `.condition`; a plain
    LispError-style exception (an older raising convention still used in much
    of the codebase) is itself the condition object.
    """
    return exc.condition if isinstance(exc, ConditionException) else exc


def _run_handlers_on_unwind(handlers, exc):
    """Backstop for conditions that reach an establishing form by *unwinding*
    rather than by being signaled.

    Most of the codebase predates the condition system and reports errors by
    raising `lisptype.LispError` (and subclasses) directly from Python, never
    calling SIGNAL/ERROR, so those never reach `signal_condition` and their
    handlers would otherwise never run at all. This runs them here, on the way
    out, which is where *all* handlers used to run.

    That is not ANSI semantics -- the protected form's frames are gone by now,
    so a handler cannot throw into them -- and it is deliberately limited to
    the raise sites that bypass signaling. Anything that went through
    `signal_condition` is marked `handlers_run` and skipped here, so no handler
    ever runs twice for one condition. Migrating those raise sites onto SIGNAL
    is what would let this function be deleted.
    """
    if getattr(exc, 'handlers_run', False):
        return
    condition = _condition_of(exc)
    for handler_type, handler in handlers:
        if _condition_matches(handler_type, condition):
            from .evaluation_core import funcall
            # A handler that transfers control (RETURN-FROM, THROW, ...) simply
            # propagates out of here, replacing the in-flight exception.
            funcall(handler, condition)


class _HandlerCluster:
    """Context manager establishing one handler cluster for a dynamic extent.

    Used by HANDLER-BIND, HANDLER-CASE and IGNORE-ERRORS alike so that
    establishing handlers is one operation with one unwinding discipline,
    rather than three hand-rolled push/pop pairs.
    """

    def __init__(self, handlers):
        self.handlers = handlers

    def __enter__(self):
        state.handler_stack.append(self.handlers)
        return self

    def __exit__(self, exc_type, exc_value, traceback):
        # Remove by identity rather than popping the end: signal_condition
        # temporarily removes and restores a suffix of the stack while a
        # handler runs, and a handler that exits non-locally unwinds through
        # here mid-restore. Identity keeps this correct without depending on
        # that interleaving.
        stack = state.handler_stack
        for i in range(len(stack) - 1, -1, -1):
            if stack[i] is self.handlers:
                del stack[i]
                return False
        return False


def _apply_report(condition):
    """Render a simple condition's report into the slot `__str__` reads, and
    return the condition.

    CLHS 9.1.3: a simple condition reports itself by applying FORMAT to its
    FORMAT-CONTROL and FORMAT-ARGUMENTS. The two slots keep the *unrendered*
    control and arguments, so SIMPLE-CONDITION-FORMAT-CONTROL still returns the
    control string rather than the report.

    Rendering happens here, once, at construction -- not in
    `Condition.__str__`. `__str__` runs during error reporting and inside
    ConditionException's own constructor, so calling FORMAT from there would
    recurse if FORMAT ever signaled while rendering a condition. Without this
    step every error message printed the raw control string
    ("~%No test with name ~:@(~S~)." instead of the test's name), because
    Condition.__str__ returns the message slot verbatim.
    """
    from fclpy.lispfunc.io_write import format_fn

    if not isinstance(condition, lisptype.SimpleCondition):
        return condition
    control = condition.get_slot('format-control')
    if not isinstance(control, (str, lisptype.LispString)):
        # A function format control (FORMATTER's result) is left to whoever
        # reports the condition; FORMAT dispatches on it directly.
        return condition
    condition.message = format_fn(
        lisptype.NIL, str(control), *(condition.get_slot('format-arguments') or []))
    return condition


def build_condition(datum, arguments, default_class):
    """Turn an evaluated (DATUM &rest ARGUMENTS) condition designator into a
    real condition object (CLHS 9.1.2.1).

    `default_class` is the condition type a format-control datum designates,
    which is the *only* thing that differs between the signaling operators:
    SIMPLE-ERROR for ERROR and CERROR, SIMPLE-CONDITION for SIGNAL,
    SIMPLE-WARNING for WARN. Passing it in is what lets all four share one
    dispatch; previously ERROR/CERROR shared one copy and WARN had a second,
    near-identical one that had already drifted (it accepted no function
    format-control), and SIGNAL had no dispatch at all -- it signaled whatever
    its argument evaluated to, so (signal "a string") produced a generic ERROR
    and was wrongly caught by (ERROR (C) ...) handlers.

    The datum is always inspected *after* evaluation, so a string datum
    behaves the same whether it arrived as a literal or through a variable.

    Always returns a `lisptype.Condition`: a designator this function cannot
    interpret degrades to `default_class` carrying the datum's printed
    representation rather than being passed through as itself, because a
    non-condition object signaled as a condition matches no handler at all --
    not even (T (C) ...) -- and therefore escapes every enclosing handler and
    aborts the run (plan.md Finding E).
    """
    if isinstance(datum, lisptype.Condition):
        # Already a condition object (e.g. built earlier by MAKE-CONDITION);
        # signal it as-is.
        return datum

    if isinstance(datum, (str, lisptype.LispString)) or callable(datum):
        # CLHS glossary "format control": a format-control datum is either a
        # string or a function of (stream &rest args) -- e.g. the closure
        # FORMATTER returns. A function datum must be kept as the function
        # object, not stringified, because FORMAT dispatches on it directly
        # (CLHS 22.3.1); stringifying it here used to hand FORMAT the text
        # "<function ... at 0x...>" to interpret as a literal format string.
        format_control = str(datum) if isinstance(datum, (str, lisptype.LispString)) else datum
        return _apply_report(default_class(format_control=format_control,
                                          format_arguments=list(arguments)))

    built = make_condition_of_type(datum, arguments)
    if built is not None:
        # Per ANSI condition designators, a symbol naming a condition type
        # designates an instance of that type built from the remaining keyword
        # init-args -- not the bare type-name symbol itself.
        return built

    # An unrecognized designator: a symbol naming neither a built-in nor a
    # DEFINE-CONDITION-created type. Degrading to `default_class` keeps the
    # "a signaled condition is always a real condition object" invariant, and
    # keeps the severity right: such a datum used to become a generic ERROR
    # regardless of which operator signaled it, so (signal 'undefined-type)
    # was catchable as an ERROR.
    return default_class(format_control=str(datum), format_arguments=list(arguments))


def _build_condition_from_forms(datum_form, remaining_args_form, env, default_class):
    """Evaluate a signaling operator's datum and argument *forms*, then build
    the condition they designate. The special-form front end to
    `build_condition`; the function-designator entry points (`#'ERROR`,
    `#'SIGNAL`, `#'WARN` in utilities_errors.py) call `build_condition`
    directly because the registry has already evaluated their arguments.
    """
    from .evaluation_core import eval

    datum = eval(datum_form, env)
    arguments = []
    cur = remaining_args_form
    while _consp_internal(cur):
        arguments.append(eval(car(cur), env))
        cur = cdr(cur)
    return build_condition(datum, arguments, default_class)


def signal_condition_object(condition):
    """SIGNAL's runtime behavior for an already-built condition: offer it to
    the handlers, and if none takes control, return NIL (CLHS SIGNAL).

    SIGNAL returns NIL when no handler transfers control, whether or not any
    handler ran -- it does not unwind and it does not enter the debugger for a
    non-serious condition. The previous implementation raised unconditionally,
    so (signal ...) behaved like ERROR: a declining handler still lost control
    of the rest of the protected form.
    """
    signal_condition(condition)
    return lisptype.NIL


def eval_signal(form, env):
    """Implement SIGNAL special form.

    Syntax: (SIGNAL datum &rest arguments)

    Signals the condition designated by datum/arguments -- default type
    SIMPLE-CONDITION, per CLHS -- and returns NIL if no handler transfers
    control.
    """
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("SIGNAL requires a condition argument")

    condition = _build_condition_from_forms(car(args), cdr(args), env, lisptype.SimpleCondition)
    return signal_condition_object(condition)


def signal_file_error(pathname, message=None):
    """Signal a FILE-ERROR naming `pathname` (CLHS 20.1 / FILE-ERROR).

    This is the one place a file operation reports failure. Every such
    operator -- LOAD, COMPILE-FILE, OPEN, DELETE-FILE, RENAME-FILE, TRUENAME,
    PROBE-FILE, DIRECTORY -- previously let Python's own `FileNotFoundError`
    escape, which is not a condition at all: it matched no handler clause, so
    `(signals-error (load "nope") file-error)` saw the Python exception
    surface as the *value* of the form (prompt.txt: "Python exceptions must
    not appear as Lisp values"), and HANDLER-CASE could not distinguish "the
    file was missing" from "the implementation broke".

    Routes through `signal_error_object`, so handlers run at the signal point
    with the caller's CATCH/RESTART-CASE/UNWIND-PROTECT frames still live,
    exactly as for any other ERROR.
    """
    condition = lisptype.FileError(pathname=pathname,
                                   message=message or '')
    return signal_error_object(condition)


def signal_error_object(condition, recoverable=False, continue_format=None):
    """ERROR's/CERROR's runtime behavior for an already-built condition: offer
    it to the handlers first, and only if none takes control unwind by raising.

    ERROR never returns (CLHS): if every handler declines, the condition goes
    to the debugger, which here means propagating out as a ConditionException.
    That raise happens *after* the handler walk, not instead of it, which is
    the point -- a handler now runs while the signaling form's CATCH,
    RESTART-CASE and UNWIND-PROTECT frames are all still live.

    The raised exception is marked `handlers_run` so the unwinding backstop in
    HANDLER-BIND/HANDLER-CASE (still needed for raise sites that bypass
    signaling entirely -- see eval_handler_bind) can tell "already offered to
    the handlers and declined" from "never offered", and so never runs a
    handler twice for one condition.
    """
    exception = ConditionException(condition, recoverable=recoverable)
    exception.handlers_run = True
    if continue_format is not None:
        exception.continue_format = continue_format
    signal_condition(condition)
    raise exception


def eval_error(form, env):
    """Implement ERROR special form.

    Syntax: (ERROR) or (ERROR condition-object) or (ERROR format-control &rest format-arguments)

    Signals the error designated by datum/arguments -- default type
    SIMPLE-ERROR -- and does not return.
    """
    args = cdr(form)

    # If no arguments, create a generic error
    if not _consp_internal(args):
        return signal_error_object(lisptype.Error(message="Unspecified error"))

    condition = _build_condition_from_forms(car(args), cdr(args), env, lisptype.SimpleError)
    return signal_error_object(condition)


def eval_cerror(form, env):
    """Implement CERROR special form.
    
    Syntax: (CERROR continue-format-control condition &optional (format-control) format-args...)
    
    Signal an error that has a built-in continue restart. If the user continues,
    CERROR returns NIL and execution resumes.
    """
    args = cdr(form)
    if not _consp_internal(args) or not _consp_internal(cdr(args)):
        raise lisptype.LispNotImplementedError("CERROR requires at least condition argument")

    continue_format = car(args)  # Format for the continue option
    condition_form = car(cdr(args))
    # CLHS 9.1: cerror's datum/arguments behave "as if by (apply #'error
    # datum arguments)" -- same dispatch as ERROR, including string datums
    # from a variable building a proper SIMPLE-ERROR.
    remaining_args_form = cdr(cdr(args))

    condition = _build_condition_from_forms(
        condition_form, remaining_args_form, env, lisptype.SimpleError)

    # Recoverable: CERROR's condition carries a CONTINUE restart. Handlers get
    # to run before any unwinding, same as ERROR.
    return signal_error_object(condition, recoverable=True, continue_format=continue_format)


def signal_warning(datum, arguments):
    """WARN's runtime behavior: build the warning designated by an already
    evaluated (DATUM &rest ARGUMENTS), offer it to the handlers, and report it
    on *ERROR-OUTPUT* only if no handler took control. Returns NIL.

    Shared by the WARN special form (eval_warn) and the WARN function
    designator (warn_fn in utilities_errors.py, used by FUNCALL/APPLY/#'WARN)
    so there is exactly one place that knows how a warning is built and
    reported. Condition construction is now `build_condition`'s job, the same
    dispatch ERROR/CERROR/SIGNAL use, rather than a fourth private copy of it.

    Now that handlers run before unwinding, a HANDLER-BIND on WARNING /
    SIMPLE-WARNING / STYLE-WARNING actually sees the warning and can transfer
    control out of it -- previously WARN never consulted a handler at all and
    unconditionally printed.
    """
    condition = build_condition(datum, arguments, lisptype.SimpleWarning)
    signal_condition(condition)

    # No handler transferred control, so WARN reports the warning itself.
    # MUFFLE-WARNING -- the restart that suppresses this report -- needs the
    # restart system M8's second half covers; until then a declining handler
    # cannot suppress the report, which is WARN's correct *unhandled* behavior.
    print(f"Warning: {condition}")
    return lisptype.NIL


def eval_warn(form, env):
    """Implement WARN special form.

    Syntax: (WARN format-control &rest format-arguments) or (WARN condition-designator ...)

    Signal a warning condition. Unlike ERROR, warnings don't require handling
    and execution normally continues.
    """
    from .evaluation_core import eval

    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("WARN requires at least one argument")

    datum = eval(car(args), env)
    arguments = []
    cur = cdr(args)
    while _consp_internal(cur):
        arguments.append(eval(car(cur), env))
        cur = cdr(cur)

    return signal_warning(datum, arguments)


def eval_restart_case(form, env):
    """Implement RESTART-CASE special form.
    
    Syntax: (RESTART-CASE protected-form {restart-clause}*)
    
    Establishes named restarts with handlers that can be invoked during condition handling.
    """
    from .evaluation_core import eval
    
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("RESTART-CASE requires a protected form")
    
    protected_form = car(args)
    restart_clauses = cdr(args)
    
    # Parse restart clauses into handlers
    restarts = {}
    current = restart_clauses
    while _consp_internal(current):
        clause = car(current)
        if _consp_internal(clause):
            restart_name = car(clause)
            clause_body = cdr(clause)
            
            if isinstance(restart_name, lisptype.LispSymbol):
                # Create handler that evaluates the clause body
                def make_handler(body):
                    def handler(*args):
                        result = lisptype.NIL
                        current_body = body
                        while _consp_internal(current_body):
                            result = eval(car(current_body), env)
                            current_body = cdr(current_body)
                        return result
                    return handler
                
                restarts[restart_name.name] = make_handler(clause_body)
        
        current = cdr(current)
    
    # Push restarts onto stack
    state.restart_stack.append(restarts)
    
    try:
        # Evaluate protected form
        result = eval(protected_form, env)
        return result
    except lisptype.RestartException as e:
        # Restart was invoked
        if e.restart_name in restarts:
            handler = restarts[e.restart_name]
            return handler(*e.args)
        raise
    finally:
        # Pop restarts from stack
        state.restart_stack.pop()


def eval_restart_bind(form, env):
    """Implement RESTART-BIND special form.
    
    Syntax: (RESTART-BIND ((name function) ...) {body}*)
    
    Binds restart functions for invocation.
    """
    from .evaluation_core import eval
    
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("RESTART-BIND requires bindings")
    
    binding_clauses = car(args)
    body_forms = cdr(args)
    
    # Parse bindings
    restarts = {}
    current = binding_clauses
    while _consp_internal(current):
        binding = car(current)
        if _consp_internal(binding) and _consp_internal(cdr(binding)):
            restart_name = car(binding)
            handler_form = car(cdr(binding))
            
            handler = eval(handler_form, env)
            
            if isinstance(restart_name, lisptype.LispSymbol):
                restarts[restart_name.name] = handler
        
        current = cdr(current)
    
    # Push restarts onto stack
    state.restart_stack.append(restarts)
    
    try:
        # Evaluate body
        result = lisptype.NIL
        current = body_forms
        while _consp_internal(current):
            result = eval(car(current), env)
            current = cdr(current)
        return result
    finally:
        # Pop restarts from stack
        state.restart_stack.pop()


def eval_invoke_restart(form, env):
    """Implement INVOKE-RESTART special form.
    
    Syntax: (INVOKE-RESTART restart-name &rest arguments)
    
    Invokes a restart by name.
    """
    from .evaluation_core import eval
    
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("INVOKE-RESTART requires a restart name")
    
    restart_name_form = car(args)
    restart_args = cdr(args)
    
    # Evaluate restart name
    if isinstance(restart_name_form, lisptype.LispSymbol):
        restart_name = restart_name_form.name
    else:
        restart_name = str(eval(restart_name_form, env))
    
    # Evaluate arguments
    evaluated_args = []
    current = restart_args
    while _consp_internal(current):
        evaluated_args.append(eval(car(current), env))
        current = cdr(current)
    
    # Search restart stack
    for restarts in reversed(state.restart_stack):
        if restart_name in restarts:
            handler = restarts[restart_name]
            result = handler(*evaluated_args) if evaluated_args else handler()
            raise lisptype.RestartException(restart_name, [result])
    
    # Restart not found
    raise lisptype.LispError(f"No restart named {restart_name}")


def eval_abort(form, env):
    """Implement ABORT special form.
    
    Syntax: (ABORT)
    
    Invokes the ABORT restart.
    """
    # Try to invoke ABORT restart
    for restarts in reversed(state.restart_stack):
        if 'ABORT' in restarts:
            handler = restarts['ABORT']
            result = handler()
            raise lisptype.RestartException('ABORT', [result])
    
    # No ABORT restart found
    raise lisptype.LispError("ABORT: No abort restart available")


def _assign_variable_or_place(var, result, env):
    """Assign `result` to `var`, honoring a SYMBOL-MACROLET expansion.

    If `var` is a plain variable name, SETQ it directly. If it names a
    symbol-macro (e.g. established by SYMBOL-MACROLET binding it to
    (CAR X)), re-evaluate the expansion's sub-forms fresh (they may have
    side effects, per ANSI) and mutate the resulting place. Only CAR/CDR
    expansions are supported as places here; anything else falls back to
    plain variable assignment.
    """
    from .evaluation_core import eval

    expansion = env.get_symbol_macro(var)
    if expansion is None:
        env.set_variable(var, result)
        return

    if isinstance(expansion, lisptype.LispSymbol):
        _assign_variable_or_place(expansion, result, env)
        return

    if _consp_internal(expansion) and isinstance(car(expansion), lisptype.LispSymbol):
        op_name = car(expansion).name
        place_args = cdr(expansion)
        if op_name == 'CAR' and _consp_internal(place_args):
            target = eval(car(place_args), env)
            if _consp_internal(target):
                target.car = result
                return
        elif op_name == 'CDR' and _consp_internal(place_args):
            target = eval(car(place_args), env)
            if _consp_internal(target):
                target.cdr = result
                return

    raise lisptype.LispNotImplementedError(
        f"MULTIPLE-VALUE-SETQ: unsupported symbol-macro place expansion for {var}")


@_registry.cl_special('MULTIPLE-VALUE-SETQ')
def eval_multiple_value_setq(form, env):
    """Evaluate MULTIPLE-VALUE-SETQ special form.

    Syntax: (MULTIPLE-VALUE-SETQ (var1 var2 ...) value-form)

    Evaluates value-form once. If it returns a MultipleValues, each
    variable is SETQ'd to the corresponding value (or NIL if there aren't
    enough values). If it returns a single value, the first variable gets
    that value and the rest get NIL. Returns the primary (first) value of
    value-form, regardless of how many variables are given. A var naming a
    symbol-macro is assigned through its expansion (see
    _assign_variable_or_place) rather than as a plain variable.
    """
    from .evaluation_core import eval

    args = cdr(form)
    if not _consp_internal(args) or not _consp_internal(cdr(args)):
        raise lisptype.LispNotImplementedError("MULTIPLE-VALUE-SETQ requires vars and a value-form")

    vars = car(args)
    value_form = car(cdr(args))

    values = eval(value_form, env)

    var_list = []
    current = vars
    while _consp_internal(current):
        var_list.append(car(current))
        current = cdr(current)

    if isinstance(values, lisptype.MultipleValues):
        value_tuple = values.get_all()
        for i, var in enumerate(var_list):
            _assign_variable_or_place(var, value_tuple[i] if i < len(value_tuple) else lisptype.NIL, env)
        return value_tuple[0] if value_tuple else lisptype.NIL
    else:
        primary = values if values is not None else lisptype.NIL
        for i, var in enumerate(var_list):
            _assign_variable_or_place(var, primary if i == 0 else lisptype.NIL, env)
        return primary


@_registry.cl_special('MULTIPLE-VALUE-PROG1')
def eval_multiple_value_prog1(form, env):
    """Evaluate MULTIPLE-VALUE-PROG1 special form.

    Syntax: (MULTIPLE-VALUE-PROG1 first-form form*)

    Evaluates first-form, saving all of its values (primary and any
    secondary values). Then evaluates the remaining forms in order, for
    effect only, discarding their results. Finally returns the saved
    values from first-form.
    """
    from .evaluation_core import eval

    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("MULTIPLE-VALUE-PROG1 requires at least one form")

    saved_values = eval(car(args), env)

    rest = cdr(args)
    while _consp_internal(rest):
        eval(car(rest), env)
        rest = cdr(rest)

    return saved_values


@_registry.cl_special('MULTIPLE-VALUE-CALL')
def eval_multiple_value_call(form, env):
    """Evaluate MULTIPLE-VALUE-CALL special form.
    
    Syntax: (MULTIPLE-VALUE-CALL function value-form1 value-form2 ...)
    
    Each value-form is evaluated. If a value-form returns a MultipleValues,
    all its values are passed as separate arguments to the function.
    If it returns a single value, that value is passed as one argument.
    
    Returns the result of calling the function.
    """
    from .evaluation_core import eval
    
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("MULTIPLE-VALUE-CALL requires at least a function")
    
    # Evaluate the function form
    function_form = car(args)
    func = eval(function_form, env)
    
    # If func is a symbol, look it up in the environment (function position)
    if isinstance(func, lisptype.LispSymbol):
        func = env.find_func(func)
        if func is None:
            # Try auto-loading from registry
            try:
                from . import registry as _registry
                py_name = _registry.get_function_py_name(function_form.name)
                if py_name:
                    func = getattr(lispfunc, py_name, None)
                    if func:
                        # Bind into environment for future lookups
                        env.add_function(function_form, func)
            except Exception:
                pass
        if func is None:
            raise lisptype.LispNotImplementedError(f"MULTIPLE-VALUE-CALL: undefined function: {function_form}")
    
    # Collect all arguments from the value forms
    call_args = []
    value_forms = cdr(args)
    while _consp_internal(value_forms):
        result = eval(car(value_forms), env)
        if isinstance(result, lisptype.MultipleValues):
            # Add all values from MultipleValues
            call_args.extend(result.get_all())
        else:
            # Add single value
            call_args.append(result)
        value_forms = cdr(value_forms)
    
    # Call the function with collected arguments
    if callable(func):
        return func(*call_args) if call_args else func()
    else:
        raise lisptype.LispNotImplementedError(f"MULTIPLE-VALUE-CALL: not a function: {func}")


@_registry.cl_special('MULTIPLE-VALUE-BIND')
def eval_multiple_value_bind(form, env):
    """Evaluate MULTIPLE-VALUE-BIND special form.
    
    Syntax: (MULTIPLE-VALUE-BIND (var1 var2 ...) value-form body...)
    
    Evaluates value-form. If it returns a MultipleValues, each variable
    is bound to the corresponding value (or NIL if there aren't enough values).
    If it returns a single value, the first variable gets that value and
    others get NIL. Then evaluates the body forms.
    
    Returns the value of the last body form.
    """
    from .evaluation_core import eval
    
    args = cdr(form)
    if not _consp_internal(args) or not _consp_internal(cdr(args)):
        raise lisptype.LispNotImplementedError("MULTIPLE-VALUE-BIND requires vars, value-form, and body")
    
    # Extract the variable list and value-form
    vars = car(args)
    value_form = car(cdr(args))
    body = cdr(cdr(args))
    
    # Evaluate the value-form
    values = eval(value_form, env)
    
    # Create new environment for bindings
    new_env = lisptype.Environment(parent=env)
    
    # Extract variable list (it's a Lisp list of symbols)
    var_list = []
    current = vars
    while _consp_internal(current):
        var = car(current)
        var_list.append(var)
        current = cdr(current)
    
    # Bind variables to values
    if isinstance(values, lisptype.MultipleValues):
        value_tuple = values.get_all()
        for i, var in enumerate(var_list):
            if i < len(value_tuple):
                new_env.add_variable(var, value_tuple[i])
            else:
                new_env.add_variable(var, lisptype.NIL)
    else:
        # Single value - bind to first variable, rest get NIL
        if var_list:
            new_env.add_variable(var_list[0], values)
            for var in var_list[1:]:
                new_env.add_variable(var, lisptype.NIL)
    
    # Evaluate body forms and return last result
    result = lisptype.NIL
    while _consp_internal(body):
        result = eval(car(body), new_env)
        body = cdr(body)
    
    return result


def eval_handler_bind(form, env):
    """Implement HANDLER-BIND special form.

    Syntax: (HANDLER-BIND (binding*) form*)

    Where each binding is: (condition-type handler-function)

    Establishes condition handlers for the dynamic extent of the body forms.
    If a condition matching one of the types is signaled, the corresponding
    handler function is called with the condition object. Unlike HANDLER-CASE,
    HANDLER-BIND does not itself transfer control: if the handler returns
    normally (rather than performing a non-local exit via RETURN-FROM, THROW,
    a restart, etc.), signaling continues outward past this HANDLER-BIND.

    Note: bindings may be NIL (empty), which is common for #+/-sbcl conditional
    code that excludes certain bindings for non-SBCL implementations; an empty
    binding list simply means nothing here can handle the condition.

    Implementation: the bindings are pushed onto `state.handler_stack` for the
    dynamic extent of the body and invoked by `signal_condition` at the signal
    point. HANDLER-BIND itself catches nothing -- which is exactly why a
    handler can now THROW to a tag or invoke a restart established *inside*
    the protected form (ANSI test HANDLER-BIND.13). Running handlers from a
    Python `except` here, as this used to, meant those frames were already
    gone (plan.md Finding E).
    """
    from .evaluation_core import eval

    args = cdr(form)
    if not _consp_internal(args):
        return lisptype.NIL

    bindings = car(args)
    body = cdr(args)

    handlers = []
    current = bindings
    while _consp_internal(current):
        binding = car(current)
        if _consp_internal(binding) and _consp_internal(cdr(binding)):
            condition_type = car(binding)
            handler_form = car(cdr(binding))
            handlers.append((condition_type, _resolve_handler(eval(handler_form, env), env)))
        current = cdr(current)

    def run_body():
        result = lisptype.NIL
        cur = body
        while _consp_internal(cur):
            result = eval(car(cur), env)
            cur = cdr(cur)
        return result

    # The try/except sits *outside* the `with` so the cluster is already
    # disestablished if the legacy backstop below has to run a handler.
    try:
        with _HandlerCluster(handlers):
            return run_body()
    except (ConditionException, lisptype.LispError) as exc:
        _run_handlers_on_unwind(handlers, exc)
        raise


def _run_handler_case_clause(clause, condition, env):
    """Evaluate one HANDLER-CASE clause body, binding its optional variable to
    the condition. Runs after unwinding, with the handlers disestablished.
    """
    from .evaluation_core import eval

    var_list = car(cdr(clause))
    clause_body = cdr(cdr(clause))

    new_env = lisptype.Environment(parent=env)
    if _consp_internal(var_list):
        new_env.add_variable(car(var_list), condition)

    result = lisptype.NIL
    while _consp_internal(clause_body):
        result = eval(car(clause_body), new_env)
        clause_body = cdr(clause_body)
    return result


def eval_handler_case(form, env):
    """Implement HANDLER-CASE special form.

    Syntax: (HANDLER-CASE expression
              (condition-type ([var]) form*) ...)

    Evaluates expression. If a condition of one of the specified types is
    signaled, the stack unwinds back to this form and the matching clause's
    body is evaluated, with `var` (if given) bound to the condition.

    Implementation: HANDLER-CASE establishes a handler cluster like
    HANDLER-BIND does, whose handlers immediately transfer control back here
    (CLHS defines HANDLER-CASE in exactly those terms). Sharing one mechanism
    is what makes handler *ordering* right between the two forms: an inner
    HANDLER-BIND handler must get the condition before an outer HANDLER-CASE
    clause, which cannot happen while one form walks a handler stack and the
    other catches a Python exception. The clause body runs outside the `with`,
    so the handlers are disestablished first, as ANSI requires.
    """
    from .evaluation_core import eval

    args = cdr(form)

    if not _consp_internal(args):
        return lisptype.NIL

    expression = car(args)
    clauses = cdr(args)

    tag = HandlerCaseTag()
    handlers = []
    clause_list = []
    current = clauses
    while _consp_internal(current):
        clause = car(current)
        if _consp_internal(clause):
            clause_list.append(clause)

            def make_handler(the_clause):
                def transfer(condition):
                    raise HandlerCaseTransfer(tag, the_clause, condition)
                return transfer

            handlers.append((car(clause), make_handler(clause)))
        current = cdr(current)

    try:
        with _HandlerCluster(handlers):
            return eval(expression, env)
    except HandlerCaseTransfer as transfer:
        if transfer.tag is not tag:
            # Belongs to an enclosing HANDLER-CASE; let it through.
            raise
        return _run_handler_case_clause(transfer.clause, transfer.condition, env)
    except ThrowException as e:
        # An uncaught THROW is a CONTROL-ERROR (CLHS 5.2). Converting it here
        # rather than at the THROW itself is a known approximation: it needs a
        # catch-tag stack to know at throw time that no tag matches (M7). A
        # clause matching CONTROL-ERROR therefore still gets a chance.
        control_error = lisptype.ControlError(message=f"Uncaught THROW {e.tag}")
        for clause in clause_list:
            if _condition_matches(car(clause), control_error):
                return _run_handler_case_clause(clause, control_error, env)
        raise
    except (ConditionException, lisptype.LispError) as exc:
        # Backstop for conditions raised without being signaled -- see
        # _run_handlers_on_unwind. For HANDLER-CASE this is semantically fine
        # (HANDLER-CASE unwinds before running its clause anyway), so matching
        # here yields correct behavior rather than an approximation.
        if getattr(exc, 'handlers_run', False):
            raise
        condition = _condition_of(exc)
        for clause in clause_list:
            if _condition_matches(car(clause), condition):
                return _run_handler_case_clause(clause, condition, env)
        raise


def eval_ignore_errors(form, env):
    """Implement IGNORE-ERRORS special form.

    Syntax: (IGNORE-ERRORS form*)

    Evaluates the body forms in sequence. If any form signals an error,
    execution stops and IGNORE-ERRORS returns two values: NIL and the
    condition object. If no error occurs, returns the primary value of the
    last form and NIL.

    CLHS defines this as (HANDLER-CASE (PROGN form*) (ERROR (C) (VALUES NIL
    C))), and that is how it is implemented -- as an ERROR handler on the same
    handler stack -- so it participates in handler ordering like any other
    establishing form instead of being a third, independent way to intercept
    errors.

    Two defects this replaces, both of the "catches too much" kind: the
    previous `except Exception` swallowed the control-transfer exceptions as
    well, so (IGNORE-ERRORS (RETURN-FROM F 1)) silently discarded the
    RETURN-FROM (the same defect as plan.md Finding K's `funcall` bug); and it
    returned `str(e)` as the second value, i.e. a Python string where ANSI
    requires the condition object.
    """
    from .evaluation_core import eval

    args = cdr(form)
    tag = HandlerCaseTag()

    def transfer(condition):
        raise HandlerCaseTransfer(tag, None, condition)

    handlers = [('ERROR', transfer)]

    try:
        with _HandlerCluster(handlers):
            result = lisptype.NIL
            cur = args
            while _consp_internal(cur):
                result = eval(car(cur), env)
                cur = cdr(cur)
            # CLHS: "If the execution of forms is completed normally,
            # ignore-errors returns whatever values the forms return" -- not
            # the primary value padded with a forced NIL. Forcing a second
            # value here made `(ignore-errors (values 1 2 3))` answer `(1
            # NIL)` instead of `(1 2 3)`, which is wrong even in the common
            # single-value case: any caller capturing *all* values (
            # MULTIPLE-VALUE-LIST, MULTIPLE-VALUE-BIND with >1 var) saw an
            # extra NIL that was never there.
            return result
    except HandlerCaseTransfer as transferred:
        if transferred.tag is not tag:
            raise
        return lisptype.MultipleValues(lisptype.NIL, transferred.condition)
    except (ConditionException, lisptype.LispError) as exc:
        # Raised without being signaled -- see _run_handlers_on_unwind.
        if getattr(exc, 'handlers_run', False):
            raise
        condition = _condition_of(exc)
        if _condition_matches('ERROR', condition):
            return lisptype.MultipleValues(lisptype.NIL, condition)
        raise
    except (ReturnFromException, ThrowException, GoException):
        # Non-local control transfers are not conditions and must pass through.
        raise
    except Exception as exc:  # noqa: BLE001
        # A bare Python exception escaping the evaluator is an fclpy bug, not a
        # Lisp condition. It is converted here rather than passed through so a
        # single interpreter defect cannot abort a whole ANSI suite run, but it
        # is converted into a *real* condition object so nothing downstream
        # sees a Python value. plan.md's M0 structural observation asks for
        # this conversion to happen once at the EVAL boundary instead; when it
        # does, this clause should be deleted rather than kept in parallel.
        return lisptype.MultipleValues(
            lisptype.NIL,
            lisptype.Error(message=f"{type(exc).__name__}: {exc}"))


__all__ = [
    'eval_define_condition',
    'eval_signal',
    'eval_error',
    'eval_cerror',
    'eval_warn',
    'eval_restart_case',
    'eval_restart_bind',
    'eval_invoke_restart',
    'eval_abort',
    'eval_multiple_value_setq',
    'eval_multiple_value_call',
    'eval_multiple_value_bind',
    'eval_handler_bind',
    'eval_handler_case',
    'eval_ignore_errors',
]
