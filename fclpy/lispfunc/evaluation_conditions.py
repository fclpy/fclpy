"""Condition handling: SIGNAL, ERROR, restarts, multiple values."""

import fclpy.state as state
import fclpy.lisptype as lisptype
from .core import car, cdr, cons, _consp_internal
from . import registry as _registry
from .evaluation_core import (
    ConditionException, ThrowException, ReturnFromException, GoException,
    HandlerCaseTag, HandlerCaseTransfer, RestartCaseTag, RestartCaseTransfer)
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


def _iter_list(form):
    """Yield the elements of a Lisp list `form` (a lispCons chain, or NIL for
    none). Shared by every restart-parsing walk below instead of each
    re-writing its own `while _consp_internal(cur): ... cur = cdr(cur)`."""
    cur = form
    while _consp_internal(cur):
        yield car(cur)
        cur = cdr(cur)


def _list_from(seq):
    """Build a Lisp list from a Python sequence -- the inverse of `_iter_list`."""
    result = lisptype.NIL
    for element in reversed(list(seq)):
        result = cons(element, result)
    return result


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


def restart_report_text(restart):
    """Render a restart's report function (CLHS 9.1) into a string, for the
    printer's PRINC/`~A`-style representation of a `lisptype.Restart`
    (restart-bind.16/20-22). Returns None if the restart has no report
    function at all.
    """
    if restart.report_function is None:
        return None
    from .evaluation_core import funcall
    from .streams import StringOutputStream
    stream = StringOutputStream()
    funcall(restart.report_function, stream)
    return stream.peek_string()


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
    import fclpy.classes as classes
    if isinstance(type_designator, type) and issubclass(type_designator, lisptype.Condition):
        # A raw Python condition class, as some callers pass directly.
        condition_class = type_designator
    elif isinstance(type_designator, classes.LispClass):
        # FIND-CLASS returns a CLOS `LispClass` wrapper even for a built-in
        # condition type (classes._init_builtin_classes registers one for
        # every name in its `condition_classes` set), so
        # `(make-condition (find-class 'error))` -- make-condition.2/.3/.4's
        # own construction, over every condition type in
        # *CL-CONDITION-TYPE-SYMBOLS* -- handed this designator shape, not a
        # symbol, and `_condition_class_for_name` only understands a name.
        condition_class = _condition_class_for_name(type_designator.name.name)
        if condition_class is None:
            return None
    elif isinstance(type_designator, (lisptype.LispSymbol, lisptype.lispKeyword)):
        condition_class = _condition_class_for_name(type_designator.name)
        if condition_class is None:
            return None
    elif _consp_internal(type_designator):
        # A compound condition type specifier (CLHS 9.1.2.1's
        # "condition type specifier" is not restricted to a bare name).
        # CLHS does not say which instance a compound specifier builds --
        # ansi-test marks make-condition.3/.4 `:ansi-spec-problem` for
        # exactly that -- so this is a documented implementation-defined
        # choice: build an instance of the *first* component that names a
        # known condition type. For an OR the result genuinely satisfies
        # the specifier (an instance of a disjunct is an instance of the
        # OR); for an AND that is not decidable in general, and the
        # ansi-test's own subtypep guards accept the first-conjunct build.
        # Anything else still falls through to the caller's error, and a
        # component that names no known type keeps the designator unknown.
        head = car(type_designator)
        head_name = head.name if isinstance(head, (lisptype.LispSymbol, lisptype.lispKeyword)) else None
        if head_name in ('OR', 'AND'):
            for item in _lisp_list_items(cdr(type_designator)):
                if isinstance(item, (lisptype.LispSymbol, lisptype.lispKeyword)):
                    built = make_condition_of_type(item, arguments)
                    if built is not None:
                        return built
        return None
    else:
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
    # `format_fn` to NIL returns a LispString (CLHS 22.3.1); the message slot
    # is read back as a plain Python str (Condition.__str__ returns it
    # verbatim), so coerce here rather than at every reader.
    condition.message = str(format_fn(
        lisptype.NIL, str(control), *(condition.get_slot('format-arguments') or [])))
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
        signal_error_object(lisptype.ProgramError(
            message="SIGNAL requires a condition argument"))

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


def _make_case_transfer(tag, clause_index):
    """The `function` slot of a RESTART-CASE-style `Restart`: invoking it
    never performs the recovery itself, it captures the arguments and
    performs the clause's implicit non-local exit (CLHS 9.2) back to
    whichever form established it -- RESTART-CASE itself, or one of the
    built-in restarts below that are RESTART-CASE in spirit (CERROR's
    CONTINUE, WARN's MUFFLE-WARNING) without the user-facing macro syntax."""
    def _transfer(*args):
        raise RestartCaseTransfer(tag, clause_index, args)
    return _transfer


def _string_report_function(text):
    """A `report_function` that writes a fixed string -- CLHS 9.1's rule that
    a literal-string :REPORT/report-generation-argument is used directly,
    never coerced to a function the way a symbol or lambda-expression is."""
    def _report(stream):
        from .io_write import write_text
        write_text(text, stream)
    return _report


def _format_report_function(format_control):
    """A `report_function` that writes `format_control` via FORMAT with no
    arguments -- CERROR's continue-format-control (CLHS 9.1), which may
    itself be a function (a FORMATTER result), so this must go through
    FORMAT's own dispatch rather than assuming a string."""
    def _report(stream):
        from .io_write import format_fn
        format_fn(stream, format_control)
    return _report


def _signal_warning_object(condition):
    """WARN's runtime core given an already-built condition: offer it to the
    handlers, with an implicit MUFFLE-WARNING restart around the offer (CLHS
    9.1: WARN's protected form behaves as if wrapped in a RESTART-CASE whose
    only clause is MUFFLE-WARNING); if nothing transfers control or invokes
    that restart, WARN reports the warning itself and returns NIL.

    Shared by the WARN special form (`eval_warn`), the WARN function
    designator (`warn_fn` in utilities_errors.py), and RESTART-CASE's
    auto-association dispatch (`_dispatch_restart_case_signal`) for a
    protected form that is literally `(WARN ...)` -- one place that knows how
    a warning is reported, not three.
    """
    tag = RestartCaseTag()
    restart = lisptype.Restart(
        lisptype.LispSymbol('MUFFLE-WARNING'), _make_case_transfer(tag, 0),
        report_function=_string_report_function("Skip the warning."))
    restart.associated_conditions.append(condition)

    state.restart_stack.append([restart])
    try:
        try:
            signal_condition(condition)
        except RestartCaseTransfer as exc:
            if exc.tag is not tag:
                raise
            return lisptype.NIL
    finally:
        state.restart_stack.pop()

    # No handler transferred control and MUFFLE-WARNING was not invoked, so
    # WARN reports the warning itself, on the *value* of *ERROR-OUTPUT* --
    # not unconditionally on Python's stdout, which is what a plain print()
    # did and is why `(with-output-to-string (*error-output*) (warn ...))`
    # (warn.4) always saw the empty string no matter what WARN did.
    from .binding import dynamic_value
    from .io_write import write_text
    report = condition_report_text(condition)
    error_output_symbol = lisptype.COMMON_LISP_PACKAGE.intern_symbol('*ERROR-OUTPUT*')
    write_text(f"Warning: {report if report is not None else condition}\n",
               dynamic_value(error_output_symbol))
    return lisptype.NIL


def signal_warning(datum, arguments):
    """WARN's runtime behavior given an evaluated (DATUM &rest ARGUMENTS)
    condition designator: build it (`build_condition`, the same dispatch
    ERROR/CERROR/SIGNAL use), validate it (CLHS 9.2), delegate to
    `_signal_warning_object`.

    Two validations the tests pin, both CLHS 9.2's "is signaled of type
    type-error" clauses:
    * the condition WARN signals must actually be a WARNING -- the symbol
      datums `CONDITION`/`SIMPLE-CONDITION` name *types* that are not
      warnings (warn.12/.13), and instances of CONDITION/
      SIMPLE-CONDITION/SIMPLE-ERROR are not warnings either (warn.16/.17/
      .18). Previously such a datum was signaled as-is: a non-warning
      condition offered to WARNING handlers, which no conforming program
      could be expected to handle.
    * a condition *instance* datum takes no format arguments -- the
      initargs were already fixed when MAKE-CONDITION built it (warn.14).
    The TYPE-ERROR goes through `signal_error_object`, so it is a real
    signaled condition: `signals-error` sees it, and a caller's handler
    could catch it.
    """
    condition = build_condition(datum, arguments, lisptype.SimpleWarning)
    if not isinstance(condition, lisptype.Warning):
        signal_error_object(lisptype.TypeError(
            datum=condition,
            expected_type='warning',
            message=f"WARN: the condition designated is not a warning: {condition}"))
    if isinstance(datum, lisptype.Condition) and arguments:
        signal_error_object(lisptype.TypeError(
            datum=datum,
            expected_type='format-control',
            message=f"WARN: format arguments were supplied with a condition datum: {datum}"))
    return _signal_warning_object(condition)


def eval_warn(form, env):
    """Implement WARN special form.

    Syntax: (WARN format-control &rest format-arguments) or (WARN condition-designator ...)

    Signal a warning condition. Unlike ERROR, warnings don't require handling
    and execution normally continues.
    """
    from .evaluation_core import eval

    args = cdr(form)
    if not _consp_internal(args):
        signal_error_object(lisptype.ProgramError(
            message="WARN requires at least one argument"))

    datum = eval(car(args), env)
    arguments = [eval(a, env) for a in _iter_list(cdr(args))]
    return signal_warning(datum, arguments)


def _signal_cerror_object(condition, continue_format):
    """CERROR's runtime core given an already-built condition: offer it to
    the handlers with an implicit CONTINUE restart around the offer (CLHS
    9.1); if that restart is invoked (directly, or via the CONTINUE
    function), CERROR returns NIL and its caller resumes. Otherwise -- no
    handler transferred control -- CERROR does not return, same as ERROR.

    Shared by the CERROR special form (`eval_cerror`), a CERROR function
    designator, and RESTART-CASE's auto-association dispatch for a protected
    form that is literally `(CERROR ...)`.
    """
    tag = RestartCaseTag()
    restart = lisptype.Restart(
        lisptype.LispSymbol('CONTINUE'), _make_case_transfer(tag, 0),
        report_function=_format_report_function(continue_format))
    restart.associated_conditions.append(condition)

    state.restart_stack.append([restart])
    try:
        try:
            return signal_error_object(condition)
        except RestartCaseTransfer as exc:
            if exc.tag is not tag:
                raise
            return lisptype.NIL
    finally:
        state.restart_stack.pop()


def eval_cerror(form, env):
    """Implement CERROR special form.

    Syntax: (CERROR continue-format-control datum &rest arguments)

    Signals an error that has a built-in CONTINUE restart (CLHS 9.1). If that
    restart is invoked, CERROR returns NIL and execution resumes; otherwise it
    does not return.
    """
    from .evaluation_core import eval

    args = cdr(form)
    if not _consp_internal(args) or not _consp_internal(cdr(args)):
        signal_error_object(lisptype.ProgramError(
            message="CERROR requires a continue-format-control and a datum"))

    continue_format = eval(car(args), env)
    condition_form = car(cdr(args))
    # CLHS 9.1: cerror's datum/arguments behave "as if by (apply #'error
    # datum arguments)" -- same dispatch as ERROR, including string datums
    # from a variable building a proper SIMPLE-ERROR.
    remaining_args_form = cdr(cdr(args))

    condition = _build_condition_from_forms(
        condition_form, remaining_args_form, env, lisptype.SimpleError)
    return _signal_cerror_object(condition, continue_format)


_RESTART_CASE_OPTION_KEYWORDS = {'REPORT', 'INTERACTIVE', 'TEST'}


def _parse_restart_case_options(forms):
    """Split a RESTART-CASE clause's trailing forms into its leading
    :report/:interactive/:test option pairs (CLHS 9.1) and the remaining
    forms (declarations, then body). Options are recognized only as a
    prefix, exactly like a DEFSTRUCT slot option list -- the first form that
    is not a `(:report|:interactive|:test value)` pair ends the options,
    even if a later body form happens to start with one of those keywords.
    """
    options = {}
    cur = forms
    while _consp_internal(cur):
        item = car(cur)
        rest = cdr(cur)
        if (isinstance(item, lisptype.lispKeyword) and item.name in _RESTART_CASE_OPTION_KEYWORDS
                and _consp_internal(rest)):
            options[item.name] = car(rest)
            cur = cdr(rest)
            continue
        break
    return options, cur


def _eval_function_designator_option(option_form, env):
    """Evaluate a RESTART-CASE :interactive/:test option (or a non-string
    :report) value: CLHS 9.1 coerces it via FUNCTION, which is what handles
    both a bare function-name symbol (e.g. an FLET-local name, restart-
    case.21) and a lambda-expression (restart-case.18) the same way #' does.
    """
    from .evaluation_core import eval
    function_form = cons(lisptype.LispSymbol('FUNCTION'), cons(option_form, lisptype.NIL))
    return eval(function_form, env)


def _eval_report_option(option_form, env):
    """RESTART-CASE's :report is a `report-generation-argument` (CLHS
    glossary): a literal string is used directly (never coerced to a
    function -- restart-case.20), anything else is a function designator."""
    if isinstance(option_form, (str, lisptype.LispString)):
        return _string_report_function(str(option_form))
    return _eval_function_designator_option(option_form, env)


def _parse_keyword_plist(forms):
    """A plist of `:keyword value` pairs -- RESTART-BIND's per-binding
    options (`:report-function`/`:interactive-function`/`:test-function`),
    whose values are ordinary forms evaluated directly (CLHS 9.1: unlike
    RESTART-CASE's :report/:interactive/:test, these are not coerced via
    FUNCTION -- the caller already writes `#'(lambda ...)` explicitly)."""
    result = {}
    cur = forms
    while _consp_internal(cur) and _consp_internal(cdr(cur)):
        key = car(cur)
        if isinstance(key, lisptype.lispKeyword):
            result[key.name] = car(cdr(cur))
        cur = cdr(cdr(cur))
    return result


def _restart_case_signal_target(protected_form, env):
    """CLHS 9.1: if RESTART-CASE's protected form is, after fully expanding
    any macro or symbol-macro call in `env` (restart-case.29/.30/.31 exercise
    exactly this through MACROLET/SYMBOL-MACROLET), literally a call to
    SIGNAL, ERROR, CERROR or WARN, then RESTART-CASE associates its own
    restarts with the specific condition *that call* signals -- not with
    whatever some other, nested signal happens to raise while a handler
    runs (restart-case.25-.28's whole point). Returns None if the protected
    form is not such a call, else (operator-name, condition, extra), where
    `extra` is CERROR's evaluated continue-format-control, else None.
    """
    from .misc_packages import (macroexpand as _macroexpand,
                                macro_expansion_evaluates)

    expanded = protected_form
    for _ in range(20):
        # This loop asks a question about the protected form's *shape*, so it
        # must not run the program to answer it. The `_reuse_definer` family
        # used to expand by *evaluating* its form -- expanding
        # `(loop repeat 3 do (incf x))` here ran it, and `_run_protected`
        # ran it again, leaving X at 6 (plan.md finding 12). Those macros
        # now expand to a pure deferred form, and this guard remains as the
        # tripwire for any future macro whose expander is not pure: none of
        # those operators is SIGNAL/ERROR/CERROR/WARN, so stopping at one
        # loses nothing this function is looking for.
        if macro_expansion_evaluates(expanded, env):
            break
        if isinstance(expanded, lisptype.LispSymbol):
            expansion = env.get_symbol_macro(expanded)
            if expansion is None:
                break
            expanded = expansion
            continue
        if _consp_internal(expanded):
            # `_macroexpand` is `misc_packages.macroexpand`, the *Lisp-facing*
            # function -- CLHS 3.8 makes it two-valued, so it now returns a
            # `MultipleValues` wrapper even when nothing expanded, never the
            # bare `expanded` object back. Comparing `new is expanded`
            # directly against that wrapper is always false, so this loop
            # used to treat "nothing left to expand" as "expanded once more",
            # replace `expanded` with the wrapper itself, fail the `consp`
            # check next iteration, and return None -- silently disabling the
            # whole restart/condition-association feature this function
            # exists for (CLHS 9.1; `restart-case.26`-`.31`). `primary_value`
            # unwraps back to the same object `macroexpand` wrapped, so the
            # identity check still terminates on the first unchanged pass.
            new = lisptype.primary_value(_macroexpand(expanded, env))
            if new is expanded:
                break
            expanded = new
            continue
        break

    if not _consp_internal(expanded):
        return None
    operator = car(expanded)
    if not isinstance(operator, lisptype.LispSymbol):
        return None

    call_args = cdr(expanded)
    op_name = operator.name
    if op_name == 'SIGNAL':
        if not _consp_internal(call_args):
            return None
        condition = _build_condition_from_forms(
            car(call_args), cdr(call_args), env, lisptype.SimpleCondition)
        return ('SIGNAL', condition, None)
    if op_name == 'ERROR':
        if not _consp_internal(call_args):
            return None
        condition = _build_condition_from_forms(
            car(call_args), cdr(call_args), env, lisptype.SimpleError)
        return ('ERROR', condition, None)
    if op_name == 'WARN':
        if not _consp_internal(call_args):
            return None
        condition = _build_condition_from_forms(
            car(call_args), cdr(call_args), env, lisptype.SimpleWarning)
        return ('WARN', condition, None)
    if op_name == 'CERROR':
        if not (_consp_internal(call_args) and _consp_internal(cdr(call_args))):
            return None
        from .evaluation_core import eval
        continue_format = eval(car(call_args), env)
        condition = _build_condition_from_forms(
            car(cdr(call_args)), cdr(cdr(call_args)), env, lisptype.SimpleError)
        return ('CERROR', condition, continue_format)
    return None


def _dispatch_restart_case_signal(op_name, condition, extra):
    if op_name == 'ERROR':
        return signal_error_object(condition)
    if op_name == 'SIGNAL':
        return signal_condition_object(condition)
    if op_name == 'WARN':
        return _signal_warning_object(condition)
    if op_name == 'CERROR':
        return _signal_cerror_object(condition, extra)
    raise AssertionError(f"unreachable: {op_name}")  # pragma: no cover


def eval_restart_case(form, env):
    """Implement RESTART-CASE special form (CLHS 9.2).

    Syntax: (RESTART-CASE protected-form {(case-name arglist
             [:report r] [:interactive i] [:test t] {form}*)}*)

    Each clause becomes a `Restart` whose `function` performs the clause's
    implicit non-local exit rather than running the clause body directly
    (`_make_case_transfer`); the body itself is compiled once, as an ordinary
    LAMBDA closure over `arglist`, so full CLHS 3.4.1 lambda-list support
    (&optional/&rest/&key/&aux, DECLARE) is LAMBDA's, not a second binder
    duplicating it (CLAUDE.md's standing warning about copy-pasted binders).

    The clause's restarts must be disestablished -- popped off
    `state.restart_stack` -- *before* the clause body runs, not after: CLHS
    9.2's own example (restart-case.12) nests two RESTART-CASE forms under
    the same name and has the inner clause's body re-invoke that name,
    which must resolve to the *outer* restart once the inner one has exited.
    That is why the clause funcall happens after the `try/finally` below has
    already popped, not inside the `except` clause.
    """
    from .evaluation_core import eval, funcall

    args = cdr(form)
    if not _consp_internal(args):
        signal_error_object(lisptype.ProgramError(
            message="RESTART-CASE requires a protected form"))

    protected_form = car(args)
    tag = RestartCaseTag()
    restarts = []
    clause_closures = []

    for clause in _iter_list(cdr(args)):
        if not _consp_internal(clause):
            continue
        name = car(clause)
        rest = cdr(clause)
        arglist = car(rest) if _consp_internal(rest) else lisptype.NIL
        trailing = cdr(rest) if _consp_internal(rest) else lisptype.NIL
        options, body_forms = _parse_restart_case_options(trailing)

        lambda_form = cons(lisptype.LispSymbol('LAMBDA'), cons(arglist, body_forms))
        clause_closures.append(eval(lambda_form, env))
        index = len(clause_closures) - 1

        report_fn = _eval_report_option(options['REPORT'], env) if 'REPORT' in options else None
        interactive_fn = (_eval_function_designator_option(options['INTERACTIVE'], env)
                           if 'INTERACTIVE' in options else None)
        test_fn = (_eval_function_designator_option(options['TEST'], env)
                   if 'TEST' in options else None)
        restart_name = name if isinstance(name, lisptype.LispSymbol) else lisptype.NIL
        restarts.append(lisptype.Restart(
            restart_name, _make_case_transfer(tag, index),
            report_function=report_fn, interactive_function=interactive_fn,
            test_function=test_fn))

    def _run_protected():
        target = _restart_case_signal_target(protected_form, env)
        if target is None:
            return eval(protected_form, env)
        op_name, condition, extra = target
        for r in restarts:
            r.associated_conditions.append(condition)
        try:
            return _dispatch_restart_case_signal(op_name, condition, extra)
        finally:
            for r in restarts:
                r.associated_conditions.remove(condition)

    state.restart_stack.append(restarts)
    outcome = lisptype.NIL
    pending_transfer = None
    try:
        try:
            outcome = _run_protected()
        except RestartCaseTransfer as exc:
            if exc.tag is not tag:
                raise
            pending_transfer = exc
    finally:
        state.restart_stack.pop()

    if pending_transfer is not None:
        return funcall(clause_closures[pending_transfer.clause_index], *pending_transfer.args)
    return outcome


def eval_restart_bind(form, env):
    """Implement RESTART-BIND special form (CLHS 9.2).

    Syntax: (RESTART-BIND ((name function-form
                            [:report-function r] [:interactive-function i]
                            [:test-function t]) ...) {form}*)

    Unlike RESTART-CASE, invoking one of these restarts does not itself
    unwind: `function-form` is evaluated once, at binding time, to produce
    the function INVOKE-RESTART funcalls directly, in the dynamic
    environment of the invocation -- any non-local exit is the function
    body's own doing (RETURN-FROM/GO/THROW), which is exactly why it must be
    invoked through `evaluation_core.funcall` (RESTART-BIND.ERROR.1-3: a
    wrong argument count is a PROGRAM-ERROR, the same conversion funcall
    already does for any other misapplied function).
    """
    from .evaluation_core import eval

    args = cdr(form)
    if not _consp_internal(args):
        signal_error_object(lisptype.ProgramError(
            message="RESTART-BIND requires a binding list"))

    binding_clauses = car(args)
    body_forms = cdr(args)

    restarts = []
    for binding in _iter_list(binding_clauses):
        if not _consp_internal(binding):
            continue
        name = car(binding)
        rest = cdr(binding)
        if not _consp_internal(rest):
            continue
        function = eval(car(rest), env)
        options = _parse_keyword_plist(cdr(rest))
        report_fn = eval(options['REPORT-FUNCTION'], env) if 'REPORT-FUNCTION' in options else None
        interactive_fn = (eval(options['INTERACTIVE-FUNCTION'], env)
                           if 'INTERACTIVE-FUNCTION' in options else None)
        test_fn = eval(options['TEST-FUNCTION'], env) if 'TEST-FUNCTION' in options else None
        restart_name = name if isinstance(name, lisptype.LispSymbol) else lisptype.NIL
        restarts.append(lisptype.Restart(
            restart_name, function, report_function=report_fn,
            interactive_function=interactive_fn, test_function=test_fn))

    state.restart_stack.append(restarts)
    try:
        result = lisptype.NIL
        for f in _iter_list(body_forms):
            result = eval(f, env)
        return result
    finally:
        state.restart_stack.pop()


def compute_restarts_list(condition=None):
    """CLHS 9.1 COMPUTE-RESTARTS: every currently active restart applicable
    to `condition` (all of them, if `condition` is NIL/omitted), most
    recently established frame first, clause order preserved within a
    frame -- the same search order INVOKE-RESTART's by-name lookup uses, so
    `(find 'foo (compute-restarts) :key #'restart-name)` and
    `(invoke-restart 'foo)` agree about which restart named foo is "the"
    one (compute-restarts.3-.6)."""
    result = []
    for frame in reversed(state.restart_stack):
        for restart in frame:
            if restart.applies_to(condition):
                result.append(restart)
    return result


def find_restart_obj(identifier, condition=None):
    """CLHS 9.1 FIND-RESTART. `identifier` is a restart name designator
    (symbol/string) or a restart object itself, in which case it is returned
    only if it is still active and still applicable to `condition`."""
    if isinstance(identifier, lisptype.Restart):
        for frame in reversed(state.restart_stack):
            if identifier in frame:
                return identifier if identifier.applies_to(condition) else None
        return None
    for frame in reversed(state.restart_stack):
        for restart in frame:
            if restart.name_matches(identifier) and restart.applies_to(condition):
                return restart
    return None


def invoke_restart_obj(restart, args):
    """CLHS 9.1 INVOKE-RESTART given an already-resolved restart object: call
    its function the same way any other Lisp function call is made, so a
    wrong argument count converts to PROGRAM-ERROR rather than a bare Python
    TypeError (plan.md's "Python exceptions must not appear as Lisp
    values")."""
    from .evaluation_core import funcall
    return funcall(restart.function, *args)


def _invoke_named_restart(name, condition, args, error_if_missing):
    """The shared shape of ABORT/CONTINUE/MUFFLE-WARNING/USE-VALUE/
    STORE-VALUE (CLHS 9.1): find the restart named `name` applicable to
    `condition`, invoke it if found, else either signal CONTROL-ERROR
    (ABORT, MUFFLE-WARNING) or simply return NIL (CONTINUE, USE-VALUE,
    STORE-VALUE) -- one function parameterized by that single difference,
    not five near-identical copies."""
    restart = find_restart_obj(lisptype.LispSymbol(name), condition)
    if restart is None:
        if error_if_missing:
            return signal_error_object(lisptype.ControlError(
                message=f"No {name} restart is currently active."))
        return lisptype.NIL
    return invoke_restart_obj(restart, args)


def eval_invoke_restart(form, env):
    """Implement INVOKE-RESTART special form.

    Syntax: (INVOKE-RESTART restart-designator &rest arguments)

    Every argument position is evaluated normally -- CLHS gives INVOKE-RESTART
    no unevaluated syntax at all; it is a special form here only so the
    function-designator entry point (`utilities_errors.invoke_restart`, used
    by FUNCALL/APPLY/#'INVOKE-RESTART) and this direct-call path can share
    `find_restart_obj`/`invoke_restart_obj` as the one resolution+invocation
    mechanism.
    """
    from .evaluation_core import eval

    args = cdr(form)
    if not _consp_internal(args):
        signal_error_object(lisptype.ProgramError(
            message="INVOKE-RESTART requires a restart designator"))

    designator = eval(car(args), env)
    call_args = [eval(a, env) for a in _iter_list(cdr(args))]

    # One resolution for every designator kind (CLHS 9.1): a name finds the
    # innermost active restart so named, and a restart *object* is used only
    # if it is still active -- `find_restart_obj` answers both. Skipping the
    # liveness check for an object used to invoke its transfer closure
    # anyway, whose RestartCaseTransfer then reached a frame that had already
    # exited and surfaced as "Uncaught THROW #<restart-case-tag ...>"
    # (CLHS: a designator naming no active restart signals CONTROL-ERROR).
    restart = find_restart_obj(designator)
    if restart is None:
        if isinstance(designator, lisptype.Restart):
            name = (designator.name.name
                    if isinstance(designator.name, lisptype.LispSymbol)
                    else str(designator))
        elif isinstance(designator, lisptype.LispSymbol):
            name = designator.name
        else:
            name = str(designator)
        return signal_error_object(lisptype.ControlError(
            message=f"No restart named {name} is currently active."))
    return invoke_restart_obj(restart, call_args)


def eval_abort(form, env):
    """Implement ABORT special form.

    Syntax: (ABORT &optional condition)

    Invokes the ABORT restart applicable to `condition`; signals CONTROL-ERROR
    if none is active (CLHS 9.1).
    """
    from .evaluation_core import eval

    args = cdr(form)
    condition = eval(car(args), env) if _consp_internal(args) else None
    return _invoke_named_restart('ABORT', condition, (), error_if_missing=True)


def eval_with_condition_restarts(form, env):
    """Implement WITH-CONDITION-RESTARTS special form (CLHS 9.1).

    Syntax: (WITH-CONDITION-RESTARTS condition-form restarts-form {form}*)

    Temporarily associates the value of `condition-form` with each restart in
    the (already-established) list `restarts-form` evaluates to, for the
    dynamic extent of the body -- restricting COMPUTE-RESTARTS/FIND-RESTART
    queries naming any *other* condition from seeing them
    (`Restart.applies_to`). This is the general form of the association
    RESTART-CASE performs automatically for a literal SIGNAL/ERROR/CERROR/
    WARN protected form (`_restart_case_signal_target`) -- one mechanism,
    used two ways, rather than RESTART-CASE reimplementing it privately.

    A special form rather than a macro because the body forms must run
    *while* the association is in effect, which a `cl_function` (whose
    arguments are all evaluated before the call) cannot arrange.
    """
    from .evaluation_core import eval

    args = cdr(form)
    if not (_consp_internal(args) and _consp_internal(cdr(args))):
        signal_error_object(lisptype.ProgramError(
            message="WITH-CONDITION-RESTARTS requires a condition form and a restarts form"))

    condition = eval(car(args), env)
    restarts_value = eval(car(cdr(args)), env)
    body = cdr(cdr(args))

    restart_list = [r for r in _iter_list(restarts_value) if isinstance(r, lisptype.Restart)]
    real_condition = None if condition in (None, lisptype.NIL) else condition

    if real_condition is not None:
        for r in restart_list:
            r.associated_conditions.append(real_condition)
    try:
        result = lisptype.NIL
        for f in _iter_list(body):
            result = eval(f, env)
        return result
    finally:
        if real_condition is not None:
            for r in restart_list:
                r.associated_conditions.remove(real_condition)


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

    CLHS 5.1.3: a place's subforms are evaluated left to right before
    the value-form -- which here means each var's symbol-macro
    expansion (the "place" part) is materialised before value-form is
    evaluated, so `(multiple-value-setq (y) i)` inside
    `SYMBOL-MACROLET ((y (car (progn (incf i) x))))` increments i
    before reading it (`multiple-value-setq.5`/`m-v-s.order.1`).
    """
    from .evaluation_core import eval

    args = cdr(form)
    if not _consp_internal(args) or not _consp_internal(cdr(args)):
        raise lisptype.LispNotImplementedError("MULTIPLE-VALUE-SETQ requires vars and a value-form")

    vars = car(args)
    value_form = car(cdr(args))

    var_list = []
    current = vars
    while _consp_internal(current):
        var_list.append(car(current))
        current = cdr(current)

    # Materialise each var's symbol-macro expansion in order, left to
    # right, *before* evaluating value-form. Plain (non-symbol-macro)
    # vars just record themselves here; their assignment happens below
    # once the value is in hand.
    resolved_places = []
    for var in var_list:
        expansion = env.get_symbol_macro(var)
        if expansion is None:
            resolved_places.append(('var', var))
        elif isinstance(expansion, lisptype.LispSymbol):
            place = expansion
            while isinstance(place, lisptype.LispSymbol):
                deeper = env.get_symbol_macro(place)
                if deeper is None:
                    break
                place = deeper
            if isinstance(place, lisptype.LispSymbol):
                resolved_places.append(('var', place))
            else:
                try:
                    from .evaluation_special_forms import _place_accessor
                    _g, _s = _place_accessor(place, env)
                    resolved_places.append(('accessor', _s))
                except lisptype.LispError:
                    resolved_places.append(('noop', var))
        elif _consp_internal(expansion) and isinstance(car(expansion), lisptype.LispSymbol):
            op_name = car(expansion).name
            place_args = cdr(expansion)
            if op_name in ('CAR', 'CDR') and _consp_internal(place_args):
                target = eval(car(place_args), env)
                if _consp_internal(target):
                    resolved_places.append(
                        ('cell', target, op_name == 'CAR'))
                else:
                    resolved_places.append(('noop', var))
            else:
                try:
                    from .evaluation_special_forms import _place_accessor
                    _g, _s = _place_accessor(expansion, env)
                    resolved_places.append(('accessor', _s))
                except lisptype.LispError:
                    resolved_places.append(('noop', var))
        else:
            resolved_places.append(('noop', var))

    values = eval(value_form, env)

    if isinstance(values, lisptype.MultipleValues):
        value_tuple = values.get_all()
    else:
        primary = values if values is not None else lisptype.NIL
        value_tuple = ([primary]
                       + [lisptype.NIL] * (len(var_list) - 1)
                       if var_list else [primary])

    for i, place in enumerate(resolved_places):
        result = value_tuple[i] if i < len(value_tuple) else lisptype.NIL
        kind = place[0]
        if kind == 'cell':
            target = place[1]
            is_car = place[2]
            if is_car:
                target.car = result
            else:
                target.cdr = result
        elif kind == 'var':
            env.set_variable(place[1], result)
        elif kind == 'accessor':
            place[1](result)
        # 'noop' (an unrecognised place form) is silently dropped,
        # matching the legacy behaviour -- the test that pins this
        # order (m-v-s.5) only exercises the CAR/CDR expansion path.

    return value_tuple[0] if value_tuple else lisptype.NIL


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

    The clause's variable binding is an implicit LET over the body, so a
    body-level ``(declare (special var))`` must make it bind the symbol's
    value cell for the clause's extent -- handler-case.11 binds the condition
    to a special ``*C*`` exactly this way and then reads it from a helper
    called outside the binding form. The binding therefore goes through
    `BindingFrame` with the clause body in hand, the same question every
    other binding form asks, rather than a bare lexical `add_variable`.
    """
    from .evaluation_core import eval
    from .binding import BindingFrame

    var_list = car(cdr(clause))
    clause_body = cdr(cdr(clause))

    new_env = lisptype.Environment(parent=env)
    if _consp_internal(var_list):
        var = car(var_list)
        frame = BindingFrame(new_env, body=clause_body, bound_vars=[var],
                             defer_free_declarations=True)
        with frame:
            frame.bind(var, condition)
            # No init forms are evaluated in this environment, so the free
            # declarations (if any) can be installed immediately after the
            # parameters are bound, mirroring make_ordinary_function's order.
            frame.install_free_declarations()
            result = lisptype.NIL
            body = clause_body
            while _consp_internal(body):
                result = eval(car(body), new_env)
                body = cdr(body)
            return result

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

    A `(:no-error lambda-list body*)` clause is the success path (CLHS 9.5.2):
    when the expression returns normally, the clause is called as an ordinary
    function of the expression's *values*. See `_run_no_error_clause`.

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
    no_error_clause = None
    current = clauses
    while _consp_internal(current):
        clause = car(current)
        if _consp_internal(clause):
            head = car(clause)
            if lisptype.is_keyword(head) and head.name == 'NO-ERROR':
                # Not a handler: the success-path clause. It must not appear
                # in `clause_list` -- the THROW/ConditionException backstops
                # below match against that list, and a :no-error clause is
                # not a handler for anything (CLHS 9.5.2).
                no_error_clause = clause
            else:
                clause_list.append(clause)

                def make_handler(the_clause):
                    def transfer(condition):
                        raise HandlerCaseTransfer(tag, the_clause, condition)
                    return transfer

                handlers.append((head, make_handler(clause)))
        current = cdr(current)

    try:
        with _HandlerCluster(handlers):
            result = eval(expression, env)
    except HandlerCaseTransfer as transfer:
        if transfer.tag is not tag:
            # Belongs to an enclosing HANDLER-CASE; let it through.
            raise
        return _run_handler_case_clause(transfer.clause, transfer.condition, env)
    except RestartCaseTransfer:
        # A restart invocation in transit to its own establishing form (an
        # enclosing RESTART-CASE, or CERROR/WARN's built-in CONTINUE/
        # MUFFLE-WARNING restart) -- not a Lisp-level THROW at all, so it
        # must never be mistaken for one below. Without this, any
        # RESTART-CASE/CERROR/WARN whose restart is invoked from inside a
        # HANDLER-CASE clause's protected form (handler-case.20-.26/.29) had
        # its transfer intercepted here and misreported as an uncaught THROW.
        raise
    except ThrowException:
        # A ThrowException reaching here always has a live CATCH further
        # out: EVAL-THROW consults `state.catch_tags` *before* raising and
        # signals the CONTROL-ERROR itself for a tag with no catcher -- the
        # approximation this clause used to make ("needs a catch-tag stack
        # to know at throw time", M7) is obsolete, and acting on it broke
        # the pass-through of a THROW whose catcher encloses this
        # HANDLER-CASE (the unit suite's non-local-exit contract, and CLHS
        # 5.2's throw semantics). Let the transfer continue outward.
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
    else:
        # Success: the handlers of THIS form are disestablished (the `with`
        # has exited) before the :no-error clause runs, so an error the
        # :no-error body signals escapes to enclosing handlers instead of
        # being caught by this form's own clauses -- handler-case.25.
        if no_error_clause is not None:
            return _run_no_error_clause(no_error_clause, result, env)
        return result


def _run_no_error_clause(clause, values, env):
    """Evaluate HANDLER-CASE's :no-error clause (CLHS 9.5.2).

    The clause is an ordinary function of the protected form's *values*: it
    is built by `make_ordinary_function` -- the one ordinary-lambda-list
    constructor -- so a wrong value count is a PROGRAM-ERROR exactly as it is
    for any call (handler-case.23/.24), and &aux parameters and free special
    declarations behave exactly as they do in any lambda (handler-case.29).
    Calling through `funcall` is what turns the binder's arity
    `LispProgramError` into the PROGRAM-ERROR condition the test observes.
    """
    from .evaluation_core import funcall
    from .evaluation_special_forms import make_ordinary_function

    var_list = car(cdr(clause))
    body = cdr(cdr(clause))
    fn = make_ordinary_function(var_list, body, env)
    if isinstance(values, lisptype.MultipleValues):
        args = list(values.values)
    else:
        args = [values]
    return funcall(fn, *args)


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
