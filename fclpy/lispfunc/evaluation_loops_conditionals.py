"""Loops and conditionals: WHEN, COND, AND, OR, PROGN, LET, quasiquote."""

import fclpy.lisptype as lisptype
from .core import car, cdr, _consp_internal, cons
from . import registry as _registry
import time
import sys

# Timeout for loop warning (in seconds) - set to 0 to disable
LOOP_TIMEOUT_WARNING = 120  # 2 minutes

# Hard cap for any loop that fails to terminate. The obvious case is LOOP's
# simple-loop path (CLHS 6.1.1), which has no driver at all: an unrecognized or
# misparsed clause whose tokens land in body_forms instead of being consumed has
# no way to ever terminate.
#
# It is NOT the only case, and the comment that used to stand here -- claiming
# every other construct "is naturally bounded by its own driver" -- was false.
# `for var = form` is an unbounded driver by definition (CLHS 6.1.2.1.3): it is
# bounded only by a *separate* clause, so any defect that drops that clause
# produces a driver-path runaway. `repeat 5 for x = 7` was exactly that, and it
# hung with no bound to stop it. The cap therefore applies to every path.
#
# Set well above LOOP_TIMEOUT_WARNING so it never fires on a legitimately slow
# ANSI test, only on a genuine runaway.
LOOP_TIMEOUT_ERROR = 600  # 10 minutes

# LOOP's accumulation clauses (CLHS 6.1.3) mapped to the accumulator each one
# feeds. ALWAYS/THEREIS are termination-test clauses rather than accumulations,
# but they are parsed and executed through the same slot, so they live here too.
# One table instead of one parse branch per keyword: the branches differed only
# in this string, which is how INTO came to be handled in none of them.
ACCUMULATION_CLAUSES = {
    'COLLECT': 'collect', 'COLLECTING': 'collect',
    'APPEND': 'append', 'APPENDING': 'append',
    'NCONC': 'nconc', 'NCONCING': 'nconc',
    'SUM': 'sum', 'SUMMING': 'sum',
    'COUNT': 'count', 'COUNTING': 'count',
    'ALWAYS': 'always',
    'THEREIS': 'thereis',
}

# Every token that begins a new LOOP clause, and therefore ends the one being
# parsed. Single source of truth: this used to be two hand-maintained copies
# (the FOR-clause scanner and the DO body scanner) that could drift apart.
LOOP_CLAUSE_KEYWORDS = frozenset(ACCUMULATION_CLAUSES) | {
    'FOR', 'AS', 'WHILE', 'UNTIL', 'REPEAT', 'DO', 'DOING',
    'WHEN', 'UNLESS', 'IF', 'RETURN', 'INITIALLY', 'FINALLY',
}


class LoopWatchdog:
    """Reports a loop that runs unreasonably long -- and, crucially, reports how
    it ended.

    The warning alone is not a usable signal. It fires once per loop and nothing
    is ever printed again, so three very different outcomes look byte-identical
    in `run_all_tests.err`:

      * the loop was merely slow and finished normally;
      * the loop is still spinning right now;
      * the loop hit the hard cap and was aborted -- which raises a LispError
        that surfaces in the *.log* as an ordinary test failure, never in .err.

    Reading a bare "LOOP WARNING ... 120.1s" therefore cannot tell you whether a
    run is stuck, which is exactly the question it exists to answer. This class
    emits a matching RESOLVED/ABORTED line whenever a loop that warned finally
    ends, by any path including a non-local exit, and stamps every line with the
    wall clock so it can be placed against the rest of the run.

    It also replaces three separately maintained copies of this logic (LOOP, DO,
    DO*) that had already drifted apart: only LOOP's had the hard cap at all.
    `describe` is a callable so the diagnostic detail is only built if a warning
    actually fires.
    """

    def __init__(self, kind, describe, hard_cap=0):
        self.kind = kind
        self.describe = describe
        self.hard_cap = hard_cap
        # perf_counter, not time(): durations need a monotonic clock (a system
        # clock adjustment mid-run must not make a loop look 30 minutes old), and
        # on Windows time() has ~16ms granularity, coarse enough that a tight
        # loop can measure a 0.0s elapsed. The wall clock is only used for the
        # human-readable stamp.
        self.start_time = time.perf_counter()
        self.iterations = 0
        self.warned = False

    def _stamp(self):
        return time.strftime('%H:%M:%S', time.localtime())

    def tick(self):
        """Count one iteration; warn once if slow, abort if past the hard cap."""
        self.iterations += 1
        elapsed = time.perf_counter() - self.start_time

        if LOOP_TIMEOUT_WARNING > 0 and not self.warned and elapsed > LOOP_TIMEOUT_WARNING:
            self.warned = True
            print(f"\n*** {self.kind} WARNING [{self._stamp()}]: running for "
                  f"{elapsed:.1f}s ({self.iterations} iterations) ***", file=sys.stderr)
            for line in self.describe():
                print(f"*** {self.kind}: {line} ***", file=sys.stderr)
            sys.stderr.flush()

        if self.hard_cap > 0 and elapsed > self.hard_cap:
            # Announce on the same stream as the warning before raising: the
            # LispError itself lands in the .log as a test failure, so .err
            # would otherwise show a warning with no outcome.
            print(f"*** {self.kind} ABORTED [{self._stamp()}]: exceeded {self.hard_cap}s "
                  f"({self.iterations} iterations) ***", file=sys.stderr)
            sys.stderr.flush()
            raise lisptype.LispError(
                f"{self.kind} exceeded {self.hard_cap}s ({self.iterations} iterations) "
                f"without terminating -- aborting instead of hanging. "
                f"{'; '.join(self.describe())}")

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_value, traceback):
        # Only report an outcome for loops that warned; a normal fast loop must
        # stay silent. Reported on every exit path, including a non-local exit
        # (RETURN-FROM/THROW/GO), which is how most Lisp loops actually end.
        if self.warned:
            elapsed = time.perf_counter() - self.start_time
            how = 'RESOLVED' if exc_type is None else f'EXITED via {exc_type.__name__}'
            print(f"*** {self.kind} {how} [{self._stamp()}]: after {elapsed:.1f}s "
                  f"({self.iterations} iterations) ***", file=sys.stderr)
            sys.stderr.flush()
        return False


def eval_when(form, env):
    """Evaluate WHEN special form."""
    from .evaluation_core import eval
    
    args = cdr(form)
    if not _consp_internal(args):
        return lisptype.NIL
    
    test_form = car(args)
    body = cdr(args)
    
    test_result = eval(test_form, env)
    if lisptype.is_truthy(test_result):
        result = lisptype.NIL
        current_body = body
        while _consp_internal(current_body):
            result = eval(car(current_body), env)
            current_body = cdr(current_body)
        return lisptype.NIL if result is None else result

    return lisptype.NIL


def eval_unless(form, env):
    """Evaluate UNLESS special form."""
    from .evaluation_core import eval
    
    args = cdr(form)
    if not _consp_internal(args):
        return lisptype.NIL
    
    test_form = car(args)
    body = cdr(args)
    
    test_result = eval(test_form, env)
    if not lisptype.is_truthy(test_result):
        result = lisptype.NIL
        current_body = body
        while _consp_internal(current_body):
            result = eval(car(current_body), env)
            current_body = cdr(current_body)
        return lisptype.NIL if result is None else result

    return lisptype.NIL


def eval_eval_when(form, env):
    """Evaluate EVAL-WHEN special form.
    
    (EVAL-WHEN (situation*) form*)
    
    Situations can be:
    - :COMPILE-TOPLEVEL (or COMPILE) - evaluate at compile time
    - :LOAD-TOPLEVEL (or LOAD) - evaluate at load time  
    - :EXECUTE (or EVAL) - evaluate at execution time
    
    For an interpreter, we evaluate if :EXECUTE or :LOAD-TOPLEVEL is present.
    """
    from .evaluation_core import eval
    
    args = cdr(form)
    if not _consp_internal(args):
        return lisptype.NIL
    
    situations = car(args)
    body = cdr(args)
    
    # Check if any relevant situation applies
    # For an interpreter at runtime, :EXECUTE and :LOAD-TOPLEVEL apply
    should_execute = False
    current = situations
    while _consp_internal(current):
        sit = car(current)
        if isinstance(sit, lisptype.lispKeyword):
            sit_name = sit.name.upper()
        elif isinstance(sit, lisptype.LispSymbol):
            sit_name = sit.name.upper()
        else:
            sit_name = str(sit).upper()
        
        # :EXECUTE means "at runtime" - always applies for interpreter
        # :LOAD-TOPLEVEL means "when loading" - applies when loading files
        if sit_name in ('EXECUTE', ':EXECUTE', 'EVAL', 'LOAD-TOPLEVEL', ':LOAD-TOPLEVEL', 'LOAD'):
            should_execute = True
            break
        current = cdr(current)
    
    if should_execute:
        # Evaluate body forms, return value of last
        result = lisptype.NIL
        current = body
        while _consp_internal(current):
            result = eval(car(current), env)
            current = cdr(current)
        return result
    else:
        # Don't evaluate, return NIL
        return lisptype.NIL


def eval_cond(form, env):
    """Evaluate COND special form."""
    from .evaluation_core import eval
    
    clauses = cdr(form)
    
    while _consp_internal(clauses):
        clause = car(clauses)
        if _consp_internal(clause):
            test = car(clause)
            
            # Special case for T
            if (isinstance(test, lisptype.LispSymbol) and test.name == 'T') or eval(test, env):
                # Execute forms in clause
                result = test if not _consp_internal(cdr(clause)) else None
                forms = cdr(clause)
                while _consp_internal(forms):
                    result = eval(car(forms), env)
                    forms = cdr(forms)
                return result
        
        clauses = cdr(clauses)
    
    return None


def _primary_value(value):
    """Coerce a possibly-multiple-valued result to its single primary value.

    Used wherever ANSI evaluates a form in a single-value context (e.g. a
    CASE-family keyform): if it returned a MultipleValues, only the first
    value is used (NIL if it returned zero values); anything else passes
    through unchanged.
    """
    if isinstance(value, lisptype.MultipleValues):
        values = value.get_all()
        return values[0] if values else lisptype.NIL
    return value


def eval_case(form, env):
    """Evaluate CASE special form.
    
    Syntax: (CASE keyform {normal-clause}* [otherwise-clause])
    normal-clause ::= (keys form*)
    otherwise-clause ::= ({otherwise | t} form*)
    keys ::= key | (key*)
    
    Evaluates keyform, then compares with EQL to keys in each clause.
    Returns NIL if no clause matches.
    """
    from .evaluation_core import eval
    from .comparison import eql
    
    args = cdr(form)
    if not _consp_internal(args):
        return lisptype.NIL
    
    # Evaluate keyform
    keyform = car(args)
    key_value = _primary_value(eval(keyform, env))
    
    clauses = cdr(args)
    
    while _consp_internal(clauses):
        clause = car(clauses)
        if _consp_internal(clause):
            keys = car(clause)
            forms = cdr(clause)
            
            # Check for OTHERWISE or T (final catch-all clause)
            if isinstance(keys, lisptype.LispSymbol):
                if keys.name in ('OTHERWISE', 'T'):
                    # Execute forms and return
                    result = lisptype.NIL
                    while _consp_internal(forms):
                        result = eval(car(forms), env)
                        forms = cdr(forms)
                    return result
                else:
                    # Single key - compare with EQL
                    if eql(key_value, keys):
                        result = lisptype.NIL
                        while _consp_internal(forms):
                            result = eval(car(forms), env)
                            forms = cdr(forms)
                        return result
            elif _consp_internal(keys):
                # List of keys - check each with EQL
                current_key = keys
                while _consp_internal(current_key):
                    k = car(current_key)
                    if eql(key_value, k):
                        result = lisptype.NIL
                        while _consp_internal(forms):
                            result = eval(car(forms), env)
                            forms = cdr(forms)
                        return result
                    current_key = cdr(current_key)
            else:
                # Atomic key (number, character, etc.) - compare with EQL
                if eql(key_value, keys):
                    result = lisptype.NIL
                    while _consp_internal(forms):
                        result = eval(car(forms), env)
                        forms = cdr(forms)
                    return result
        
        clauses = cdr(clauses)

    return lisptype.NIL


def eval_ccase(form, env):
    """Evaluate CCASE special form.

    Syntax: (CCASE place {normal-clause}*)
    normal-clause ::= (keys form*)
    keys ::= key | (key*) | ()

    Like CASE, but T and OTHERWISE are ordinary keys here (no catch-all
    clause), and a bare NIL in the keys position designates an empty list of
    keys (never matches) rather than a singleton key of NIL -- to match on a
    literal NIL key, write it as a one-element list: ((nil) form*).

    `place` is evaluated exactly once. If no clause matches, CCASE signals a
    correctable TYPE-ERROR (datum = place's value, expected-type = a MEMBER
    type over every key across all clauses).

    Note: full ANSI CCASE also lets a STORE-VALUE restart supply a new value,
    store it back into `place`, and retry the match. That restart-based retry
    protocol is not implemented here -- a non-matching key simply signals the
    TYPE-ERROR once.
    """
    from .evaluation_core import eval, ConditionException
    from .comparison import eql

    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispProgramError("CCASE requires a place and at least one clause")

    place_form = car(args)
    key_value = _primary_value(eval(place_form, env))

    clauses = cdr(args)

    def clause_keys(keys):
        """Return the list of literal keys designated by a clause's keys form."""
        if _consp_internal(keys):
            items = []
            current_key = keys
            while _consp_internal(current_key):
                items.append(car(current_key))
                current_key = cdr(current_key)
            return items
        elif keys is lisptype.NIL or keys == lisptype.NIL:
            # Bare NIL means an empty list of keys, not a singleton NIL key.
            return []
        else:
            return [keys]

    all_keys = []
    parsed_clauses = []
    current = clauses
    while _consp_internal(current):
        clause = car(current)
        if _consp_internal(clause):
            keys = clause_keys(car(clause))
            all_keys.extend(keys)
            parsed_clauses.append((keys, cdr(clause)))
        current = cdr(current)

    for keys, forms in parsed_clauses:
        for key in keys:
            if eql(key_value, key):
                result = lisptype.NIL
                current_form = forms
                while _consp_internal(current_form):
                    result = eval(car(current_form), env)
                    current_form = cdr(current_form)
                return result

    # No clause matched: signal a correctable type-error over the union of keys.
    member_type = lisptype.NIL
    for key in reversed(all_keys):
        member_type = cons(key, member_type)
    member_type = cons(lisptype.LispSymbol('MEMBER'), member_type)

    condition = lisptype.TypeError(datum=key_value, expected_type=member_type)
    raise ConditionException(condition, recoverable=True)


def _case_clause_keys(keys):
    """Return the list of literal keys designated by a CASE-family clause's keys form."""
    if _consp_internal(keys):
        items = []
        current = keys
        while _consp_internal(current):
            items.append(car(current))
            current = cdr(current)
        return items
    elif keys is lisptype.NIL or keys == lisptype.NIL:
        return []
    else:
        return [keys]


def eval_ecase(form, env):
    """Evaluate ECASE special form.

    Syntax: (ECASE keyform {normal-clause}*)
    normal-clause ::= (keys form*)

    Like CASE, but T and OTHERWISE are ordinary keys here (no catch-all
    clause). keyform is evaluated exactly once. If no clause matches,
    ECASE signals a TYPE-ERROR (datum = keyform's value, expected-type = a
    MEMBER type over every key across all clauses).
    """
    from .evaluation_core import eval, ConditionException
    from .comparison import eql

    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispProgramError("ECASE requires a keyform and at least one clause")

    key_value = _primary_value(eval(car(args), env))
    clauses = cdr(args)

    all_keys = []
    current = clauses
    while _consp_internal(current):
        clause = car(current)
        if _consp_internal(clause):
            keys = _case_clause_keys(car(clause))
            all_keys.extend(keys)
            for k in keys:
                if eql(key_value, k):
                    result = lisptype.NIL
                    forms = cdr(clause)
                    while _consp_internal(forms):
                        result = eval(car(forms), env)
                        forms = cdr(forms)
                    return result
        current = cdr(current)

    member_type = lisptype.NIL
    for key in reversed(all_keys):
        member_type = cons(key, member_type)
    member_type = cons(lisptype.LispSymbol('MEMBER'), member_type)

    condition = lisptype.TypeError(datum=key_value, expected_type=member_type)
    raise ConditionException(condition, recoverable=False)


def _typecase_dispatch(form, env, exhaustive, correctable, form_name):
    """Shared implementation for TYPECASE/ETYPECASE/CTYPECASE.

    Syntax: (name keyform {normal-clause}*)
    normal-clause ::= (type form*)

    keyform is evaluated exactly once; each clause's type is matched with
    TYPEP (not evaluated). TYPECASE treats a final T/OTHERWISE clause as a
    catch-all; ETYPECASE/CTYPECASE do not (every key is an ordinary type).
    """
    from .evaluation_core import eval, ConditionException
    from .comparison import typep

    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispProgramError(f"{form_name} requires a keyform and at least one clause")

    key_value = _primary_value(eval(car(args), env))
    clauses = cdr(args)

    all_types = []
    current = clauses
    while _consp_internal(current):
        clause = car(current)
        if _consp_internal(clause):
            type_spec = car(clause)
            forms = cdr(clause)

            if (not exhaustive and isinstance(type_spec, lisptype.LispSymbol)
                    and type_spec.name in ('OTHERWISE', 'T')):
                result = lisptype.NIL
                while _consp_internal(forms):
                    result = eval(car(forms), env)
                    forms = cdr(forms)
                return result

            all_types.append(type_spec)
            if lisptype.is_truthy(typep(key_value, type_spec)):
                result = lisptype.NIL
                while _consp_internal(forms):
                    result = eval(car(forms), env)
                    forms = cdr(forms)
                return result
        current = cdr(current)

    expected_type = lisptype.NIL
    for t in reversed(all_types):
        expected_type = cons(t, expected_type)
    expected_type = cons(lisptype.LispSymbol('OR'), expected_type)

    condition = lisptype.TypeError(datum=key_value, expected_type=expected_type)
    raise ConditionException(condition, recoverable=correctable)


def eval_typecase(form, env):
    """Evaluate TYPECASE special form (has a T/OTHERWISE catch-all, returns NIL if none match and no catch-all)."""
    from .evaluation_core import eval

    args = cdr(form)
    if not _consp_internal(args):
        return lisptype.NIL

    key_value = _primary_value(eval(car(args), env))
    from .comparison import typep

    clauses = cdr(args)
    current = clauses
    while _consp_internal(current):
        clause = car(current)
        if _consp_internal(clause):
            type_spec = car(clause)
            forms = cdr(clause)
            is_catchall = (isinstance(type_spec, lisptype.LispSymbol)
                           and type_spec.name in ('OTHERWISE', 'T'))
            if is_catchall or lisptype.is_truthy(typep(key_value, type_spec)):
                result = lisptype.NIL
                while _consp_internal(forms):
                    result = eval(car(forms), env)
                    forms = cdr(forms)
                return result
        current = cdr(current)

    return lisptype.NIL


def eval_etypecase(form, env):
    """Evaluate ETYPECASE special form (exhaustive: signals TYPE-ERROR if nothing matches)."""
    return _typecase_dispatch(form, env, exhaustive=True, correctable=False, form_name="ETYPECASE")


def eval_ctypecase(form, env):
    """Evaluate CTYPECASE special form (like ETYPECASE but the signaled error is correctable)."""
    return _typecase_dispatch(form, env, exhaustive=True, correctable=True, form_name="CTYPECASE")


def eval_and(form, env):
    """Evaluate AND special form."""
    from .evaluation_core import eval
    
    args = cdr(form)
    result = lisptype.T  # AND with no arguments is T
    
    while _consp_internal(args):
        result = eval(car(args), env)
        if not lisptype.is_truthy(result):
            return lisptype.NIL
        args = cdr(args)
    
    return lisptype.NIL if result is None else result


def eval_or(form, env):
    """Evaluate OR special form."""
    from .evaluation_core import eval
    
    args = cdr(form)
    
    while _consp_internal(args):
        result = eval(car(args), env)
        if result is not None and result != lisptype.NIL:
            return result
        args = cdr(args)
    
    # OR with no truthy values returns NIL, not Python None
    return lisptype.NIL


def eval_progn(form, env):
    """Evaluate PROGN special form.

    CLHS: "If no forms are supplied, (progn) returns nil." The initial value is
    `lisptype.NIL`, not Python `None` -- those are distinct objects here (plan.md
    Finding G), and returning `None` leaked a Python value out as the value of a
    Lisp form.
    """
    from .evaluation_core import eval

    args = cdr(form)
    result = lisptype.NIL

    while _consp_internal(args):
        result = eval(car(args), env)
        args = cdr(args)

    return result


def eval_locally(form, env):
    """Evaluate LOCALLY special form.

    (LOCALLY declaration* form*)

    LOCALLY is like PROGN but can include declarations at the start. Most
    declarations (TYPE, OPTIMIZE, ...) are advisory and ignored. A (SPECIAL
    var*) declaration, however, changes semantics: within this LOCALLY's
    body, references to `var` must go through its global/dynamic value cell
    (the same cell SYMBOL-VALUE/BOUNDP/SET/PROGV use) instead of any
    lexical binding. We implement that by installing a symbol-macro that
    expands `var` to `(SYMBOL-VALUE 'var)` in a child environment, scoped
    to this LOCALLY's body only.
    """
    from .evaluation_core import eval

    args = cdr(form)
    result = lisptype.NIL
    special_vars = []

    # Process DECLARE forms at the start
    while _consp_internal(args):
        first = car(args)
        # Check if this is a DECLARE form
        if (_consp_internal(first) and
            isinstance(car(first), lisptype.LispSymbol) and
            car(first).name == 'DECLARE'):
            decl_specs = cdr(first)
            while _consp_internal(decl_specs):
                spec = car(decl_specs)
                if (_consp_internal(spec) and isinstance(car(spec), lisptype.LispSymbol)
                        and car(spec).name == 'SPECIAL'):
                    names = cdr(spec)
                    while _consp_internal(names):
                        var = car(names)
                        if isinstance(var, lisptype.LispSymbol):
                            special_vars.append(var)
                        names = cdr(names)
                decl_specs = cdr(decl_specs)
            # Skip this declaration
            args = cdr(args)
        else:
            # Not a declaration, start evaluating body
            break

    if special_vars:
        body_env = lisptype.Environment(parent=env)
        for var in special_vars:
            # %SPECIAL-REF reads the symbol's dynamic value cell if one has
            # been established (e.g. by PROGV), falling back to a normal
            # lexical lookup of `var` otherwise -- so this doesn't break
            # variables that are only ever bound lexically (e.g. a DO/LET
            # loop variable that also happens to be declared special for
            # its own binding form, which isn't dynamically bound here).
            special_ref_form = cons(lisptype.LispSymbol('%SPECIAL-REF'), cons(var, lisptype.NIL))
            body_env.add_symbol_macro(var, special_ref_form)
    else:
        body_env = env

    # Evaluate remaining body forms
    while _consp_internal(args):
        result = eval(car(args), body_env)
        args = cdr(args)
    
    return result


def eval_let(form, env):
    """Evaluate LET special form with parallel binding semantics.
    
    (LET ((var1 init1) (var2 init2) ...) body...)
    
    In LET, all init forms are evaluated in the current environment BEFORE
    any bindings are created in the new scope. This is "parallel" binding.
    """
    from .evaluation_core import eval
    
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("LET requires at least a binding list")
    
    bindings_form = car(args)
    body = cdr(args)
    
    # Create new environment for LET scope
    let_env = lisptype.Environment(env)
    
    # Process bindings - evaluate all init forms in OUTER environment first
    bindings_list = []
    current = bindings_form
    while _consp_internal(current):
        binding = car(current)
        # Support both binding forms of the shape (var init) and bare var symbols
        if _consp_internal(binding):
            var = car(binding)
            init_form = car(cdr(binding))
        else:
            var = binding
            init_form = lisptype.NIL

        # Evaluate init in OUTER environment
        value = eval(init_form, env)

        bindings_list.append((var, value))
        current = cdr(current)
    
    # Now bind all variables in new environment
    # Track special variables that need special handling
    import fclpy.state as state
    old_package = None
    has_package_binding = False
    # Determine the global/root environment for special-variable checks
    global_env = env
    while global_env.parent is not None:
        global_env = global_env.parent

    # Collect any local DECLARE (SPECIAL ...) entries at start of body
    local_specials = set()
    decl_cursor = body
    while _consp_internal(decl_cursor):
        first = car(decl_cursor)
        if (_consp_internal(first) and isinstance(car(first), lisptype.LispSymbol) and car(first).name == 'DECLARE'):
            # iterate over declaration specs inside this DECLARE
            specs = cdr(first)
            while _consp_internal(specs):
                spec = car(specs)
                if (_consp_internal(spec) and isinstance(car(spec), lisptype.LispSymbol) and car(spec).name == 'SPECIAL'):
                    # add all symbols in (SPECIAL ...)
                    s = cdr(spec)
                    while _consp_internal(s):
                        sym = car(s)
                        if isinstance(sym, lisptype.LispSymbol):
                            local_specials.add(sym.name)
                        s = cdr(s)
                specs = cdr(specs)
            # move to next top-level form (in case multiple DECLAREs present)
            decl_cursor = cdr(decl_cursor)
        else:
            break

    # (symbol, had_value, old_value) for each dynamically (specially) bound
    # variable, so its value cell can be restored when LET exits -- the same
    # cell SYMBOL-VALUE/BOUNDP/SET/PROGV use, so a SET inside this LET's
    # extent is visible to plain references to the variable and vice versa.
    dynamic_saves = []
    for var, value in bindings_list:
        if isinstance(var, lisptype.LispSymbol):
            # If this symbol has been declared SPECIAL locally or globally,
            # bind its dynamic value cell. Otherwise bind lexically in let_env.
            if var.name in local_specials or (hasattr(global_env, '_special_variables') and var.name in global_env._special_variables):
                had_value = getattr(var, 'value', None) is not None
                dynamic_saves.append((var, had_value, getattr(var, 'value', None)))
                var.value = value
            else:
                let_env.add_variable(var, value)

            # Handle *PACKAGE* special variable - update state.current_package
            if var.name == '*PACKAGE*' and isinstance(value, lisptype.Package):
                old_package = getattr(state, 'current_package', None)
                state.current_package = value
                has_package_binding = True

    # Update state.current_environment for functions that need it (like LOAD)
    old_env = state.current_environment
    state.current_environment = let_env

    try:
        # Evaluate body in new environment
        result = None
        current = body
        while _consp_internal(current):
            result = eval(car(current), let_env)
            current = cdr(current)

        return result
    finally:
        # Restore the previous environment
        state.current_environment = old_env
        # Restore *PACKAGE* if it was dynamically bound
        if has_package_binding and old_package is not None:
            state.current_package = old_package
        # Restore any dynamically (specially) bound variables' value cells
        for sym, had_value, old_value in dynamic_saves:
            sym.value = old_value if had_value else None


def eval_letstar(form, env):
    """Evaluate LET* special form with sequential binding semantics.
    
    (LET* ((var1 init1) (var2 init2) ...) body...)
    
    In LET*, each init form is evaluated in the environment AFTER previous
    bindings have been established. This is "sequential" binding.
    """
    from .evaluation_core import eval
    
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("LET* requires at least a binding list")
    
    bindings_form = car(args)
    body = cdr(args)
    
    # Create new environment for LET* scope
    letstar_env = lisptype.Environment(env)
    
    # Track special variables that need special handling
    import fclpy.state as state
    old_package = getattr(state, 'current_package', None)
    has_package_binding = False
    
    # Collect any local DECLARE (SPECIAL ...) entries at start of body
    local_specials = set()
    decl_cursor = body
    while _consp_internal(decl_cursor):
        first = car(decl_cursor)
        if (_consp_internal(first) and isinstance(car(first), lisptype.LispSymbol) and car(first).name == 'DECLARE'):
            specs = cdr(first)
            while _consp_internal(specs):
                spec = car(specs)
                if (_consp_internal(spec) and isinstance(car(spec), lisptype.LispSymbol) and car(spec).name == 'SPECIAL'):
                    s = cdr(spec)
                    while _consp_internal(s):
                        sym = car(s)
                        if isinstance(sym, lisptype.LispSymbol):
                            local_specials.add(sym.name)
                        s = cdr(s)
                specs = cdr(specs)
            decl_cursor = cdr(decl_cursor)
        else:
            break

    # Process bindings sequentially - each can see previous ones
    current = bindings_form
    while _consp_internal(current):
        binding = car(current)
        if _consp_internal(binding):
            var = car(binding)
            init_form = car(cdr(binding))
            # Evaluate init in CURRENT environment (with previous bindings)
            value = eval(init_form, letstar_env)
            if isinstance(var, lisptype.LispSymbol):
                # If declared SPECIAL globally, bind into the global environment
                # to provide dynamic semantics; otherwise bind lexically.
                global_env = env
                while global_env.parent is not None:
                    global_env = global_env.parent
                if var.name in local_specials or (hasattr(global_env, '_special_variables') and var.name in global_env._special_variables):
                    global_env.add_variable(var, value)
                else:
                    letstar_env.add_variable(var, value)
                # Handle *PACKAGE* special variable - update state.current_package
                if var.name == '*PACKAGE*' and isinstance(value, lisptype.Package):
                    if not has_package_binding:
                        old_package = getattr(state, 'current_package', None)
                        has_package_binding = True
                    state.current_package = value
        current = cdr(current)
    
    # Update state.current_environment for functions that need it (like LOAD)
    old_env = state.current_environment
    state.current_environment = letstar_env
    
    try:
        # Evaluate body in environment with all bindings
        result = None
        current = body
        while _consp_internal(current):
            result = eval(car(current), letstar_env)
            current = cdr(current)
        
        return result
    finally:
        # Restore the previous environment
        state.current_environment = old_env
        # Restore *PACKAGE* if it was dynamically bound
        if has_package_binding:
            state.current_package = old_package


def eval_flet(form, env):
    """Evaluate FLET special form - local function bindings.
    
    (FLET ((name1 (args1...) body1...) (name2 (args2...) body2...)) body...)
    
    FLET establishes local function bindings. The function definitions
    DO NOT see each other (use LABELS for mutually recursive functions).
    The body is evaluated in an environment with the local function bindings.
    """
    from .evaluation_core import eval
    
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("FLET requires at least a binding list")
    
    bindings_form = car(args)
    body = cdr(args)
    
    # Create new environment for FLET scope
    flet_env = lisptype.Environment(env)
    
    # Process function definitions - create closures in OUTER environment
    current = bindings_form
    while _consp_internal(current):
        binding = car(current)
        if _consp_internal(binding):
            func_name = car(binding)
            func_lambda_list = car(cdr(binding))
            func_body = cdr(cdr(binding))
            
            # Create a lambda-like closure
            if isinstance(func_name, lisptype.LispSymbol):
                # Build the function closure. FLET establishes an implicit
                # block named after the function, same as DEFUN.
                closure = make_lambda_closure(func_lambda_list, func_body, env, block_name=func_name)
                # Bind the function in the new environment
                flet_env.add_function(func_name, closure)
        current = cdr(current)
    
    # Evaluate body in environment with local function bindings
    result = lisptype.NIL
    current = body
    while _consp_internal(current):
        result = eval(car(current), flet_env)
        current = cdr(current)
    
    return result


def eval_labels(form, env):
    """Evaluate LABELS special form - mutually recursive local function bindings.
    
    (LABELS ((name1 (args1...) body1...) (name2 (args2...) body2...)) body...)
    
    LABELS establishes local function bindings where the functions CAN see
    each other (for mutual recursion). The body is evaluated in an environment
    with the local function bindings.
    """
    from .evaluation_core import eval
    
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("LABELS requires at least a binding list")
    
    bindings_form = car(args)
    body = cdr(args)
    
    # Create new environment for LABELS scope
    labels_env = lisptype.Environment(env)
    
    # Process function definitions - create closures in the NEW environment
    # so they can see each other
    current = bindings_form
    while _consp_internal(current):
        binding = car(current)
        if _consp_internal(binding):
            func_name = car(binding)
            func_lambda_list = car(cdr(binding))
            func_body = cdr(cdr(binding))
            
            # Create a lambda-like closure in the labels environment
            if isinstance(func_name, lisptype.LispSymbol):
                # Build the function closure - uses labels_env so functions can call each other.
                # LABELS establishes an implicit block named after the function, same as DEFUN.
                closure = make_lambda_closure(func_lambda_list, func_body, labels_env, block_name=func_name)
                # Bind the function in the new environment
                labels_env.add_function(func_name, closure)
        current = cdr(current)
    
    # Evaluate body in environment with local function bindings
    result = lisptype.NIL
    current = body
    while _consp_internal(current):
        result = eval(car(current), labels_env)
        current = cdr(current)
    
    return result


def make_lambda_closure(lambda_list, body, env, block_name=None):
    """Create a closure function from a lambda list and body.

    This handles parsing the lambda list with &optional, &rest, &key, etc.

    If block_name is given (FLET/LABELS pass the function's own name), the
    body is enclosed in an implicit block of that name, so RETURN-FROM on it
    works, matching the DEFUN implicit block.
    """
    from .evaluation_core import eval, ReturnFromException
    
    # Parse the lambda list
    required_params = []
    optional_params = []  # (name, default_value)
    rest_param = None
    key_params = []  # (keyword, name, default_value)
    
    current = lambda_list
    mode = 'required'
    
    while _consp_internal(current):
        param = car(current)
        if isinstance(param, lisptype.LispSymbol):
            param_name = param.name.upper()
            if param_name == '&OPTIONAL':
                mode = 'optional'
            elif param_name == '&REST':
                mode = 'rest'
            elif param_name == '&KEY':
                mode = 'key'
            elif param_name == '&ALLOW-OTHER-KEYS':
                pass  # Ignore for now
            elif param_name == '&BODY':
                mode = 'rest'  # &body is similar to &rest
            elif param_name == '&AUX':
                mode = 'aux'
            else:
                if mode == 'required':
                    required_params.append(param)
                elif mode == 'optional':
                    optional_params.append((param, lisptype.NIL))
                elif mode == 'rest':
                    rest_param = param
                    mode = 'after_rest'  # Only one &rest param
                elif mode == 'key':
                    key_params.append((lisptype.intern_keyword(param.name), param, lisptype.NIL))
                elif mode == 'aux':
                    pass  # &aux params are local bindings, handle later
        elif _consp_internal(param):
            # Complex parameter form: (name default) or (name default supplied-p)
            pname = car(param)
            pdefault = car(cdr(param)) if _consp_internal(cdr(param)) else lisptype.NIL
            if mode == 'optional':
                optional_params.append((pname, pdefault))
            elif mode == 'key':
                # Key params can be (name default) or ((:keyword name) default)
                if _consp_internal(pname):
                    keyword = car(pname)
                    actual_name = car(cdr(pname))
                    key_params.append((keyword, actual_name, pdefault))
                else:
                    key_params.append((lisptype.intern_keyword(pname.name), pname, pdefault))
        current = cdr(current)
    
    def closure_function(*args):
        # Create local environment for this call
        call_env = lisptype.Environment(env)
        
        args_list = list(args)
        arg_idx = 0
        
        # Bind required parameters
        for param in required_params:
            if arg_idx < len(args_list):
                call_env.add_variable(param, args_list[arg_idx])
                arg_idx += 1
            else:
                call_env.add_variable(param, lisptype.NIL)
        
        # Bind optional parameters
        for param, default in optional_params:
            if arg_idx < len(args_list):
                call_env.add_variable(param, args_list[arg_idx])
                arg_idx += 1
            else:
                # Evaluate default in closure environment
                default_val = eval(default, env) if default != lisptype.NIL else lisptype.NIL
                call_env.add_variable(param, default_val)
        
        # Handle keyword arguments
        remaining_args = args_list[arg_idx:]
        keyword_values = {}
        i = 0
        while i < len(remaining_args):
            if isinstance(remaining_args[i], lisptype.lispKeyword):
                if i + 1 < len(remaining_args):
                    keyword_values[remaining_args[i].name.upper()] = remaining_args[i + 1]
                    i += 2
                else:
                    i += 1
            else:
                i += 1
        
        for keyword, param, default in key_params:
            key_name = keyword.name.upper()
            if key_name in keyword_values:
                call_env.add_variable(param, keyword_values[key_name])
            else:
                # Evaluate default
                default_val = eval(default, env) if default != lisptype.NIL else lisptype.NIL
                call_env.add_variable(param, default_val)
        
        # Bind rest parameter if present
        if rest_param:
            # Collect remaining non-keyword args
            rest_args = args_list[arg_idx:]
            # Build a list from rest args
            rest_list = lisptype.NIL
            for arg in reversed(rest_args):
                rest_list = cons(arg, rest_list)
            call_env.add_variable(rest_param, rest_list)
        
        # Evaluate body, enclosed in the implicit block FLET/LABELS
        # establishes around the function body (named after the function).
        result = lisptype.NIL
        try:
            current_form = body
            while _consp_internal(current_form):
                result = eval(car(current_form), call_env)
                current_form = cdr(current_form)
        except ReturnFromException as e:
            tag = e.tag
            block_match = False
            if tag == block_name:
                block_match = True
            elif isinstance(tag, lisptype.LispSymbol) and isinstance(block_name, lisptype.LispSymbol):
                block_match = (tag.name == block_name.name)
            if block_match:
                result = e.value
            else:
                raise

        return result
    
    return closure_function


def eval_quasiquote(form, env):
    """Evaluate a QUASIQUOTE form by processing UNQUOTE and UNQUOTE-SPLICING.

    This quasiquote evaluator handles common patterns including nested quasiquotes:
    - (QUASIQUOTE x) where x is a list will return a new list where elements
      of the form (UNQUOTE e) are replaced with the evaluated value of e,
      and elements of the form (UNQUOTE-SPLICING e) are spliced into the list.
    - Nested quasiquotes are properly handled with nesting level tracking.
    """
    from .evaluation_core import eval
    
    expr = car(cdr(form))

    def _quasi(obj, level=1):
        """Process quasiquote at given nesting level.
        
        level=1 means we're at the outermost quasiquote.
        level>1 means we're inside nested quasiquotes.
        """
        # If an explicit (UNQUOTE e) form
        if _consp_internal(obj) and isinstance(car(obj), lisptype.LispSymbol) and car(obj).name == 'UNQUOTE':
            if level == 1:
                # At outermost level, evaluate the unquote
                return eval(car(cdr(obj)), env)
            else:
                # Inside nested quasiquote, decrease level and recurse
                return cons(car(obj), cons(_quasi(car(cdr(obj)), level - 1), lisptype.NIL))
        
        # If an explicit (QUASIQUOTE e) form - entering nested quasiquote
        if _consp_internal(obj) and isinstance(car(obj), lisptype.LispSymbol) and car(obj).name == 'QUASIQUOTE':
            # Increase nesting level
            return cons(car(obj), cons(_quasi(car(cdr(obj)), level + 1), lisptype.NIL))

        # If atom, return as-is
        if not _consp_internal(obj):
            return obj

        # Otherwise obj is a cons/list: build a resulting list applying unquote rules
        parts = []
        cur = obj
        while _consp_internal(cur):
            item = car(cur)
            # Handle (UNQUOTE-SPLICING e)
            if _consp_internal(item) and isinstance(car(item), lisptype.LispSymbol):
                name = car(item).name
                if name == 'UNQUOTE-SPLICING':
                    if level == 1:
                        # At outermost level, evaluate and splice
                        val = eval(car(cdr(item)), env)
                        # If val is NIL, splice nothing (empty)
                        if val is lisptype.NIL or val is None:
                            pass  # Don't append anything
                        # If val is a lispCons, iterate its elements
                        elif isinstance(val, lisptype.lispCons):
                            for v in val:
                                parts.append(v)
                        elif isinstance(val, (list, tuple)):
                            for v in val:
                                parts.append(v)
                        else:
                            parts.append(val)
                        cur = cdr(cur)
                        continue
                    else:
                        # Inside nested quasiquote, decrease level and preserve structure
                        parts.append(cons(car(item), cons(_quasi(car(cdr(item)), level - 1), lisptype.NIL)))
                        cur = cdr(cur)
                        continue
                elif name == 'UNQUOTE':
                    if level == 1:
                        # At outermost level, evaluate
                        val = eval(car(cdr(item)), env)
                        parts.append(val)
                        cur = cdr(cur)
                        continue
                    else:
                        # Inside nested quasiquote, decrease level and preserve structure
                        parts.append(cons(car(item), cons(_quasi(car(cdr(item)), level - 1), lisptype.NIL)))
                        cur = cdr(cur)
                        continue
                elif name == 'QUASIQUOTE':
                    # Entering nested quasiquote, increase level
                    parts.append(cons(car(item), cons(_quasi(car(cdr(item)), level + 1), lisptype.NIL)))
                    cur = cdr(cur)
                    continue

            # Otherwise, recursively quasiquote the item
            if _consp_internal(item):
                parts.append(_quasi(item, level))
            else:
                parts.append(item)

            cur = cdr(cur)

        # Convert parts to a lispCons chain
        res = lisptype.NIL
        for p in reversed(parts):
            res = lisptype.lispCons(p, res)
        return res

    return _quasi(expr)



def eval_prog1(form, env):
    """Evaluate PROG1 special form."""
    from .evaluation_core import eval
    
    args = cdr(form)
    if not _consp_internal(args):
        return None
    
    result = _primary_value(eval(car(args), env))
    args = cdr(args)

    # Evaluate remaining forms for side effects
    while _consp_internal(args):
        eval(car(args), env)
        args = cdr(args)

    return result


def eval_prog2(form, env):
    """Evaluate PROG2 special form."""
    from .evaluation_core import eval
    
    args = cdr(form)
    if not _consp_internal(args) or not _consp_internal(cdr(args)):
        return None
    
    # Evaluate first form for side effects
    eval(car(args), env)
    
    # Return value of second form
    result = _primary_value(eval(car(cdr(args)), env))
    args = cdr(cdr(args))
    
    # Evaluate remaining forms for side effects
    while _consp_internal(args):
        eval(car(args), env)
        args = cdr(args)
    
    return result


def _eval_prog_impl(form, env, let_symbol_name):
    """Shared PROG/PROG* implementation.

    Per ANSI, (PROG (var*) body...) is equivalent to
    (BLOCK NIL (LET (var*) (TAGBODY . body))), and PROG* is the same with
    LET* instead of LET. Building that expansion and delegating to the
    existing BLOCK/LET/LET*/TAGBODY handlers keeps binding, GO-tag, and
    implicit-NIL-block semantics consistent with those special forms.
    """
    from .evaluation_core import eval

    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError(f"{let_symbol_name} requires a variable list")

    varlist = car(args)
    body = cdr(args)

    # Leading (DECLARE ...) forms belong to the LET's own bindings (e.g. to
    # mark a variable SPECIAL), not to the TAGBODY, so hoist them out of the
    # tagbody body and place them directly in the LET body ahead of it.
    declare_forms = []
    while _consp_internal(body):
        candidate = car(body)
        if _consp_internal(candidate) and isinstance(car(candidate), lisptype.LispSymbol) and car(candidate).name == 'DECLARE':
            declare_forms.append(candidate)
            body = cdr(body)
        else:
            break

    tagbody_form = cons(lisptype.LispSymbol('TAGBODY'), body)

    # let_body = (declare1 declare2 ... tagbody_form)
    let_body = cons(tagbody_form, lisptype.NIL)
    for declare_form in reversed(declare_forms):
        let_body = cons(declare_form, let_body)

    let_form = cons(lisptype.LispSymbol(let_symbol_name), cons(varlist, let_body))
    block_form = cons(lisptype.LispSymbol('BLOCK'), cons(lisptype.NIL, cons(let_form, lisptype.NIL)))
    return eval(block_form, env)


def eval_prog(form, env):
    """Evaluate PROG special form: (BLOCK NIL (LET (var*) (TAGBODY . body)))."""
    return _eval_prog_impl(form, env, 'LET')


def eval_prog_star(form, env):
    """Evaluate PROG* special form: (BLOCK NIL (LET* (var*) (TAGBODY . body)))."""
    return _eval_prog_impl(form, env, 'LET*')


def eval_time(form, env):
    """Evaluate TIME special form.
    
    TIME evaluates the form and prints timing information to *TRACE-OUTPUT*.
    Returns the result of evaluating the form.
    
    Usage: (TIME form)
    """
    from .evaluation_core import eval
    
    args = cdr(form)
    if not _consp_internal(args):
        return lisptype.NIL
    
    form_to_time = car(args)
    
    # Record start time (using process time for execution time)
    start_real = time.time()
    start_cpu = time.process_time()
    
    # Evaluate the form
    result = eval(form_to_time, env)
    
    # Record end time
    end_real = time.time()
    end_cpu = time.process_time()
    
    # Calculate elapsed times
    real_elapsed = end_real - start_real
    cpu_elapsed = end_cpu - start_cpu
    
    # Print timing info (like SBCL format) to stderr (*TRACE-OUTPUT*)
    # Convert to seconds with 3 decimal places
    print(f"Evaluation took:", file=sys.stderr)
    print(f"  {cpu_elapsed:.6f} seconds of CPU time", file=sys.stderr)
    print(f"  {real_elapsed:.6f} seconds of real time", file=sys.stderr)
    
    return result


def eval_loop(form, env):
    """Evaluate LOOP special form with lexical environment support.
    
    Extended LOOP implementation supporting ANSI CL loop macro clauses.
    This is implemented as a special form to access the lexical environment.

    Supports forms such as:
      (LOOP FOR i FROM 0 TO 9 BY 1 DO (PRINT i))
      (LOOP WHILE <test> DO <forms>)
      (LOOP UNTIL <test> DO <forms>)
      (LOOP REPEAT <n> DO <forms>)
      (LOOP WHILE <test> UNLESS <cond> DO <action> APPEND <form>)
      (LOOP FOR x IN list COLLECT x)
    """
    from .evaluation_core import eval
    
    # Get loop clauses from form
    args = cdr(form)
    
    # Normalize args into a Python list of forms
    forms = []
    current = args
    while _consp_internal(current):
        forms.append(car(current))
        current = cdr(current)
    
    def sym_name(x):
        """Get uppercase symbol name or None."""
        return x.name.upper() if isinstance(x, lisptype.LispSymbol) else None

    def _bind_varspec(loop_env, varspec, value):
        """Bind a LOOP var spec (symbol or simple destructuring pattern)."""
        # Common case: single symbol
        if isinstance(varspec, lisptype.LispSymbol):
            loop_env.set_variable(varspec, value)
            return

        # Destructuring patterns used by ANSI tests (e.g., (KEY . VAL))
        if _consp_internal(varspec):
            # Dotted pair pattern: (A . B)
            left = car(varspec)
            right = cdr(varspec)
            if isinstance(left, lisptype.LispSymbol) and isinstance(right, lisptype.LispSymbol) and not _consp_internal(right):
                if _consp_internal(value):
                    loop_env.set_variable(left, car(value))
                    loop_env.set_variable(right, cdr(value))
                else:
                    loop_env.set_variable(left, lisptype.NIL)
                    loop_env.set_variable(right, lisptype.NIL)
                return

            # Proper list pattern: (A B C)
            pat = varspec
            cur_val = value
            while _consp_internal(pat):
                pitem = car(pat)
                if isinstance(pitem, lisptype.LispSymbol):
                    if _consp_internal(cur_val):
                        loop_env.set_variable(pitem, car(cur_val))
                        cur_val = cdr(cur_val)
                    else:
                        loop_env.set_variable(pitem, lisptype.NIL)
                pat = cdr(pat)
            return

        raise lisptype.LispNotImplementedError('LOOP FOR requires a symbol')

    # Parse loop clauses into structured form
    i = 0

    # Iteration drivers, in clause order. Each driver is a dict with keys var,
    # kind and kind-specific data. CLHS 6.1.2: every iteration-control clause
    # (FOR/AS, and REPEAT, which is a bounding clause in the same group) is a
    # driver, and drivers *compose* -- the loop runs while all of them still
    # have a value. There is deliberately no scalar "the iteration type" here:
    # a single scalar cannot represent `for x = 7 repeat 5`, and whichever
    # clause was parsed last silently won.
    iteration_drivers = []

    # WHILE/UNTIL termination tests (CLHS 6.1.2.1.2). Also composing: a loop may
    # carry several, and they bound whatever drivers are present rather than
    # replacing them.
    #
    # Position matters. A termination test is evaluated where it is written
    # (CLHS 6.1.2.1.2), so `while x collect x` tests before accumulating while
    # `collect x until x` accumulates and then tests -- which is why each entry
    # records whether a main clause had already been seen when it was parsed.
    termination_tests = []  # list of ('while'/'until', test_form, after_body)

    conditionals = []  # list of ('when'/'unless', test_form)
    body_forms = []

    # Accumulation clauses, in order. CLHS 6.1.3 permits several in one loop
    # (`collect i into foo always (< i 20)`); a single slot silently kept only
    # the last one parsed and discarded the rest.
    # Each entry: {'type', 'form', 'into', 'conditionals'}.
    accumulations = []

    initially_forms = []  # INITIALLY prologue, run once before the first iteration
    finally_forms = []
    return_form = None  # RETURN clause form to evaluate during loop
    finally_return_form = None  # RETURN clause in FINALLY section (evaluated at end)
    loop_block_name = None  # NIL unless a NAMED clause gives the loop its own block name

    # Parse clauses
    while i < len(forms):
        token = forms[i]
        name = sym_name(token)

        if name == 'NAMED':
            loop_block_name = forms[i+1]
            i += 2
            continue

        if name in ('FOR', 'AS'):
            # CLHS 6.1.2.1: "either the keyword FOR or the keyword AS may be
            # used to begin a for-as-clause" -- AS is a full synonym, not a
            # distinct clause. Previously unrecognized, so e.g. (loop as x in
            # '(a b c) collect x) fell through to the "no iteration clause"
            # branch below and looped forever evaluating AS/X/IN as inert
            # body forms until the 10-minute LOOP_TIMEOUT_ERROR hard cap
            # fired -- exercised by ~15 tests across iteration/loop2-7.lsp.
            candidate_var = forms[i+1]
            if not (isinstance(candidate_var, lisptype.LispSymbol) or _consp_internal(candidate_var)):
                raise lisptype.LispNotImplementedError('LOOP FOR requires a symbol')

            clause_stop = LOOP_CLAUSE_KEYWORDS

            # Parse the FOR clause into either a driver (IN/ON/ACROSS/FROM...) or an aux binding (=
            # without FROM/IN/etc) when a driver already exists.
            j = i + 2
            saw_driver_keyword = False
            driver_kind = None
            driver_start = None
            driver_end = None
            driver_step = None
            driver_list = None
            aux_init = None
            aux_then = None
            driver_downward = False

            while j < len(forms):
                fname = sym_name(forms[j])
                if fname == 'FROM':
                    saw_driver_keyword = True
                    driver_start = forms[j+1]
                    j += 2
                elif fname in ('TO', 'UPTO'):
                    saw_driver_keyword = True
                    driver_end = forms[j+1]
                    driver_kind = 'for-range'
                    j += 2
                elif fname == 'BELOW':
                    saw_driver_keyword = True
                    driver_end = forms[j+1]
                    driver_kind = 'for-below'
                    j += 2
                elif fname == 'DOWNTO':
                    saw_driver_keyword = True
                    driver_end = forms[j+1]
                    driver_kind = 'for-range'
                    driver_downward = True
                    j += 2
                elif fname == 'ABOVE':
                    saw_driver_keyword = True
                    driver_end = forms[j+1]
                    driver_kind = 'for-below'
                    driver_downward = True
                    j += 2
                elif fname == 'BY':
                    saw_driver_keyword = True
                    driver_step = forms[j+1]
                    j += 2
                elif fname == 'IN':
                    saw_driver_keyword = True
                    driver_list = forms[j+1]
                    driver_kind = 'for-in'
                    j += 2
                elif fname == 'ON':
                    saw_driver_keyword = True
                    driver_list = forms[j+1]
                    driver_kind = 'for-on'
                    j += 2
                elif fname == 'BEING' or fname == 'BEING-THE':
                    # Handle forms like: FOR x BEING THE SYMBOLS OF "KEYWORD"
                    saw_driver_keyword = True
                    k = j + 1
                    # optional THE
                    if k < len(forms) and sym_name(forms[k]) == 'THE':
                        k += 1
                    # expect SYMBOLS
                    if k < len(forms) and sym_name(forms[k]) == 'SYMBOLS':
                        # optional OF <package>
                        if k + 1 < len(forms) and sym_name(forms[k+1]) == 'OF':
                            driver_kind = 'for-being-symbols'
                            driver_list = forms[k+2] if (k + 2) < len(forms) else None
                            j = k + 3
                        else:
                            driver_kind = 'for-being-symbols'
                            driver_list = None
                            j = k + 1
                    else:
                        # Not a supported BEING clause; stop parsing here
                        break
                elif fname == 'ACROSS':
                    saw_driver_keyword = True
                    driver_list = forms[j+1]
                    driver_kind = 'for-across'
                    j += 2
                elif fname == '=':
                    # FOR x = init-form [THEN step-form]
                    aux_init = forms[j+1]
                    j += 2
                    if j < len(forms) and sym_name(forms[j]) == 'THEN':
                        aux_then = forms[j+1]
                        j += 2
                    # '=' can be either a driver (if this is the first/only iteration clause)
                    # or an auxiliary binding when a driver already exists.
                    if not saw_driver_keyword and driver_kind is None:
                        driver_kind = 'for-equals'
                        driver_start = aux_init
                        driver_step = aux_then
                elif fname == 'THEN':
                    # THEN after = was already handled above
                    j += 2
                elif fname in clause_stop:
                    break
                else:
                    break

            # If we saw FROM but no end specifier, treat as an unbounded arithmetic progression.
            # This is valid in ANSI LOOP, and is often paired with another driver that
            # terminates the overall loop.
            if driver_kind is None and saw_driver_keyword and driver_start is not None:
                driver_kind = 'for-from'

            if driver_kind is None:
                # e.g., "FOR X" without IN/FROM/=
                raise lisptype.LispNotImplementedError('LOOP FOR clause missing iteration spec')

            # DOWNTO/ABOVE count downward: BY gives a magnitude, so negate it
            # (an explicit BY form is wrapped as "(- form)" and negated at eval time).
            if driver_downward:
                if driver_step is None:
                    driver_step = -1
                elif isinstance(driver_step, int):
                    driver_step = -abs(driver_step)
                else:
                    driver_step = cons(lisptype.LispSymbol('-'), cons(driver_step, lisptype.NIL))

            iteration_drivers.append({
                'var': candidate_var,
                'kind': driver_kind,
                'start': driver_start,
                'end': driver_end,
                'step': driver_step,
                'list': driver_list,
            })

            i = j
            continue

        elif name == 'WHILE':
            termination_tests.append(('while', forms[i+1], bool(body_forms or accumulations)))
            i += 2

        elif name == 'UNTIL':
            termination_tests.append(('until', forms[i+1], bool(body_forms or accumulations)))
            i += 2

        elif name == 'REPEAT':
            # CLHS 6.1.2.1.1: REPEAT bounds the iteration; it does not replace
            # whatever driver is present. Modelling it as an anonymous driver is
            # what makes `for x = 7 repeat 5` and `repeat 5 for x = 7` mean the
            # same thing regardless of clause order.
            iteration_drivers.append({
                'var': None,
                'kind': 'repeat',
                'count': forms[i+1],
            })
            i += 2

        elif name in ('WHEN', 'IF'):
            conditionals.append(('when', forms[i+1]))
            i += 2
            
        elif name == 'UNLESS':
            conditionals.append(('unless', forms[i+1]))
            i += 2
            
        elif name in ('DO', 'DOING'):
            # Collect body forms until next clause keyword
            # DO clause consumes the current conditionals - they don't apply to subsequent clauses
            i += 1
            while i < len(forms) and sym_name(forms[i]) not in LOOP_CLAUSE_KEYWORDS:
                body_forms.append(forms[i])
                i += 1

        elif name == 'INITIALLY':
            # CLHS 6.1.7.1: the prologue. Its forms run once, after the
            # iteration variables are established and before the first
            # iteration. Previously unrecognized, so its forms fell into
            # body_forms and were re-run every iteration (or were dropped
            # outright once a driver had been parsed).
            i += 1
            while i < len(forms) and sym_name(forms[i]) not in LOOP_CLAUSE_KEYWORDS:
                initially_forms.append(forms[i])
                i += 1

        elif name in ACCUMULATION_CLAUSES:
            # One branch for all the accumulation clauses. They differ only in
            # the accumulator they feed, which execute_iteration_body already
            # dispatches on, so a parse branch per keyword only meant one more
            # place for INTO to be forgotten.
            clause = {
                'type': ACCUMULATION_CLAUSES[name],
                'form': forms[i+1],
                'into': None,
                # Pending conditionals apply here only if no DO consumed them.
                'conditionals': [] if body_forms else list(conditionals),
            }
            i += 2
            # CLHS 6.1.3: "into var" accumulates into a loop-local variable
            # instead of into the loop's value. Previously the INTO token and
            # its variable simply fell through to the unrecognized-keyword
            # branch and were dropped, so the accumulation silently became the
            # loop's value and the named variable was never bound at all.
            if i < len(forms) and sym_name(forms[i]) == 'INTO':
                clause['into'] = forms[i+1]
                i += 2
            accumulations.append(clause)

        elif name == 'RETURN':
            # Store return form for evaluation during loop execution
            if i + 1 < len(forms):
                return_form = forms[i+1]
                i += 2
            else:
                i += 1
            
        elif name == 'FINALLY':
            i += 1
            while i < len(forms) and sym_name(forms[i]) not in LOOP_CLAUSE_KEYWORDS:
                f = forms[i]
                # Check if this form is (RETURN ...)
                if _consp_internal(f):
                    car_f = car(f)
                    if isinstance(car_f, lisptype.LispSymbol) and car_f.name.upper() == 'RETURN':
                        # Extract the return value from (RETURN form)
                        cdr_f = cdr(f)
                        if _consp_internal(cdr_f):
                            finally_return_form = car(cdr_f)
                        i += 1
                    else:
                        finally_forms.append(f)
                        i += 1
                else:
                    finally_forms.append(f)
                    i += 1
                
        else:
            # Simple loop (CLHS 6.1.1): with no iteration-control clause seen so
            # far the compound forms are the loop body. Once a driver or a
            # termination test exists this is an unrecognized loop keyword; it is
            # still dropped here, as it always has been -- see plan.md's
            # Discovered issues, this is the one remaining silent path in LOOP.
            if not iteration_drivers and not termination_tests:
                body_forms.append(token)
            i += 1
    
    # Execute the loop
    result = None
    return_triggered = False  # Flag for when RETURN is executed

    # ALWAYS/THEREIS decide the loop's value directly rather than accumulating.
    always_failed = False
    thereis_result = None

    # One accumulator per INTO destination, keyed by the variable's name (None
    # is "the loop's own value"). Several clauses may share a destination, which
    # is what makes `collect a into x collect b into x` accumulate in order.
    # Each state is {'type': str, 'items': [], 'number': int}.
    acc_states = {}

    def _acc_key(clause):
        into = clause['into']
        return None if into is None else into.name

    for _clause in accumulations:
        _key = _acc_key(_clause)
        if _key not in acc_states:
            acc_states[_key] = {'type': _clause['type'], 'items': [], 'number': 0}

    def _conditionals_pass(clause_conditionals, loop_env):
        """Evaluate a WHEN/UNLESS conditional list against this iteration."""
        for cond_type, cond_form in clause_conditionals:
            cond_result = eval(cond_form, loop_env)
            if cond_type == 'when' and not lisptype.is_truthy(cond_result):
                return False
            if cond_type == 'unless' and lisptype.is_truthy(cond_result):
                return False
        return True

    def _accumulated_value(key):
        """The Lisp value accumulated so far for one INTO destination.

        Shared by the loop's return value and by INTO, which is the same
        accumulator merely stored somewhere else -- computing it in two places
        is how they would drift.
        """
        state = acc_states.get(key)
        if state is None:
            return lisptype.NIL
        acc_type = state['type']
        if acc_type in ('collect', 'append', 'nconc'):
            result_list = lisptype.NIL
            for item in reversed(state['items']):
                result_list = cons(item, result_list)
            return result_list
        if acc_type in ('sum', 'count'):
            return state['number']
        if acc_type == 'always':
            # T for all iterations, including vacuous truth
            return lisptype.NIL if always_failed else lisptype.T
        if acc_type == 'thereis':
            return thereis_result if thereis_result is not None else lisptype.NIL
        return lisptype.NIL

    def execute_iteration_body(loop_env):
        """Execute one iteration of the loop body."""
        nonlocal result, always_failed, thereis_result, return_triggered

        # Check for RETURN form and evaluate it if present
        if return_form is not None:
            result = eval(return_form, loop_env)
            return_triggered = True
            return

        # Execute body forms (controlled by main conditionals like WHEN/UNLESS)
        if _conditionals_pass(conditionals, loop_env):
            for f in body_forms:
                result = eval(f, loop_env)

        # Each accumulation clause has its own conditionals (possibly empty).
        for clause in accumulations:
            if not _conditionals_pass(clause['conditionals'], loop_env):
                continue
            acc_type = clause['type']
            key = _acc_key(clause)
            state = acc_states[key]
            acc_value = eval(clause['form'], loop_env)

            if acc_type == 'collect':
                state['items'].append(acc_value)
            elif acc_type in ('append', 'nconc'):
                # NCONC is destructive in ANSI; the observable result is the
                # same here because the accumulator owns its own list.
                if _consp_internal(acc_value):
                    cur = acc_value
                    while _consp_internal(cur):
                        state['items'].append(car(cur))
                        cur = cdr(cur)
                elif acc_type == 'append' and acc_value is not lisptype.NIL and acc_value is not None:
                    state['items'].append(acc_value)
            elif acc_type == 'sum':
                state['number'] += acc_value
            elif acc_type == 'count':
                if lisptype.is_truthy(acc_value):
                    state['number'] += 1
            elif acc_type == 'always':
                # ALWAYS: the test must hold on every iteration; the first
                # failure ends the loop with NIL.
                if not lisptype.is_truthy(acc_value):
                    always_failed = True
                    return_triggered = True
            elif acc_type == 'thereis':
                # THEREIS: the first true value ends the loop and is its value.
                if lisptype.is_truthy(acc_value):
                    thereis_result = acc_value
                    return_triggered = True

            if clause['into'] is not None:
                loop_env.set_variable(clause['into'], _accumulated_value(key))

            if return_triggered:
                return

    loop_watchdog = LoopWatchdog(
        'LOOP',
        lambda: [f"body_forms: {body_forms}",
                 f"drivers: {[(d['kind'], d['var']) for d in iteration_drivers]}",
                 f"termination_tests: {termination_tests}"],
        hard_cap=LOOP_TIMEOUT_ERROR)

    def _run_loop_and_finalize():
        """Run the loop's iteration/FINALLY/result logic under the loop's own
        implicit NIL block, so a plain (RETURN x) / (RETURN-FROM NIL x) inside
        the body exits *this* LOOP form instead of leaking out to whatever
        enclosing DO/DOLIST/DOTIMES/LOOP happens to be running the dynamic
        extent (see plan.md Finding under M0 step 1).
        """
        nonlocal result
        # Main loop execution, watched for runaway iteration. The watchdog is
        # created in the enclosing scope so the RESOLVED/ABORTED counterpart can
        # be emitted around the whole loop, including its non-local exits.
        check_loop_timeout = loop_watchdog.tick

        def _termination_break(loop_env, after_body):
            """True if a WHILE/UNTIL clause at this position ends the loop.

            after_body selects the tests written after the first main clause, so
            `collect x until x` accumulates and then tests, while `while x
            collect x` tests first (CLHS 6.1.2.1.2).
            """
            for kind, test_form, test_after_body in termination_tests:
                if test_after_body != after_body:
                    continue
                test_result = eval(test_form, loop_env)
                if kind == 'until' and lisptype.is_truthy(test_result):
                    return True
                if kind == 'while' and not lisptype.is_truthy(test_result):
                    return True
            return False

        def _init_driver(loop_env, driver):
            kind = driver['kind']
            if kind in ('for-in', 'for-on'):
                driver['_cur'] = eval(driver['list'], loop_env)
                return True
            if kind == 'for-across':
                driver['_seq'] = eval(driver['list'], loop_env)
                driver['_idx'] = 0
                return True
            if kind in ('for-range', 'for-below'):
                # `for x to 5` and `for x below 5` omit FROM, so 'start' is
                # present but None; the CLHS default is 0. (driver.get('start', 0)
                # does not do this -- the key exists, so the default never
                # applies and the loop evaluated None as its start value.)
                start_form = driver.get('start')
                if start_form is None:
                    start_form = 0
                end_form = driver.get('end')
                step_form = driver.get('step')
                if step_form is None:
                    step_form = 1
                start = eval(start_form, loop_env) if not isinstance(start_form, int) else start_form
                end = eval(end_form, loop_env)
                step = eval(step_form, loop_env) if not isinstance(step_form, int) else step_form
                if step == 0:
                    raise lisptype.LispNotImplementedError('LOOP BY step cannot be 0')
                driver['_cur'] = start
                driver['_end'] = end
                driver['_step'] = step
                return True
            if kind == 'for-from':
                # `for x to 5` and `for x below 5` omit FROM, so 'start' is
                # present but None; the CLHS default is 0. (driver.get('start', 0)
                # does not do this -- the key exists, so the default never
                # applies and the loop evaluated None as its start value.)
                start_form = driver.get('start')
                if start_form is None:
                    start_form = 0
                step_form = driver.get('step')
                if step_form is None:
                    step_form = 1
                start = eval(start_form, loop_env) if not isinstance(start_form, int) else start_form
                step = eval(step_form, loop_env) if not isinstance(step_form, int) else step_form
                if step == 0:
                    raise lisptype.LispNotImplementedError('LOOP BY step cannot be 0')
                driver['_cur'] = start
                driver['_step'] = step
                return True
            if kind == 'repeat':
                count = eval(driver['count'], loop_env)
                if not isinstance(count, (int, float)) or isinstance(count, bool):
                    raise lisptype.LispNotImplementedError(
                        f'LOOP REPEAT requires a number, got {count!r}')
                driver['_remaining'] = count
                return True
            if kind == 'for-equals':
                # Nothing is evaluated here. CLHS 6.1.2.1.3: for-as-equals-then
                # computes its value on each iteration, and for sequential FOR
                # clauses that value may depend on a driver bound earlier in the
                # same iteration -- e.g.
                #   (LOOP FOR I FROM 0 BELOW 256 FOR C = (CODE-CHAR I) ...)
                # so the init form is evaluated in _bind_driver, after the
                # preceding drivers have bound their variables, not at loop setup
                # when I does not exist yet.
                driver['_first'] = True
                return True
            if kind == 'for-being-symbols':
                # Evaluate package spec (could be string, symbol, or package object)
                pkg_spec = driver.get('list')
                pkg = None
                if pkg_spec is None:
                    pkg = None
                else:
                    # Evaluate the package spec in the loop environment if it's an expression
                    try:
                        pkg_val = eval(pkg_spec, loop_env) if not isinstance(pkg_spec, (str,)) else pkg_spec
                    except Exception:
                        pkg_val = pkg_spec

                    # pkg_val may be a LispPackage, LispSymbol, or string.
                    # (No local `import fclpy.lisptype as lisptype` here: it
                    # made lisptype a local of the whole enclosing function, so
                    # every earlier `lisptype.LispNotImplementedError` in
                    # _init_driver raised UnboundLocalError instead. The
                    # module-level import at the top of this file is the one to
                    # use.)
                    if isinstance(pkg_val, lisptype.Package):
                        pkg = pkg_val
                    else:
                        name = None
                        if isinstance(pkg_val, lisptype.LispSymbol):
                            name = pkg_val.name
                        elif isinstance(pkg_val, str):
                            name = pkg_val
                        else:
                            # Try string conversion
                            name = str(pkg_val)
                        try:
                            pkg = lisptype.find_package(name)
                        except Exception:
                            pkg = None

                # Build a cons list of symbols from the package
                cur_list = lisptype.NIL
                if pkg is not None and hasattr(pkg, 'symbols'):
                    # iterate over symbol objects
                    vals = list(pkg.symbols.values())
                    for s in reversed(vals):
                        cur_list = cons(s, cur_list)
                driver['_cur'] = cur_list
                return True
            raise lisptype.LispNotImplementedError(f'LOOP driver kind not implemented: {kind}')

        def _driver_has_value(driver):
            kind = driver['kind']
            if kind in ('for-in', 'for-on'):
                return _consp_internal(driver.get('_cur'))
            if kind == 'for-across':
                seq = driver.get('_seq')
                idx = driver.get('_idx', 0)
                if isinstance(seq, str):
                    return idx < len(seq)
                if hasattr(seq, '__len__'):
                    return idx < len(seq)
                return False
            if kind == 'for-range':
                cur = driver.get('_cur')
                end = driver.get('_end')
                step = driver.get('_step')
                if step is None:
                    return False
                return (cur <= end) if step > 0 else (cur >= end)
            if kind == 'for-below':
                cur = driver.get('_cur')
                end = driver.get('_end')
                step = driver.get('_step')
                if step is None:
                    return False
                return (cur < end) if step > 0 else (cur > end)
            if kind == 'for-from':
                return True
            if kind == 'for-equals':
                # An unbounded driver: `for x = form` never terminates on its
                # own (CLHS 6.1.2.1.3), so something else -- REPEAT, WHILE,
                # another driver, or a non-local exit -- must bound the loop.
                return True
            if kind == 'repeat':
                return driver.get('_remaining', 0) > 0
            if kind == 'for-being-symbols':
                return _consp_internal(driver.get('_cur'))
            return False

        def _bind_driver(loop_env, driver):
            kind = driver['kind']
            var = driver['var']
            if kind == 'for-in':
                _bind_varspec(loop_env, var, car(driver['_cur']))
                return
            if kind == 'for-on':
                _bind_varspec(loop_env, var, driver['_cur'])
                return
            if kind == 'for-across':
                seq = driver['_seq']
                idx = driver['_idx']
                # The elements of a string are CHARACTERs (CLHS 15.1), so
                # `loop for e across "abcd"` must bind characters. Binding
                # the bare length-1 strings that indexing yields only looked
                # correct because EQL conflates the two; it made a character
                # simultaneously a one-element string, and therefore a
                # one-element vector, which anything walking a sequence
                # element-wise follows into unbounded recursion. Shared with
                # AREF so both halves agree on what a character is.
                from .sequences_higher import string_element
                _bind_varspec(loop_env, var, string_element(seq, seq[idx]))
                return
            if kind in ('for-range', 'for-below'):
                _bind_varspec(loop_env, var, driver['_cur'])
                return
            if kind == 'for-from':
                _bind_varspec(loop_env, var, driver['_cur'])
                return
            if kind == 'for-equals':
                if driver['_first']:
                    value_form = driver.get('start')
                    driver['_first'] = False
                else:
                    # No THEN form means the init form supplies every value.
                    step_form = driver.get('step')
                    value_form = driver.get('start') if step_form is None else step_form
                _bind_varspec(loop_env, var, eval(value_form, loop_env))
                return
            if kind == 'repeat':
                return
            if kind == 'for-being-symbols':
                _bind_varspec(loop_env, var, car(driver['_cur']))
                return

        def _step_driver(loop_env, driver):
            kind = driver['kind']
            if kind in ('for-in', 'for-on'):
                driver['_cur'] = cdr(driver['_cur'])
                return
            if kind == 'for-across':
                driver['_idx'] = driver.get('_idx', 0) + 1
                return
            if kind in ('for-range', 'for-below'):
                driver['_cur'] = driver['_cur'] + driver['_step']
                return
            if kind == 'for-from':
                driver['_cur'] = driver['_cur'] + driver['_step']
                return
            if kind == 'for-equals':
                # Stepping happens in _bind_driver -- see _init_driver.
                return
            if kind == 'repeat':
                driver['_remaining'] -= 1
                return
            if kind == 'for-being-symbols':
                driver['_cur'] = cdr(driver['_cur'])
                return
    
        # ------------------------------------------------------------------
        # The one iteration engine.
        #
        # There used to be nine near-duplicate copies of this loop here -- one
        # per iteration_type ('while', 'until', 'repeat', 'for-range',
        # 'for-below', 'for-in', 'for-on', 'for-across', 'for-equals') -- plus
        # this generic driver path, which only ever ran for driver kinds that
        # had no inline copy. Because a single `iteration_type` scalar selected
        # among them, the *last* iteration-control clause parsed decided which
        # engine ran and every other clause was discarded: `for x = 7 repeat 5`
        # ran REPEAT's copy, which never binds a driver variable (hence
        # "Unbound variable: X"), and `repeat 5 for x = 7` ran for-equals' copy,
        # which has no bound, hence a loop that never terminates.
        #
        # CLHS 6.1.2 has no such scalar: iteration-control clauses compose, and
        # the loop ends when any one of them runs out. That is exactly what
        # `all(_driver_has_value(...))` expresses, so all the special cases are
        # gone rather than fixed.
        # ------------------------------------------------------------------
        loop_env = lisptype.Environment(env)
        for d in iteration_drivers:
            _init_driver(loop_env, d)

        # INTO names a variable local to the loop (CLHS 6.1.3), so bind it here
        # -- add_variable, not set_variable, or the accumulation would assign
        # through to an outer binding of the same name and clobber it.
        for clause in accumulations:
            if clause['into'] is not None:
                loop_env.add_variable(clause['into'], _accumulated_value(_acc_key(clause)))

        # The prologue runs once, after the iteration variables exist and
        # before the first termination test (CLHS 6.1.7.1).
        for f in initially_forms:
            eval(f, loop_env)

        # With no drivers, no termination test and nothing to execute there is
        # nothing to iterate; running would just spin until the hard cap.
        if iteration_drivers or termination_tests or body_forms or accumulations \
                or (return_form is not None):
            while all(_driver_has_value(d) for d in iteration_drivers):
                check_loop_timeout()

                # Bind before testing. A termination test routinely reads the
                # variable its own driver supplies -- (loop for x = 1 then (* 2 x)
                # while (< x 20) ...) -- so testing first sees either an unbound
                # variable on the first iteration or a stale one thereafter.
                for d in iteration_drivers:
                    _bind_driver(loop_env, d)

                if _termination_break(loop_env, after_body=False):
                    break

                execute_iteration_body(loop_env)
                if return_triggered:
                    break

                if _termination_break(loop_env, after_body=True):
                    break

                for d in iteration_drivers:
                    _step_driver(loop_env, d)

        # CLHS 6.1.2.2: ALWAYS/NEVER/THEREIS terminate the loop *immediately*
        # when their test decides the answer -- the epilogue does not run, so a
        # FINALLY (RETURN ...) cannot override the NIL or the found value.
        if always_failed or thereis_result is not None:
            return thereis_result if thereis_result is not None else lisptype.NIL

        # Execute FINALLY forms -- in the loop environment, so they can see the
        # iteration variables and any INTO accumulator (CLHS 6.1.4: the epilogue
        # is inside the loop's variable bindings).
        for f in finally_forms:
            result = eval(f, loop_env)

        # Execute FINALLY RETURN if present (overrides early RETURN)
        if finally_return_form is not None:
            # CLHS 6.1.1.4: a value returned by the epilogue takes precedence
            # over the one an accumulation clause would have produced, so this
            # must return directly rather than fall through to the accumulation
            # block below, which would discard it.
            return eval(finally_return_form, loop_env)

        # An accumulation with INTO feeds its variable, not the loop's value;
        # only a destination-less clause supplies the value of the LOOP form.
        if None in acc_states:
            return _accumulated_value(None)

        # Ensure we always return a Lisp value, never None
        # If result is still None (no body executed, no return form), return NIL
        if result is None:
            return lisptype.NIL
    
        return result

    with loop_watchdog:
        return _run_with_nil_block(_run_loop_and_finalize, loop_block_name)


def _run_with_nil_block(thunk, block_name=None):
    """Run thunk(), catching a RETURN/RETURN-FROM aimed at the implicit block
    DO/DO*/DOLIST/DOTIMES/LOOP establish around their loop.

    block_name is the target block's name: None/NIL for the ordinary implicit
    NIL block every one of these forms gets by default, or a symbol for a
    LOOP that used a NAMED clause (CLHS 6.1: NAMED gives the loop its own
    block instead of NIL, so a bare (RETURN x) -- which is (RETURN-FROM NIL
    x) -- must NOT be caught here; it has to keep propagating to find an
    actual enclosing NIL block).
    """
    from .evaluation_core import ReturnFromException

    def _tag_name(tag):
        if tag is None or tag == lisptype.NIL:
            return 'NIL'
        if isinstance(tag, lisptype.LispSymbol):
            return tag.name
        return None

    target_name = _tag_name(block_name)
    try:
        return thunk()
    except ReturnFromException as e:
        if _tag_name(e.tag) == target_name:
            return e.value
        raise


def _exec_iteration_body(body, env):
    """Execute a DO/DO*/DOLIST/DOTIMES body of {tag | statement}* forms as a
    TAGBODY, so GO can jump between tags in the body (per ANSI, these forms
    all accept the same tagbody-style body as TAGBODY itself).
    """
    from .evaluation_control_flow import eval_tagbody
    tagbody_form = cons(lisptype.LispSymbol('TAGBODY'), body)
    eval_tagbody(tagbody_form, env)


def eval_do(form, env):
    """Evaluate DO special form.
    
    (DO ((var init [step])*)
        (end-test result-form*)
        declaration*
        {tag | statement}*)
    
    DO evaluates init forms in parallel (like LET) and binds variables.
    """
    from .evaluation_core import eval
    
    args = cdr(form)
    if not _consp_internal(args):
        return lisptype.NIL
    
    # Parse variable bindings
    var_list = car(args)
    args = cdr(args)
    
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("DO requires end-test clause")
    
    # Parse end-test clause  
    end_clause = car(args)
    body = cdr(args)
    
    if not _consp_internal(end_clause):
        raise lisptype.LispNotImplementedError("DO end-test must be a list")
    
    end_test = car(end_clause)
    result_forms = cdr(end_clause)
    
    # Create new environment
    loop_env = lisptype.Environment(env)
    
    # Parse var specs and evaluate init forms IN PARALLEL (collect first)
    var_specs = []
    current = var_list
    while _consp_internal(current):
        spec = car(current)
        if isinstance(spec, lisptype.LispSymbol):
            var_specs.append((spec, lisptype.NIL, None))
        elif _consp_internal(spec):
            var = car(spec)
            init_form = car(cdr(spec)) if _consp_internal(cdr(spec)) else lisptype.NIL
            step_form = car(cdr(cdr(spec))) if _consp_internal(cdr(cdr(spec))) else None
            var_specs.append((var, init_form, step_form))
        current = cdr(current)
    
    # Evaluate all init forms first (parallel binding like LET)
    init_values = [eval(init_form, env) for var, init_form, _ in var_specs]
    
    # Bind variables
    for (var, _, _), value in zip(var_specs, init_values):
        loop_env.set_variable(var, value)
    
    # Main loop, watched for runaway iteration.
    watchdog = LoopWatchdog(
        'DO',
        lambda: [f"end_test: {end_test}", f"var_specs: {var_specs}"])

    def _loop():
        while True:
            watchdog.tick()

            # Check end-test
            if lisptype.is_truthy(eval(end_test, loop_env)):
                # Evaluate result forms and return last value
                result = lisptype.NIL
                current = result_forms
                while _consp_internal(current):
                    result = eval(car(current), loop_env)
                    current = cdr(current)
                return result

            # Execute body
            _exec_iteration_body(body, loop_env)

            # Update variables (evaluate all step forms first, then update)
            new_values = []
            for var, _, step_form in var_specs:
                if step_form is not None:
                    new_values.append((var, eval(step_form, loop_env)))

            for var, value in new_values:
                loop_env.set_variable(var, value)

    with watchdog:
        return _run_with_nil_block(_loop)


def eval_do_star(form, env):
    """Evaluate DO* special form.
    
    (DO* ((var init [step])*)
         (end-test result-form*)
         declaration*
         {tag | statement}*)
    
    DO* evaluates init forms sequentially (like LET*) and binds variables.
    """
    from .evaluation_core import eval
    
    args = cdr(form)
    if not _consp_internal(args):
        return lisptype.NIL
    
    # Parse variable bindings
    var_list = car(args)
    args = cdr(args)
    
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("DO* requires end-test clause")
    
    # Parse end-test clause  
    end_clause = car(args)
    body = cdr(args)
    
    if not _consp_internal(end_clause):
        raise lisptype.LispNotImplementedError("DO* end-test must be a list")
    
    end_test = car(end_clause)
    result_forms = cdr(end_clause)
    
    # Create new environment
    loop_env = lisptype.Environment(env)
    
    # Parse var specs and evaluate init forms SEQUENTIALLY (like LET*)
    var_specs = []
    current = var_list
    while _consp_internal(current):
        spec = car(current)
        if isinstance(spec, lisptype.LispSymbol):
            loop_env.set_variable(spec, lisptype.NIL)
            var_specs.append((spec, None))
        elif _consp_internal(spec):
            var = car(spec)
            init_form = car(cdr(spec)) if _consp_internal(cdr(spec)) else lisptype.NIL
            step_form = car(cdr(cdr(spec))) if _consp_internal(cdr(cdr(spec))) else None
            # Evaluate in current loop_env (sequential)
            init_value = eval(init_form, loop_env)
            loop_env.set_variable(var, init_value)
            var_specs.append((var, step_form))
        current = cdr(current)
    
    # Main loop with timeout warning
    watchdog = LoopWatchdog(
        'DO*',
        lambda: [f"end_test: {end_test}", f"var_specs: {var_specs}"])

    def _loop():
        while True:
            watchdog.tick()

            # Check end-test
            if lisptype.is_truthy(eval(end_test, loop_env)):
                # Evaluate result forms and return last value
                result = lisptype.NIL
                current = result_forms
                while _consp_internal(current):
                    result = eval(car(current), loop_env)
                    current = cdr(current)
                return result

            # Execute body
            _exec_iteration_body(body, loop_env)

            # Update variables sequentially
            for var, step_form in var_specs:
                if step_form is not None:
                    new_value = eval(step_form, loop_env)
                    loop_env.set_variable(var, new_value)

    with watchdog:
        return _run_with_nil_block(_loop)


def eval_dolist(form, env):
    """Evaluate DOLIST special form.
    
    (DOLIST (var list-form [result-form]) declaration* {tag | statement}*)
    """
    from .evaluation_core import eval
    
    args = cdr(form)
    if not _consp_internal(args):
        return lisptype.NIL
    
    # Parse (var list-form [result-form])
    var_clause = car(args)
    body = cdr(args)
    
    if not _consp_internal(var_clause):
        raise lisptype.LispNotImplementedError("DOLIST requires (var list-form) clause")
    
    var = car(var_clause)
    list_form = car(cdr(var_clause)) if _consp_internal(cdr(var_clause)) else lisptype.NIL
    result_form = car(cdr(cdr(var_clause))) if _consp_internal(cdr(cdr(var_clause))) else lisptype.NIL
    
    # Evaluate list
    lst = eval(list_form, env)
    
    # Create loop environment
    loop_env = lisptype.Environment(env)
    loop_env.set_variable(var, lisptype.NIL)

    def _loop():
        # Iterate over list
        current_list = lst
        while _consp_internal(current_list):
            loop_env.set_variable(var, car(current_list))

            # Execute body
            _exec_iteration_body(body, loop_env)

            current_list = cdr(current_list)

        # Set var to NIL for result form
        loop_env.set_variable(var, lisptype.NIL)

        # Evaluate and return result form
        return eval(result_form, loop_env)

    return _run_with_nil_block(_loop)


def eval_dotimes(form, env):
    """Evaluate DOTIMES special form.
    
    (DOTIMES (var count-form [result-form]) declaration* {tag | statement}*)
    """
    from .evaluation_core import eval
    
    args = cdr(form)
    if not _consp_internal(args):
        return lisptype.NIL
    
    # Parse (var count-form [result-form])
    var_clause = car(args)
    body = cdr(args)
    
    if not _consp_internal(var_clause):
        raise lisptype.LispNotImplementedError("DOTIMES requires (var count-form) clause")
    
    var = car(var_clause)
    count_form = car(cdr(var_clause)) if _consp_internal(cdr(var_clause)) else 0
    result_form = car(cdr(cdr(var_clause))) if _consp_internal(cdr(cdr(var_clause))) else lisptype.NIL
    
    # Evaluate count
    count = eval(count_form, env)
    if not isinstance(count, (int, float)):
        count = 0
    count = int(count)
    
    # Create loop environment
    loop_env = lisptype.Environment(env)

    def _loop():
        # Iterate count times
        for i in range(count):
            loop_env.set_variable(var, i)

            # Execute body
            _exec_iteration_body(body, loop_env)

        # Set var to count for result form
        loop_env.set_variable(var, count)

        # Evaluate and return result form
        return eval(result_form, loop_env)

    return _run_with_nil_block(_loop)


def eval_do_symbols(form, env):
    """Evaluate DO-SYMBOLS special form.
    
    (DO-SYMBOLS (var [package [result-form]]) declaration* {tag | statement}*)
    
    Iterates over all symbols accessible in the package.
    """
    from .evaluation_core import eval
    import fclpy.state as state
    
    args = cdr(form)
    if not _consp_internal(args):
        return lisptype.NIL
    
    # Parse (var [package [result-form]])
    var_clause = car(args)
    body = cdr(args)
    
    if not _consp_internal(var_clause):
        raise lisptype.LispNotImplementedError("DO-SYMBOLS requires (var [package]) clause")
    
    var = car(var_clause)
    rest = cdr(var_clause)
    package_form = car(rest) if _consp_internal(rest) else lisptype.NIL
    result_form = car(cdr(rest)) if _consp_internal(rest) and _consp_internal(cdr(rest)) else lisptype.NIL
    
    # Get the package
    if package_form is None or package_form == lisptype.NIL:
        pkg = getattr(state, 'current_package', lisptype.COMMON_LISP_USER_PACKAGE)
    else:
        pkg_designator = eval(package_form, env)
        if isinstance(pkg_designator, lisptype.Package):
            pkg = pkg_designator
        else:
            pkg = lisptype.find_package(str(pkg_designator))
        if pkg is None:
            raise lisptype.LispError(f"Package not found: {pkg_designator}")
    
    # Create loop environment
    loop_env = lisptype.Environment(env)
    loop_env.set_variable(var, lisptype.NIL)

    def _loop():
        # Iterate over all symbols in package (internal + external)
        for name, sym in list(pkg.symbols.items()):
            loop_env.set_variable(var, sym)
            _exec_iteration_body(body, loop_env)

        # Also iterate over inherited symbols from used packages
        for used_pkg in getattr(pkg, 'use_list', []):
            if used_pkg is not None:
                external_names = getattr(used_pkg, 'external_symbols', set())
                for item in list(external_names):
                    # Handle both string names and LispSymbol objects
                    if isinstance(item, lisptype.LispSymbol):
                        sym = item
                    else:
                        sym = used_pkg.symbols.get(item)
                    if sym is not None:
                        loop_env.set_variable(var, sym)
                        _exec_iteration_body(body, loop_env)

        # Set var to NIL for result form
        loop_env.set_variable(var, lisptype.NIL)
        return eval(result_form, loop_env)

    return _run_with_nil_block(_loop)


def eval_do_external_symbols(form, env):
    """Evaluate DO-EXTERNAL-SYMBOLS special form.
    
    (DO-EXTERNAL-SYMBOLS (var [package [result-form]]) declaration* {tag | statement}*)
    
    Iterates over all external (exported) symbols in the package.
    """
    from .evaluation_core import eval
    import fclpy.state as state
    
    args = cdr(form)
    if not _consp_internal(args):
        return lisptype.NIL
    
    # Parse (var [package [result-form]])
    var_clause = car(args)
    body = cdr(args)
    
    if not _consp_internal(var_clause):
        raise lisptype.LispNotImplementedError("DO-EXTERNAL-SYMBOLS requires (var [package]) clause")
    
    var = car(var_clause)
    rest = cdr(var_clause)
    package_form = car(rest) if _consp_internal(rest) else lisptype.NIL
    result_form = car(cdr(rest)) if _consp_internal(rest) and _consp_internal(cdr(rest)) else lisptype.NIL
    
    # Get the package
    if package_form is None or package_form == lisptype.NIL:
        pkg = getattr(state, 'current_package', lisptype.COMMON_LISP_USER_PACKAGE)
    else:
        pkg_designator = eval(package_form, env)
        if isinstance(pkg_designator, lisptype.Package):
            pkg = pkg_designator
        else:
            pkg = lisptype.find_package(str(pkg_designator))
        if pkg is None:
            raise lisptype.LispError(f"Package not found: {pkg_designator}")
    
    # Create loop environment
    loop_env = lisptype.Environment(env)
    loop_env.set_variable(var, lisptype.NIL)

    def _loop():
        # Iterate over external symbols only
        # Note: external_symbols may contain strings (symbol names) or LispSymbol objects
        external_names = getattr(pkg, 'external_symbols', set())
        for item in list(external_names):
            # Handle both string names and LispSymbol objects
            if isinstance(item, lisptype.LispSymbol):
                sym = item
            else:
                # It's a string name, look up the symbol
                sym = pkg.symbols.get(item)
            if sym is not None:
                loop_env.set_variable(var, sym)
                _exec_iteration_body(body, loop_env)

        # Set var to NIL for result form
        loop_env.set_variable(var, lisptype.NIL)
        return eval(result_form, loop_env)

    return _run_with_nil_block(_loop)


def eval_do_all_symbols(form, env):
    """Evaluate DO-ALL-SYMBOLS special form.
    
    (DO-ALL-SYMBOLS (var [result-form]) declaration* {tag | statement}*)
    
    Iterates over all symbols in all registered packages.
    """
    from .evaluation_core import eval
    import fclpy.state as state
    
    args = cdr(form)
    if not _consp_internal(args):
        return lisptype.NIL
    
    # Parse (var [result-form])
    var_clause = car(args)
    body = cdr(args)
    
    if not _consp_internal(var_clause):
        raise lisptype.LispNotImplementedError("DO-ALL-SYMBOLS requires (var) clause")
    
    var = car(var_clause)
    result_form = car(cdr(var_clause)) if _consp_internal(cdr(var_clause)) else lisptype.NIL
    
    # Create loop environment
    loop_env = lisptype.Environment(env)
    loop_env.set_variable(var, lisptype.NIL)

    def _loop():
        # Get all unique packages
        unique_packages = {id(p): p for p in state.packages.values()}

        # Iterate over all symbols in all packages
        for pkg in unique_packages.values():
            for name, sym in list(pkg.symbols.items()):
                loop_env.set_variable(var, sym)
                _exec_iteration_body(body, loop_env)

        # Set var to NIL for result form
        loop_env.set_variable(var, lisptype.NIL)
        return eval(result_form, loop_env)

    return _run_with_nil_block(_loop)


__all__ = [
    'eval_when',
    'eval_unless',
    'eval_cond',
    'eval_case',
    'eval_and',
    'eval_or',
    'eval_progn',
    'eval_locally',
    'eval_let',
    'eval_letstar',
    'eval_quasiquote',
    'eval_prog1',
    'eval_prog2',
    'eval_time',
    'eval_loop',
    'eval_do',
    'eval_do_star',
    'eval_dolist',
    'eval_dotimes',
    'eval_do_symbols',
    'eval_do_external_symbols',
    'eval_do_all_symbols',
]
