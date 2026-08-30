"""Loops and conditionals: WHEN, COND, AND, OR, PROGN, LET, quasiquote."""

import fclpy.lisptype as lisptype
from .core import car, cdr, _consp_internal, _null_internal, cons, _check_list
from .evaluation_core import ThrowException
from .math_arithmetic import _s_plus_ as _lisp_plus


class LoopFinishException(ThrowException):
    """Exception raised by LOOP-FINISH to terminate the loop immediately.

    This is a control flow mechanism subclassed from ThrowException so it
    propagates through all eval pass-through tuples automatically (CLHS 6.1.5).
    """
    def __init__(self):
        # LoopFinishException doesn't use tag/value like ThrowException,
        # but we need to initialize the parent. Use sentinel values.
        super().__init__(tag=None, value=None)


def _list_from(elements):
    """A Lisp list from a Python sequence -- NIL when empty."""
    result = lisptype.NIL
    for element in reversed(list(elements)):
        result = cons(element, result)
    return result
from . import registry as _registry
from .binding import BindingFrame, body_specials, special_reference
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
    'MAXIMIZE': 'maximize', 'MAXIMIZING': 'maximize',
    'MINIMIZE': 'minimize', 'MINIMIZING': 'minimize',
    'ALWAYS': 'always',
    'NEVER': 'never',
    'THEREIS': 'thereis',
}

# The accumulations CLHS 6.1.3.2 gives a numeric result, and the only ones whose
# grammar ends in an optional type-spec: `maximize x fixnum` is a type
# declaration, but `collect x` takes no such trailing token, so parsing one
# there would swallow the next clause's first form.
NUMERIC_ACCUMULATIONS = frozenset({'sum', 'count', 'maximize', 'minimize'})

# ALWAYS/NEVER/THEREIS (CLHS 6.1.2.2) do not accumulate: each one *decides* the
# value of the whole LOOP and terminates it immediately, skipping the epilogue.
# They are parsed through the accumulation table because their syntax is the
# same, but they are executed as one shared early-decision, not three flags.
BOOLEAN_TERMINATION_CLAUSES = frozenset({'always', 'never', 'thereis'})

# Every token that begins a new LOOP clause, and therefore ends the one being
# parsed. Single source of truth: this used to be two hand-maintained copies
# (the FOR-clause scanner and the DO body scanner) that could drift apart.
LOOP_CLAUSE_KEYWORDS = frozenset(ACCUMULATION_CLAUSES) | {
    'FOR', 'AS', 'WITH', 'AND', 'WHILE', 'UNTIL', 'REPEAT', 'DO', 'DOING',
    'WHEN', 'UNLESS', 'IF', 'RETURN', 'INITIALLY', 'FINALLY',
}

# The two `for var being ...` families, and every spelling CLHS gives them.
# Singular and plural are synonyms (6.1.2.1.6, 6.1.2.1.7), which is most of why
# the previous parse -- which matched only the plural SYMBOLS -- recognized one
# of these eight tokens.
LOOP_HASH_PARTS = {
    'HASH-KEY': 'key', 'HASH-KEYS': 'key',
    'HASH-VALUE': 'value', 'HASH-VALUES': 'value',
}

LOOP_PACKAGE_SYMBOL_SETS = {
    'SYMBOL': 'symbols', 'SYMBOLS': 'symbols',
    'PRESENT-SYMBOL': 'present-symbols', 'PRESENT-SYMBOLS': 'present-symbols',
    'EXTERNAL-SYMBOL': 'external-symbols', 'EXTERNAL-SYMBOLS': 'external-symbols',
}

# CLHS 6.1.1.7's `simple-type-spec`: the only type specifiers that may follow a
# LOOP variable *without* the OF-TYPE marker. Any richer specifier -- (fixnum
# fixnum), string, (integer 0 7) -- must be introduced by OF-TYPE, which is what
# makes it safe to treat every other token after a variable as the next clause.
LOOP_SIMPLE_TYPE_SPECS = frozenset({'FIXNUM', 'FLOAT', 'T', 'NIL'})

# The zero of each type, for a LOOP variable given a type-spec but no init form
# (CLHS 6.1.1.7: "bound to an appropriate value for its type, such as 0").
_LOOP_ZERO_BY_TYPE = {
    'FIXNUM': 0, 'INTEGER': 0, 'BIGNUM': 0, 'BIT': 0, 'RATIONAL': 0,
    'RATIO': 0, 'NUMBER': 0, 'SIGNED-BYTE': 0, 'UNSIGNED-BYTE': 0,
    'FLOAT': 0.0, 'SHORT-FLOAT': 0.0, 'SINGLE-FLOAT': 0.0,
    'DOUBLE-FLOAT': 0.0, 'LONG-FLOAT': 0.0, 'REAL': 0.0,
}


def _loop_sym_name(token):
    """Uppercase name of a LOOP token that is a symbol, else None.

    Shared by the clause parser and by the type-spec/variable helpers below so
    that "is this token the keyword X" has exactly one answer. NIL reads as the
    ``lispNull`` singleton rather than a ``LispSymbol``, so it needs its own
    branch -- without it ``with nil = nil`` and the ``nil`` simple-type-spec
    both look like anonymous non-symbol tokens.
    """
    if isinstance(token, lisptype.LispSymbol):
        return token.name.upper()
    if token is lisptype.NIL or token is None:
        return 'NIL'
    return None


def _loop_is_discarded_var(varspec):
    """True for the NIL that means "bind nothing here" (CLHS 6.1.1.7).

    ``(loop for nil being the hash-values of h count t)`` and
    ``(loop with (nil a) = '(1 2) return a)`` both use NIL as a placeholder
    whose value is dropped. NIL has three representations in this
    implementation, so all three have to be recognized.
    """
    return varspec is None or varspec is lisptype.NIL or (
        isinstance(varspec, lisptype.LispSymbol) and varspec.name.upper() == 'NIL')


def _loop_destructure(varspec, value, visit):
    """Walk a LOOP variable spec against a value, calling visit(symbol, value).

    One recursive walk over the cons structure replaces the three special-cased
    shapes this used to enumerate (single symbol, dotted pair ``(a . b)``,
    proper list ``(a b c)``). The general walk is what makes the shapes the
    enumeration missed work: a dotted tail ``(a b . rest)``, a NIL hole
    ``(nil . v)``, and a pattern longer than its value -- CLHS 6.1.1.7 fills
    the missing positions with NIL rather than erring, which
    ``(loop with (a b) = '(1) ...)`` relies on.

    It is shared by WITH, by every FOR driver and by USING, so all of them
    destructure identically; three copies of a partial walk is how they came to
    disagree.
    """
    if _loop_is_discarded_var(varspec):
        return
    if isinstance(varspec, lisptype.LispSymbol):
        visit(varspec, value)
        return
    if _consp_internal(varspec):
        has_value = _consp_internal(value)
        _loop_destructure(car(varspec), car(value) if has_value else lisptype.NIL, visit)
        _loop_destructure(cdr(varspec), cdr(value) if has_value else lisptype.NIL, visit)
        return
    raise lisptype.LispProgramError(
        f'LOOP variable must be a symbol or a destructuring pattern, not {varspec!r}')


def _loop_varspec_names(varspec):
    """The names a variable spec binds, in order -- CLHS 6.1.1.7's duplicate
    check needs the names *found by destructuring*, not just the top-level one."""
    names = []
    _loop_destructure(varspec, lisptype.NIL, lambda sym, _value: names.append(sym.name.upper()))
    return names


def validate_loop_form(form):
    """Expansion-time duplicate-variable pre-check (CLHS 6.1.1.7): "an error
    of type program-error is signaled (at macro expansion time) if the same
    variable is bound twice in any variable-binding clause of a single loop
    expression."

    The engine's `_claim_variables` enforces the same rule while the loop
    parses at evaluation time; this pre-check exists because MACROEXPAND must
    never run the program, yet the standard pins the PROGRAM-ERROR to
    expansion time (LOOP.4.7/.4.8, LOOP.5.ERROR.3/.4 macroexpand and expect
    the signal without ever evaluating the loop).

    Deliberately a *conservative pre-check*, not a second parser: it claims
    variables only from the token positions that are unambiguously binding
    clauses at the top level of the clause list (FOR/AS and their AND-joined
    subclauses, WITH), extracting names through the same `_loop_varspec_names`
    the engine's claim uses, so there is exactly one name-extraction rule.
    Anything the scan is unsure about is left for the engine's own check at
    evaluation time."""
    tokens = []
    current = cdr(form) if _consp_internal(form) else lisptype.NIL
    while _consp_internal(current):
        tokens.append(car(current))
        current = cdr(current)

    def _keyword_name(tok):
        name = getattr(tok, 'name', None)
        return name.upper() if isinstance(name, str) else None

    claimed = []

    def _claim(spec):
        for var_name in _loop_varspec_names(spec):
            if var_name in claimed:
                # ERROR semantics through the conditions system (not a bare
                # LispProgramError raise): the macroexpansion-time signal
                # must be a real program-error CONDITION so signals-error's
                # handler and RT's own error handler match it by type.
                from .evaluation_conditions import signal_error_object
                signal_error_object(lisptype.ProgramError(
                    message=f'LOOP binds {var_name} twice'))
            claimed.append(var_name)

    i = 0
    n = len(tokens)
    in_for_group = False
    while i < n:
        uname = _keyword_name(tokens[i])
        if uname in ('FOR', 'AS'):
            in_for_group = True
            i += 1
            if i < n:
                _claim(tokens[i])
                i += 1
            continue
        if in_for_group and uname == 'AND':
            nxt = _keyword_name(tokens[i + 1]) if i + 1 < n else None
            if nxt in ('FOR', 'AS'):
                i += 1  # `and for x ...` spelling -- let the FOR branch claim
                continue
            i += 1
            if i < n:
                _claim(tokens[i])
                i += 1
            continue
        if uname == 'WITH':
            i += 1
            if i < n:
                _claim(tokens[i])
                i += 1
            continue
        i += 1


def _loop_type_spec(forms, index):
    """Consume the optional type-spec at forms[index] (CLHS 6.1.1.7).

    Returns ``(next_index, spec)``; ``spec`` is None when there is no type-spec,
    in which case the index is unchanged. Deciding this in one place is what
    keeps FOR, WITH and the numeric accumulations from each guessing
    differently -- and the guess has to be conservative, because consuming a
    token that is really the next clause's keyword silently deletes that clause.
    """
    if index >= len(forms):
        return index, None
    name = _loop_sym_name(forms[index])
    if name == 'OF-TYPE':
        if index + 1 >= len(forms):
            raise lisptype.LispProgramError('LOOP OF-TYPE requires a type specifier')
        return index + 2, forms[index + 1]
    if name in LOOP_SIMPLE_TYPE_SPECS:
        return index + 1, forms[index]
    return index, None


def _loop_type_default(spec):
    """The initial value a type-spec implies when no init form is given.

    CLHS 6.1.1.7: a numeric type starts at its zero and everything else at NIL,
    and a *destructured* type-spec supplies one default per position -- which is
    why this mirrors the spec's own cons structure and hands the result to
    `_loop_destructure`, rather than producing a single scalar.
    """
    if _consp_internal(spec):
        return cons(_loop_type_default(car(spec)), _loop_type_default(cdr(spec)))
    return _LOOP_ZERO_BY_TYPE.get(_loop_sym_name(spec), lisptype.NIL)


def _validate_proper_list(value, context):
    """Ensure a value is a proper list (CLHS 14.1), not dotted (e.g., (A . B)).

    CLHS 6.1.2.1.3 specifies that FOR x IN requires a proper list.
    A proper list terminates in NIL; a dotted list has a non-NIL, non-cons tail.
    Raises TYPE-ERROR if the value is not a list or is a dotted list.
    """
    # First check that it's a list at all (cons or NIL)
    _check_list(value, context)

    # Now walk the list to ensure it's proper (ends in NIL, not dotted)
    current = value
    while _consp_internal(current):
        tail = current.cdr
        if _null_internal(tail):
            # Proper termination in NIL
            return value
        if not _consp_internal(tail):
            # Dotted list: tail is neither cons nor NIL
            raise lisptype.LispTypeError(
                f"{context}: {value!r} is not a proper list",
                expected_type="proper LIST", actual_value=value)
        current = tail
    return value


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
    - :COMPILE-TOPLEVEL (or COMPILE) - compile-file-time processing only
    - :LOAD-TOPLEVEL (or LOAD) - the load of a *compiled* file only
    - :EXECUTE (or EVAL) - whenever the form is evaluated
    
    CLHS 3.2.3.1's Figure 3-7 decides what each situation means, and for
    the interpreter the operative column is E: evaluating the form runs the
    body iff the situations include :EXECUTE (or EVAL). :LOAD-TOPLEVEL
    belongs to the load of a compiled file -- which COMPILE-FILE arranges
    for by emitting the body forms themselves -- so an `(eval-when
    (:load-toplevel) ...)` sitting in interpreted code evaluates to NIL
    and runs nothing (eval-when.6/.9/.12/.15).
    """
    from .evaluation_core import eval
    
    args = cdr(form)
    if not _consp_internal(args):
        return lisptype.NIL
    
    situations = car(args)
    body = cdr(args)
    
    # The interpreter is the eval situation: only :EXECUTE (or EVAL) applies.
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
        
        if sit_name in (':EXECUTE', 'EXECUTE', 'EVAL'):
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
            test_value = eval(car(clause), env)
            if lisptype.is_truthy(test_value):
                forms = cdr(clause)
                if not _consp_internal(forms):
                    # CLHS 5.3 COND: a clause with no forms answers the
                    # *value* of its test. It used to answer the test's
                    # unevaluated **form** -- `(cond ((+ 1 2)))` was the list
                    # `(+ 1 2)`, not 3 -- which is a Lisp form appearing as a
                    # Lisp value. ansi-test's own `make-array-with-checks`
                    # (and every aux helper written as one long `cond` of
                    # test-only clauses) returns exactly that shape, so the
                    # harness reported the check's source text where a result
                    # was expected and no test using one could pass.
                    return _primary_value(test_value)
                result = lisptype.NIL
                while _consp_internal(forms):
                    result = eval(car(forms), env)
                    forms = cdr(forms)
                return result

        clauses = cdr(clauses)

    # No clause was selected: COND answers NIL (CLHS 5.3), not Python None.
    return lisptype.NIL


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
    """Evaluate AND special form (CLHS 5.1).

    CLHS: "If [a form] is the last form, AND returns the values returned by
    that form, whatever they are" -- the last form's result is returned
    exactly, not reduced to NIL/T first. Only the non-last forms use the
    single-value truthiness test to decide whether to short-circuit.
    `(and (values))` must answer zero values and `(and 1 (values nil 2))`
    must answer NIL *and* 2 (AND.5/AND.8) -- checking every form's
    truthiness, including the last, discarded the last form's real
    value(s) whenever its primary value happened to be NIL.
    """
    from .evaluation_core import eval

    args = cdr(form)
    if not _consp_internal(args):
        return lisptype.T  # AND with no arguments is T

    while True:
        current = car(args)
        rest = cdr(args)
        result = eval(current, env)
        if not _consp_internal(rest):
            return result
        if not lisptype.is_truthy(result):
            return lisptype.NIL
        args = rest


def eval_or(form, env):
    """Evaluate OR special form (CLHS 5.1).

    Same last-form exception as AND: the last form's value(s) are returned
    exactly, whatever they are, rather than being reduced to NIL when its
    primary value is falsy.
    """
    from .evaluation_core import eval

    args = cdr(form)
    if not _consp_internal(args):
        return lisptype.NIL  # OR with no arguments is NIL

    while True:
        current = car(args)
        rest = cdr(args)
        result = eval(current, env)
        if not _consp_internal(rest):
            return result
        if lisptype.is_truthy(result):
            return result
        args = rest


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
    lexical binding. Since LOCALLY binds nothing, *every* special declaration
    here is a free one, which is the same case `BindingFrame` handles for a
    binding form's free declarations -- so both use `special_reference`.
    """
    from .evaluation_core import eval

    special_vars, args = body_specials(cdr(form))
    result = lisptype.NIL

    if special_vars:
        body_env = lisptype.Environment(parent=env)
        for var in special_vars:
            body_env.add_symbol_macro(var, special_reference(var))
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
    
    # Now bind all variables in the new environment. Whether each one is bound
    # lexically or dynamically is `BindingFrame`'s decision, shared with LET*
    # and all eight iteration forms -- see fclpy/lispfunc/binding.py.
    import fclpy.state as state
    frame = BindingFrame(let_env, body=body,
                         bound_vars=[var for var, _ in bindings_list])
    for var, value in bindings_list:
        if isinstance(var, lisptype.LispSymbol):
            frame.bind(var, value)

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
        frame.unwind()


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

    import fclpy.state as state

    # LET*'s copy of the special-vs-lexical decision was not merely duplicated,
    # it was wrong: a special variable was bound with
    # `global_env.add_variable`, which puts a *lexical* binding in the global
    # environment and never removes it, so the binding outlived the LET* and
    # was invisible to SYMBOL-VALUE. One shared `BindingFrame` now decides for
    # LET, LET* and the eight iteration forms alike.
    bound_vars = []
    current = bindings_form
    while _consp_internal(current):
        binding = car(current)
        bound_vars.append(car(binding) if _consp_internal(binding) else binding)
        current = cdr(current)
    # LET* evaluates each init form in `letstar_env`, so a free special declaration
    # must not be installed until the inits are done -- like DO*.16 and
    # `install_free_declarations`.
    frame = BindingFrame(letstar_env, body=body, bound_vars=bound_vars,
                         defer_free_declarations=True)
    _, body = body_specials(body)

    # Process bindings sequentially - each can see previous ones
    current = bindings_form
    while _consp_internal(current):
        binding = car(current)
        if _consp_internal(binding):
            var = car(binding)
            init_form = car(cdr(binding))
            # Evaluate init in CURRENT environment (with previous bindings)
            value = eval(init_form, letstar_env)
        else:
            # A bare symbol binds to NIL (CLHS 3.1.2.1.1), as it does in LET.
            # LET* skipped these entirely, so the variable was left unbound.
            var = binding
            value = lisptype.NIL
        if isinstance(var, lisptype.LispSymbol):
            frame.bind(var, value)
        current = cdr(current)

    # The inits are done; the body *is* in the scope of the body's declarations.
    frame.install_free_declarations()

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
        frame.unwind()


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
    for func_name, closure in _local_function_definitions(bindings_form, env, 'FLET'):
        flet_env.add_function(func_name, closure)

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
    for func_name, closure in _local_function_definitions(bindings_form, labels_env, 'LABELS'):
        labels_env.add_function(func_name, closure)

    # Evaluate body in environment with local function bindings
    result = lisptype.NIL
    current = body
    while _consp_internal(current):
        result = eval(car(current), labels_env)
        current = cdr(current)
    
    return result


def _local_function_definitions(bindings_form, definition_env, operator):
    """Yield ``(storage-symbol, callable)`` for each FLET/LABELS binding.

    FLET and LABELS differ in exactly one thing -- the environment the
    closures capture -- so that is the parameter, and everything else is
    shared. In particular the callable is built by the *same*
    `make_ordinary_function` that LAMBDA and DEFUN use.

    It previously was not: `make_lambda_closure` lived here with its own
    hand-rolled lambda-list parser that did not go through
    `parse_lambda_list`, dropped every supplied-p variable, ignored `&aux`
    and `&allow-other-keys` outright (two literal ``pass`` branches), and
    signalled no error for any malformed call. A local function was
    therefore a materially different kind of function from a global one,
    which is not a distinction Common Lisp makes.

    The function name goes through `function_name_parts`, so ``(setf %f)``
    and NIL are names like any other; testing `isinstance(name, LispSymbol)`
    and skipping everything else defined nothing at all for those two.
    """
    from .evaluation_special_forms import make_ordinary_function, function_name_parts

    current = bindings_form
    while _consp_internal(current):
        binding = car(current)
        if not _consp_internal(binding):
            raise lisptype.LispProgramError(
                f"{operator}: each binding must be (name lambda-list . body), not {binding!r}")
        name_spec = car(binding)
        rest = cdr(binding)
        if not _consp_internal(rest):
            raise lisptype.LispProgramError(
                f"{operator}: {name_spec!r} has no lambda list")
        storage_symbol, block_name = function_name_parts(name_spec, operator)
        # CLHS 3.1.2.1.2.2: the body of a local function is enclosed in an
        # implicit block named by the function, exactly as DEFUN's is.
        yield storage_symbol, make_ordinary_function(
            car(rest), cdr(rest), definition_env,
            block_name=block_name, name=storage_symbol)
        current = cdr(current)



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
                # At outermost level, evaluate the unquote. The template is
                # a *single-value* context: `(quote ,(macroexpand x env))`
                # contributes MACROEXPAND's primary value only -- the
                # MultipleValues wrapper itself became the constructed
                # form's second value and surfaced as a bogus extra return
                # value of every macro whose body unquoted a multi-valued
                # call (defmacro.17/.17A).
                return lisptype.primary_value(eval(car(cdr(obj)), env))
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

        # Otherwise obj is a cons/list: build a resulting list applying unquote
        # rules. `tail` is what the result's final cdr must be -- NIL for a
        # proper template, the (quasiquoted) terminator for a dotted one. The
        # loop used to end at a non-cons `cur` and build onto NIL, silently
        # **dropping** the dotted tail: `` `(a . d) `` answered `(A)`, and
        # ansi-test's `` `((a b) c) (,x . d)) `` idiom -- the standard way it
        # builds alists -- lost every association's value (`assoc.11`).
        parts = []
        cur = obj
        tail = lisptype.NIL
        while _consp_internal(cur):
            # `` `(a . ,x) `` reads as the *proper* list `(A UNQUOTE X)`,
            # because `. (unquote x)` is just `unquote x`. So an UNQUOTE
            # *symbol* in a car position -- as opposed to a cons whose car is
            # UNQUOTE, which is an element -- marks the rest of the template as
            # the dotted tail rather than as two more elements.
            head = car(cur)
            if (isinstance(head, lisptype.LispSymbol) and head.name == 'UNQUOTE'
                    and _consp_internal(cdr(cur))):
                if level == 1:
                    tail = lisptype.primary_value(eval(car(cdr(cur)), env))
                else:
                    tail = cons(head, cons(_quasi(car(cdr(cur)), level - 1),
                                           lisptype.NIL))
                cur = lisptype.NIL
                break
            item = head
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
                        # At outermost level, evaluate. Single-value context
                        # -- the element contributes MACROEXPAND-1's primary
                        # value only (defmacro.17/.17A).
                        parts.append(
                            lisptype.primary_value(eval(car(cdr(item)), env)))
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
        else:
            # Fell off a dotted template: its terminator is quasiquoted like
            # any other subform and becomes the result's final cdr.
            if not _null_internal(cur):
                tail = _quasi(cur, level)

        res = tail
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


# PROG/PROG* are now real macros -- see standard_macros.py's
# `_build_prog_expansion` (CLHS 5.3's own expansion into BLOCK/LET[*]/
# TAGBODY), which replaced the special-form version that used to live
# here.


def eval_time(form, env):
    """TIME (CLHS 25.1.3): evaluate `form`, report timing, return its values.

    Two things here are the specification rather than presentation.

    **The report goes to `*TRACE-OUTPUT*`**, through `io_write.write_text` --
    the one place text is written to a Lisp stream. It used to go to Python's
    `sys.stderr` with `print()`, which is not a Lisp stream at all, so
    `(with-output-to-string (*trace-output*) (time nil))` captured the empty
    string and all eight of `environment/time.lsp`'s tests -- every one of
    which asserts the captured string is *non*-empty -- could not pass however
    TIME behaved. The same defect the printer had before
    `resolve_output_stream` existed.

    **All of the form's values are returned**, not just the primary one: the
    result of `eval` is passed straight through, so `(time (values))` yields no
    values and `(time (values 'a 'b 'c 'd))` yields four.

    A missing subform is a PROGRAM-ERROR rather than a quiet NIL -- `(time)` is
    not a legal form, and answering NIL for it makes a malformed program look
    like a working one (standing rule 4).
    """
    from .evaluation_core import eval
    from .io_write import write_text
    from .binding import dynamic_value

    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispProgramError("TIME requires exactly one form")
    if _consp_internal(cdr(args)):
        raise lisptype.LispProgramError(
            "TIME requires exactly one form, not several")

    form_to_time = car(args)

    start_real = time.perf_counter()
    start_cpu = time.process_time()
    try:
        return eval(form_to_time, env)
    finally:
        # In a `finally`, so a non-local exit out of the timed form -- a
        # RETURN-FROM, a THROW, a signalled condition -- still reports. Both
        # clocks are monotonic, so an elapsed time is never negative.
        real_elapsed = time.perf_counter() - start_real
        cpu_elapsed = time.process_time() - start_cpu
        trace_output = dynamic_value(
            lisptype.COMMON_LISP_PACKAGE.intern_symbol('*TRACE-OUTPUT*'))
        write_text(
            f"Evaluation took:\n"
            f"  {real_elapsed:.6f} seconds of real time\n"
            f"  {cpu_elapsed:.6f} seconds of total run time\n",
            trace_output)


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
    
    sym_name = _loop_sym_name

    def _bind_varspec(frame, varspec, value):
        """Bind a LOOP var spec (symbol or destructuring pattern).

        Goes through the loop's `BindingFrame`, which *establishes* the variable
        on the first iteration and assigns to that same binding afterwards. It
        used to call `set_variable`, which walks out to an enclosing binding of
        the same name and mutates it -- so `(loop for s = ...)` overwrote a
        caller's `s`, rt.lsp's report stream among them.

        The pattern walk itself is `_loop_destructure`, shared with WITH and
        USING so every clause destructures the same way.
        """
        _loop_destructure(varspec, value, frame.bind)

    # CLHS 6.1.1.7: "An error of type program-error is signaled (at macro
    # expansion time) if the same variable is bound twice in any
    # variable-binding clause of a single loop expression." Collected across
    # every binding clause, including the names destructuring finds.
    bound_variable_names = []

    def _claim_variables(varspec):
        for var_name in _loop_varspec_names(varspec):
            if var_name in bound_variable_names:
                raise lisptype.LispProgramError(
                    f'LOOP binds {var_name} twice')
            bound_variable_names.append(var_name)

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

    # The same drivers grouped by their FOR clause (CLHS 6.1.2.1). A plain
    # clause is a singleton group; the subclauses one AND joins (`for i from 1
    # to 5 and j = 0 then (+ j i)`) share one group and initialize and step in
    # parallel -- all their bound forms are evaluated before any of the
    # group's variables is bound, and all their step forms before any variable
    # steps (LET where a plain sequence of FOR clauses is LET*).
    driver_groups = []

    # WHILE/UNTIL termination tests (CLHS 6.1.2.1.2). Also composing: a loop may
    # carry several, and they bound whatever drivers are present rather than
    # replacing them.
    #
    # Position matters. A termination test is evaluated where it is written
    # (CLHS 6.1.2.1.2), so `while x collect x` tests before accumulating while
    # `collect x until x` accumulates and then tests. The third element is the
    # test's position: the number of main clauses already parsed when the test
    # was read, which is where it interleaves into the iteration (see
    # `iteration_plan` below) -- three `while ... collect ...` pairs test and
    # accumulate alternately, which is loop.11.12/11.22's whole point.
    termination_tests = []  # list of ('while'/'until', test_form, position)

    # A WHEN/UNLESS/IF guards *the selectable-clause that follows it* (CLHS
    # 6.1.3): one selectable-clause, which may itself be another conditional
    # (nesting), optionally followed by more AND-joined selectable-clauses at
    # that same level, optionally followed by an ELSE branch (binding to the
    # innermost still-open conditional -- the usual dangling-else rule) and
    # an optional END. `_parse_selectable_clause`/`_parse_and_chain` are a
    # small recursive-descent pair implementing exactly that grammar;
    # `active_conditionals` is the list of `(kind, test)` guards -- outermost
    # first -- accumulated on the way down, and every leaf clause (DO,
    # RETURN, an accumulation) is stamped with a copy of it. Two clauses
    # guarded by unrelated tests must stay independent:
    #
    #     (loop when (null list2)            do (return-from ... nil)
    #           when (eql x (pop list2))     do (return))
    #
    # ansi-aux's `is-noncontiguous-sublist-of` is exactly this shape, and
    # recursion gives it for free -- each WHEN's guard list is built fresh
    # from `active_conditionals` (empty at top level) and discarded once that
    # WHEN's own then/else/end is fully parsed, rather than leaking into
    # whatever clause happens to come next.
    #
    # Each leaf entry: {'conditionals': [...], 'forms'/'form': ...} -- the
    # same shape body clauses and accumulation clauses always used, so there
    # is one convention for "a clause carries its own guards".
    body_clauses = []

    # Accumulation clauses, in order. CLHS 6.1.3 permits several in one loop
    # (`collect i into foo always (< i 20)`); a single slot silently kept only
    # the last one parsed and discarded the rest.
    # Each entry: {'type', 'form', 'into', 'conditionals', 'type_spec'}.
    accumulations = []

    # The loop's *main* clauses in source order (CLHS 6.1.2.1: the main
    # clauses of one iteration are executed in the order they were written).
    # Each entry: ('do', clause) or ('accum', clause) -- the same clause dicts
    # the two lists above hold, so nothing downstream changes shape. One
    # ordered list is what makes `do (f) collect (g) do (h)` run f, g, h in
    # that order; running the two lists separately made every `do` precede
    # every accumulation, which is observable whenever a `collect` form's
    # side effect (or a later `do`'s) depends on the interleaving.
    main_clauses = []

    def _substitute_it(form, it_form):
        """CLHS 6.1.3: in the *one* selectable-clause immediately governed by
        a WHEN/IF/UNLESS test (not an AND-joined sibling, not anything past
        an END), the token IT stands for the test's form -- literally, by
        substitution at parse time, which is why `(loop for it on '(a b c d)
        when (car it) collect it)` collects `(car it)` re-evaluated each
        iteration (A B C D) rather than the successive sublists: the FOR
        clause's own IT was never shadowed, the substituted copy of the test
        form just happens to read the same free variable the test did.
        `it_form is None` (every clause but that one) leaves `form` alone, so
        a literal IT elsewhere is ordinary variable reference, unchanged."""
        if it_form is None:
            return form
        if isinstance(form, lisptype.LispSymbol) and form.name == 'IT':
            return it_form
        if _consp_internal(form):
            return cons(_substitute_it(car(form), it_form),
                       _substitute_it(cdr(form), it_form))
        return form

    def _parse_do_clause(active_conditionals, it_form=None):
        """DO/DOING: forms up to the next clause keyword, AND or ELSE/END --
        the latter two so a DO inside a conditional's then/else set doesn't
        swallow the token that closes or continues it."""
        nonlocal i
        i += 1
        do_forms = []
        while (i < len(forms) and sym_name(forms[i]) not in LOOP_CLAUSE_KEYWORDS
               and sym_name(forms[i]) not in ('AND', 'ELSE', 'END')):
            do_forms.append(_substitute_it(forms[i], it_form))
            i += 1
        body_clauses.append({'conditionals': list(active_conditionals),
                             'forms': do_forms})
        main_clauses.append(('do', body_clauses[-1]))

    def _parse_return_clause(active_conditionals, it_form=None):
        """CLHS 6.1.5.3: `return expr` **is** `do (return expr)` -- see the
        block comment this replaced, still accurate, just relocated: it is
        parsed as a body clause containing a RETURN-FROM of the loop's own
        block so its guards apply, several survive in one loop, and it exits
        through the implicit NIL block rather than overriding an
        accumulation's value."""
        nonlocal i
        block_name = (loop_block_name if loop_block_name is not None
                      else lisptype.NIL)
        return_args = ([_substitute_it(forms[i+1], it_form)]
                       if i + 1 < len(forms) else [])
        body_clauses.append({'conditionals': list(active_conditionals),
                             'forms': [cons(lisptype.LispSymbol('RETURN-FROM'),
                                            _list_from([block_name] + return_args))]})
        main_clauses.append(('do', body_clauses[-1]))
        i += 2 if return_args else 1

    def _parse_accumulation_clause(active_conditionals, it_form=None):
        """One branch for all the accumulation clauses -- see the module-level
        note by ACCUMULATION_CLAUSES; they differ only in the accumulator
        `execute_iteration_body` dispatches to."""
        nonlocal i
        name = sym_name(forms[i])
        clause = {
            'type': ACCUMULATION_CLAUSES[name],
            'form': _substitute_it(forms[i+1], it_form),
            'into': None,
            'conditionals': list(active_conditionals),
            'type_spec': None,
        }
        i += 2
        # CLHS 6.1.3: "into var" accumulates into a loop-local variable
        # instead of into the loop's value.
        if i < len(forms) and sym_name(forms[i]) == 'INTO':
            into_var = forms[i+1]
            clause['into'] = into_var
            i += 2
        # `maximize x fixnum` / `sum x into total of-type integer`: only the
        # numeric accumulations take a trailing type-spec (CLHS 6.1.3.2), so
        # only they may consume one -- `collect x` followed by `t` would
        # otherwise lose the T.
        if clause['type'] in NUMERIC_ACCUMULATIONS:
            i, clause['type_spec'] = _loop_type_spec(forms, i)
        accumulations.append(clause)
        main_clauses.append(('accum', clause))

    def _parse_selectable_clause(active_conditionals, it_form=None):
        """Parse exactly one selectable-clause (CLHS 6.1.3): a conditional
        (which recurses, consuming its own then/else/end before returning)
        or one of DO/DOING, RETURN, an accumulation. `it_form`, when given,
        is the governing test to substitute for a literal IT -- passed only
        to the single clause immediately following a WHEN/IF/UNLESS test."""
        nonlocal i
        name = sym_name(forms[i])
        if name in ('WHEN', 'IF', 'UNLESS'):
            cond_kind = 'unless' if name == 'UNLESS' else 'when'
            test = forms[i+1]
            i += 2
            _parse_and_chain(active_conditionals + [(cond_kind, test)], it_form=test)
            # ELSE binds to the *innermost* still-open conditional -- the
            # ordinary dangling-else rule -- which is exactly the one whose
            # then-chain this call just finished parsing.
            if i < len(forms) and sym_name(forms[i]) == 'ELSE':
                i += 1
                negated_kind = 'when' if cond_kind == 'unless' else 'unless'
                _parse_and_chain(active_conditionals + [(negated_kind, test)], it_form=test)
            if i < len(forms) and sym_name(forms[i]) == 'END':
                i += 1
        elif name in ('DO', 'DOING'):
            _parse_do_clause(active_conditionals, it_form)
        elif name == 'RETURN':
            _parse_return_clause(active_conditionals, it_form)
        elif name in ACCUMULATION_CLAUSES:
            _parse_accumulation_clause(active_conditionals, it_form)

    def _parse_and_chain(active_conditionals, it_form=None):
        """One selectable-clause, plus every further one AND joins to it at
        the same level (CLHS: `if-then-set ::= selectable-clause {AND
        selectable-clause}*`, and identically for else-forms). `it_form`
        applies only to the first clause -- an AND-joined sibling is not the
        clause the test immediately governs (loop.14.29 pins this down: with
        an outer `(let ((it 'z)) ...)`, `when x collect it and collect it`
        collects the test's value the first time and the LET's Z the
        second)."""
        nonlocal i
        _parse_selectable_clause(active_conditionals, it_form)
        while i < len(forms) and sym_name(forms[i]) == 'AND':
            i += 1
            _parse_selectable_clause(active_conditionals)

    def _parse_for_subclause(clause_start, group):
        """One for-as subclause: its variable, optional type-spec and driver
        spec (CLHS 6.1.2.1).

        `clause_start` is the index of the FOR/AS keyword -- or, for an
        AND-joined later subclause, of the variable itself, since the FOR
        keyword is written once for the whole clause. Returns ``(driver,
        group, end_index)``; ``group`` is the clause's driver group, created
        here on the first subclause and shared by every AND-joined one (a
        group initializes and steps in parallel -- see `driver_groups`).
        """
        token = forms[clause_start]
        if sym_name(token) in ('FOR', 'AS'):
            var_pos = clause_start + 1
        else:
            var_pos = clause_start
        candidate_var = forms[var_pos]
        if not (isinstance(candidate_var, lisptype.LispSymbol)
                or _consp_internal(candidate_var)
                or _loop_is_discarded_var(candidate_var)):
            raise lisptype.LispNotImplementedError('LOOP FOR requires a symbol')
        _claim_variables(candidate_var)

        clause_stop = LOOP_CLAUSE_KEYWORDS

        # Parse the FOR clause into either a driver (IN/ON/ACROSS/FROM...) or an aux binding (=
        # without FROM/IN/etc) when a driver already exists.
        # The optional type-spec sits between the variable and the driver
        # keyword (`for v fixnum being the hash-values of h`), so it has to
        # be consumed before the scan below or FIXNUM reads as the end of
        # the clause and the driver is lost.
        j, _for_type = _loop_type_spec(forms, var_pos + 1)
        saw_driver_keyword = False
        driver_kind = None
        driver_start = None
        driver_end = None
        driver_step = None
        driver_list = None
        aux_init = None
        aux_then = None
        driver_downward = False
        driver_hash_part = None
        driver_symbol_set = None
        driver_using_part = None
        driver_using_var = None
        # Track the order in which FROM/TO/BY etc. are parsed (for correct evaluation order)
        driver_eval_order = []

        while j < len(forms):
            fname = sym_name(forms[j])
            if fname == 'FROM':
                saw_driver_keyword = True
                driver_start = forms[j+1]
                driver_eval_order.append(('FROM', forms[j+1]))
                j += 2
            elif fname == 'UPFROM':
                saw_driver_keyword = True
                driver_start = forms[j+1]
                driver_eval_order.append(('FROM', forms[j+1]))
                j += 2
            elif fname == 'DOWNFROM':
                saw_driver_keyword = True
                driver_start = forms[j+1]
                driver_downward = True
                driver_eval_order.append(('FROM', forms[j+1]))
                j += 2
            elif fname in ('TO', 'UPTO'):
                saw_driver_keyword = True
                driver_end = forms[j+1]
                driver_kind = 'for-range'
                driver_eval_order.append(('TO', forms[j+1]))
                j += 2
            elif fname == 'BELOW':
                saw_driver_keyword = True
                driver_end = forms[j+1]
                driver_kind = 'for-below'
                driver_eval_order.append(('BELOW', forms[j+1]))
                j += 2
            elif fname == 'DOWNTO':
                saw_driver_keyword = True
                driver_end = forms[j+1]
                driver_kind = 'for-range'
                driver_downward = True
                driver_eval_order.append(('DOWNTO', forms[j+1]))
                j += 2
            elif fname == 'ABOVE':
                saw_driver_keyword = True
                driver_end = forms[j+1]
                driver_kind = 'for-below'
                driver_downward = True
                driver_eval_order.append(('ABOVE', forms[j+1]))
                j += 2
            elif fname == 'BY':
                saw_driver_keyword = True
                driver_step = forms[j+1]
                driver_eval_order.append(('BY', forms[j+1]))
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
            elif fname == 'BEING':
                # being {each | the} <what> [{of | in} form] [using (<what> var)]
                #
                # CLHS 6.1.2.1.6 (hash tables) and 6.1.2.1.7 (packages). The
                # previous parse recognized exactly one spelling of one of
                # the eight -- plural SYMBOLS after THE -- and *broke out of
                # the clause* for every other, which left driver_kind None
                # and raised "LOOP FOR clause missing iteration spec". That
                # is the whole of loop6.lsp (47/47) and loop7.lsp (35/35).
                saw_driver_keyword = True
                k = j + 1
                if k < len(forms) and sym_name(forms[k]) in ('THE', 'EACH'):
                    k += 1
                what = sym_name(forms[k]) if k < len(forms) else None
                k += 1
                if what in LOOP_HASH_PARTS:
                    driver_kind = 'for-being-hash'
                    driver_hash_part = LOOP_HASH_PARTS[what]
                elif what in LOOP_PACKAGE_SYMBOL_SETS:
                    driver_kind = 'for-being-package'
                    driver_symbol_set = LOOP_PACKAGE_SYMBOL_SETS[what]
                else:
                    raise lisptype.LispProgramError(
                        f'LOOP BEING does not name an iterable: {what}')
                # The source is required for a hash table and optional for a
                # package, which defaults to *PACKAGE*.
                if k < len(forms) and sym_name(forms[k]) in ('OF', 'IN'):
                    driver_list = forms[k+1]
                    k += 2
                elif driver_kind == 'for-being-hash':
                    raise lisptype.LispProgramError(
                        'LOOP BEING THE HASH-KEYS/HASH-VALUES requires OF or IN')
                # using ({hash-key | hash-value} other-var) names the other
                # half of the entry (CLHS 6.1.2.1.6).
                if k < len(forms) and sym_name(forms[k]) == 'USING':
                    using_clause = forms[k+1] if (k + 1) < len(forms) else None
                    if not _consp_internal(using_clause):
                        raise lisptype.LispProgramError(
                            'LOOP USING requires (hash-key var) or (hash-value var)')
                    using_what = sym_name(car(using_clause))
                    if using_what not in LOOP_HASH_PARTS:
                        raise lisptype.LispProgramError(
                            f'LOOP USING does not name a hash-table part: {using_what}')
                    driver_using_part = LOOP_HASH_PARTS[using_what]
                    driver_using_var = car(cdr(using_clause))
                    _claim_variables(driver_using_var)
                    k += 2
                j = k
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

        driver = {
            'var': candidate_var,
            'kind': driver_kind,
            'start': driver_start,
            'end': driver_end,
            'step': driver_step,
            'list': driver_list,
            'hash_part': driver_hash_part,
            'symbol_set': driver_symbol_set,
            'using_part': driver_using_part,
            'using_var': driver_using_var,
            'eval_order': driver_eval_order,
            'downward': driver_downward,
        }
        if group is None:
            group = [driver]
            driver_groups.append(group)
        else:
            group.append(driver)
        iteration_drivers.append(driver)
        return driver, group, j

    def _and_subclause_start(end_index):
        """Where the next AND-joined for-as subclause starts, or None.

        CLHS 6.1.2.1: ``for-as-clause ::= {for | as} for-as-subclause {and
        for-as-subclause}*`` -- AND joins *subclauses*, and the FOR keyword
        is written once for the whole clause (a redundant one after the AND
        is tolerated). Anything else -- another clause keyword especially --
        is not a subclause start, and the AND is left for the top-level
        parse, as it always was. This is the check the previous parse
        lacked: it broke the clause on AND and then dropped the token, so
        `for i from 1 to 5 and j = 0 then (+ j i)` bound I and lost J
        entirely -- loop.17.21's "Unbound variable: J".
        """
        if end_index >= len(forms) or sym_name(forms[end_index]) != 'AND':
            return None
        if end_index + 1 >= len(forms):
            return None
        token = forms[end_index + 1]
        token_name = sym_name(token)
        if token_name in ('FOR', 'AS'):
            if end_index + 2 >= len(forms):
                return None
            return end_index + 2
        if (_consp_internal(token) or _loop_is_discarded_var(token)
                or (isinstance(token, lisptype.LispSymbol)
                    and token_name not in LOOP_CLAUSE_KEYWORDS)):
            return end_index + 1
        return None

    # WITH's local variables (CLHS 6.1.1.4), as a list of *groups*. Successive
    # WITH clauses initialize sequentially -- each one sees the previous -- but
    # the specs an AND joins initialize in parallel, all from the environment
    # outside the loop, which is the distinction LET* and LET make and the
    # reason a flat list would not do.
    # Each spec: {'var', 'type', 'init'} with 'init' None meaning "no = form".
    with_groups = []

    initially_forms = []  # INITIALLY prologue, run once before the first iteration
    finally_forms = []
    loop_block_name = None  # NIL unless a NAMED clause gives the loop its own block name

    # Parse clauses
    while i < len(forms):
        token = forms[i]
        name = sym_name(token)

        if name == 'NAMED':
            loop_block_name = forms[i+1]
            i += 2
            continue

        if name == 'WITH':
            # with var [type-spec] [= form] {and var [type-spec] [= form]}*
            # (CLHS 6.1.1.4). Previously unrecognized entirely, so WITH and its
            # variable fell into body_forms and were evaluated once per
            # iteration as free references -- "Unbound variable: WITH".
            group = []
            i += 1
            while True:
                spec_var = forms[i]
                _claim_variables(spec_var)
                i += 1
                i, spec_type = _loop_type_spec(forms, i)
                spec_init = None
                if i < len(forms) and sym_name(forms[i]) == '=':
                    spec_init = forms[i+1]
                    i += 2
                group.append({'var': spec_var, 'type': spec_type, 'init': spec_init})
                if i < len(forms) and sym_name(forms[i]) == 'AND':
                    i += 1
                    continue
                break
            with_groups.append(group)
            continue

        if name in ('FOR', 'AS'):
            # CLHS 6.1.2.1: "either the keyword FOR or the keyword AS may be
            # used to begin a for-as-clause" -- AS is a full synonym, not a
            # distinct clause. Previously unrecognized, so e.g. (loop as x in
            # '(a b c) collect x) fell through to the "no iteration clause"
            # branch below and looped forever evaluating AS/X/IN as inert
            # body forms until the 10-minute LOOP_TIMEOUT_ERROR hard cap
            # fired -- exercised by ~15 tests across iteration/loop2-7.lsp.
            #
            # One clause may carry several AND-joined subclauses, which
            # initialize and step in parallel (CLHS 6.1.2.1) -- that is what
            # the group list tracks; see `driver_groups`.
            group = None
            clause_start = i
            while True:
                _, group, clause_start = _parse_for_subclause(clause_start, group)
                nxt = _and_subclause_start(clause_start)
                if nxt is None:
                    break
                clause_start = nxt
            i = clause_start
            continue

        elif name == 'WHILE':
            termination_tests.append(('while', forms[i+1], len(main_clauses)))
            i += 2

        elif name == 'UNTIL':
            termination_tests.append(('until', forms[i+1], len(main_clauses)))
            i += 2

        elif name == 'REPEAT':
            # CLHS 6.1.2.1.1: REPEAT bounds the iteration; it does not replace
            # whatever driver is present. Modelling it as an anonymous driver is
            # what makes `for x = 7 repeat 5` and `repeat 5 for x = 7` mean the
            # same thing regardless of clause order.
            repeat_driver = {
                'var': None,
                'kind': 'repeat',
                'count': forms[i+1],
            }
            iteration_drivers.append(repeat_driver)
            # The initialization phase walks `driver_groups`, so REPEAT needs
            # its own (singleton) group there or `_remaining` is never
            # computed and every REPEAT loop runs zero times.
            driver_groups.append([repeat_driver])
            i += 2

        elif (name in ('WHEN', 'IF', 'UNLESS', 'DO', 'DOING', 'RETURN')
              or name in ACCUMULATION_CLAUSES):
            # A selectable-clause at the top level (CLHS 6.1.3): a
            # conditional (which recursively consumes its own then/else/end),
            # DO/DOING, RETURN, or an accumulation -- plus every further
            # clause AND joins to it at this same level, e.g. `collect x into
            # a and sum x into b` with no governing WHEN at all.
            _parse_and_chain([])

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

        elif name == 'FINALLY':
            i += 1
            while i < len(forms) and sym_name(forms[i]) not in LOOP_CLAUSE_KEYWORDS:
                # CLHS 6.1.6.2: the epilogue is just a progn in the loop's
                # variable scope. A (RETURN x) here is an *ordinary return
                # form*, evaluated like any other -- it exits through the
                # loop's implicit block (or, for a NAMED loop, through the
                # first enclosing NIL block further out, which is what
                # loop.13.87 pins down) rather than naming the loop's value
                # by special case.
                finally_forms.append(forms[i])
                i += 1
        else:
            # Simple loop (CLHS 6.1.1): with no iteration-control clause seen so
            # far the compound forms are the loop body. Once a driver or a
            # termination test exists this is an unrecognized loop keyword; it is
            # still dropped here, as it always has been -- see plan.md's
            # Discovered issues, this is the one remaining silent path in LOOP.
            if not iteration_drivers and not termination_tests:
                body_clauses.append({'conditionals': [], 'forms': [token]})
                main_clauses.append(('do', body_clauses[-1]))
            i += 1

    # CLHS 6.1.3: Validate that boolean-termination clauses (ALWAYS, NEVER,
    # THEREIS) are not mixed with value-accumulation clauses that return to the
    # loop value (COLLECT, APPEND, NCONC, SUM, COUNT without INTO). If the
    # accumulation goes INTO a variable, both can coexist.
    has_loop_value_accumulation = False  # accumulation without INTO
    has_boolean_termination = False
    for clause in accumulations:
        acc_type = clause['type']
        # Only value accumulations without INTO return to the loop value
        if acc_type in ('collect', 'append', 'nconc', 'sum', 'count', 'maximize', 'minimize'):
            if clause['into'] is None:
                has_loop_value_accumulation = True
        elif acc_type in BOOLEAN_TERMINATION_CLAUSES:
            has_boolean_termination = True
        if has_loop_value_accumulation and has_boolean_termination:
            raise lisptype.LispProgramError(
                'LOOP cannot mix value-accumulation clauses without INTO with boolean-termination clauses (ALWAYS, NEVER, THEREIS)')

    # One ordered execution plan per iteration (CLHS 6.1.2.1): the main
    # clauses and the WHILE/UNTIL tests interleaved in the order they were
    # written. Each termination test recorded the number of main clauses
    # already parsed when it was read -- merging on that position puts
    # `while x collect x while y collect y` in the order test-x, collect-x,
    # test-y, collect-y. The previous two-position approximation (every test
    # written after the first main clause ran after *all* of them) stepped
    # every `while ... collect ...` pair once per single iteration, which is
    # what loop.11.12/11.22 caught.
    iteration_plan = []
    _test_index = 0
    for _clause_index, _entry in enumerate(main_clauses):
        while (_test_index < len(termination_tests)
               and termination_tests[_test_index][2] <= _clause_index):
            iteration_plan.append(('test', termination_tests[_test_index][0],
                                   termination_tests[_test_index][1]))
            _test_index += 1
        iteration_plan.append(('clause', _entry[0], _entry[1]))
    while _test_index < len(termination_tests):
        iteration_plan.append(('test', termination_tests[_test_index][0],
                               termination_tests[_test_index][1]))
        _test_index += 1

    # Execute the loop
    #
    # CLHS 6.1.1.4 gives the LOOP form exactly four possible values, and every
    # one of them comes from a clause that *names* a value: an explicit RETURN
    # (body clause or epilogue), an ALWAYS/NEVER/THEREIS decision, a
    # destination-less accumulation clause, or -- when none of those is
    # present -- NIL. A `do` clause's forms and a `finally` clause's forms are
    # evaluated for effect and their values are discarded.
    #
    # `result` used to be assigned from *three* places: the RETURN clause, and
    # then again from every body-clause form and every FINALLY form, so the
    # last side effect the loop happened to perform became its value.
    # `(loop for x = 1 repeat 3 do (list x))` answered `(1)` and
    # `(loop repeat 100000 do (assert ...) do (setf prev next))` answered the
    # last `next` -- both must be NIL. There is now no such slot at all: the
    # three clause families that *can* name a value each have their own
    # channel (`early_decision`, `acc_states`, and the implicit NIL block a
    # RETURN form leaves through), and nothing else can reach the value.
    return_triggered = False  # Flag for when the loop should stop iterating

    # ALWAYS/NEVER/THEREIS decide the loop's value outright (CLHS 6.1.2.2)
    # instead of accumulating, and they end the loop the moment they decide, so
    # the epilogue never runs and cannot override them. One decision slot for
    # all three: they used to be two independent flags whose "did it fire?"
    # tests differed (`always_failed` versus `thereis_result is not None`), and
    # a third clause added that way would have been a third convention.
    early_decision = {'decided': False, 'value': lisptype.NIL}

    # One accumulator per INTO destination, keyed by the variable's name (None
    # is "the loop's own value"). Several clauses may share a destination, which
    # is what makes `collect a into x collect b into x` accumulate in order.
    # Each state is {'type': str, 'items': [], 'number': int, 'extremum': None}.
    acc_states = {}

    def _acc_key(clause):
        into = clause['into']
        return None if into is None else into.name

    for _clause in accumulations:
        _key = _acc_key(_clause)
        if _key not in acc_states:
            # For numeric accumulations, use the type-spec to determine the
            # correct initial value (e.g., 0.0 for FLOAT types, not 0)
            initial_number = 0
            if _clause['type'] in NUMERIC_ACCUMULATIONS and _clause['type_spec'] is not None:
                # Extract base type from type-spec (e.g., INTEGER from (INTEGER 0 100))
                base_type = _clause['type_spec']
                if _consp_internal(base_type):
                    base_type = car(base_type)
                base_type_name = _loop_sym_name(base_type)
                initial_number = _LOOP_ZERO_BY_TYPE.get(base_type_name, 0)
            acc_states[_key] = {'type': _clause['type'], 'items': [],
                                'number': initial_number, 'extremum': None, 'tail': None}

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
            # For append/nconc, start with the tail (which may be NIL or a dotted tail)
            result_list = state.get('tail') if acc_type in ('append', 'nconc') else lisptype.NIL
            if result_list is None:
                result_list = lisptype.NIL
            for item in reversed(state['items']):
                result_list = cons(item, result_list)
            return result_list
        if acc_type in ('sum', 'count'):
            return state['number']
        if acc_type in ('maximize', 'minimize'):
            # CLHS 6.1.3.2 leaves the value undefined when the clause never
            # runs; NIL is the value with no extremum yet.
            extremum = state['extremum']
            return lisptype.NIL if extremum is None else extremum
        if acc_type in BOOLEAN_TERMINATION_CLAUSES:
            # Reached only when the loop ran to completion without the clause
            # deciding: ALWAYS and NEVER are then true (vacuously so for a loop
            # with no iterations), and THEREIS found nothing.
            return lisptype.NIL if acc_type == 'thereis' else lisptype.T
        return lisptype.NIL

    def _execute_accumulation_clause(clause, loop_env):
        """Run one accumulation clause for this iteration (its guards have
        already passed)."""
        nonlocal return_triggered
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
                # If cur is non-NIL here, it's a dotted tail (e.g., (A B C . TAIL))
                if cur is not lisptype.NIL and cur is not None:
                    state['tail'] = cur
            elif acc_type == 'append' and acc_value is not lisptype.NIL and acc_value is not None:
                state['items'].append(acc_value)
        elif acc_type == 'sum':
            # Through the Lisp + function, not Python's +=: the numeric
            # tower's exactness rules (an exact COMPLEX staying exact --
            # LOOP.10.73/.87) live in one place, `math_arithmetic._s_plus_`,
            # and a Python `+=` on the accumulator silently produced a raw
            # float-pair complex instead.
            state['number'] = _lisp_plus(state['number'], acc_value)
        elif acc_type == 'count':
            if lisptype.is_truthy(acc_value):
                state['number'] += 1
        elif acc_type in ('maximize', 'minimize'):
            # CLHS 6.1.3.2: the largest/smallest value the form takes. The
            # first value seeds the extremum -- there is no identity element
            # to start from, since the values need only be REALs and may be
            # all negative or all positive.
            extremum = state['extremum']
            if extremum is None:
                state['extremum'] = acc_value
            elif (acc_value > extremum) if acc_type == 'maximize' else (acc_value < extremum):
                state['extremum'] = acc_value
        elif acc_type in BOOLEAN_TERMINATION_CLAUSES:
            # CLHS 6.1.2.2. ALWAYS fails on the first false value, NEVER on
            # the first true one, and THEREIS succeeds on the first true
            # one; in every case the decision ends the loop at once. One
            # branch, because they are one clause family that differs only
            # in which truth value decides and what the answer then is.
            is_true = lisptype.is_truthy(acc_value)
            decides = (not is_true) if acc_type == 'always' else is_true
            if decides:
                early_decision['decided'] = True
                early_decision['value'] = acc_value if acc_type == 'thereis' else lisptype.NIL
                return_triggered = True

        if clause['into'] is not None:
            # Through the frame that established it, for the same reason the
            # establishing bind goes through the frame: `set_variable` would
            # walk out to an enclosing binding of the same name.
            loop_frame[0].bind(clause['into'], _accumulated_value(key))

    def execute_iteration_body(loop_env):
        """Execute one iteration of the loop body.

        CLHS 6.1.2.1: the main clauses of one iteration run *in the order
        they were written* -- a `collect` clause between two `do` clauses
        accumulates between them, each clause's WHEN/UNLESS guards apply
        only to it, and a WHILE/UNTIL test is evaluated at the position it
        was written (CLHS 6.1.2.1.2), interleaved with the clauses around
        it. All of that is one walk over `iteration_plan`.

        Returns True to continue iterating, False when a termination test
        ended the loop (or a body form returned -- `return_triggered`).
        """
        nonlocal return_triggered

        for entry in iteration_plan:
            if entry[0] == 'test':
                _, kind, test_form = entry
                test_result = eval(test_form, loop_env)
                if kind == 'until' and lisptype.is_truthy(test_result):
                    return False
                if kind == 'while' and not lisptype.is_truthy(test_result):
                    return False
                continue
            _, kind, clause = entry
            if not _conditionals_pass(clause['conditionals'], loop_env):
                continue
            if kind == 'do':
                for f in clause['forms']:
                    # For effect only -- a `do` clause never supplies the loop's
                    # value (CLHS 6.1.1.4).
                    eval(f, loop_env)
                    if return_triggered:
                        return False
            else:
                _execute_accumulation_clause(clause, loop_env)
                if return_triggered:
                    return False
        return True

    loop_watchdog = LoopWatchdog(
        'LOOP',
        lambda: [f"body_clauses: {body_clauses}",
                 f"drivers: {[(d['kind'], d['var']) for d in iteration_drivers]}",
                 f"termination_tests: {termination_tests}"],
        hard_cap=LOOP_TIMEOUT_ERROR)

    # `loop_env` and its frame are created inside _run_loop_and_finalize, but the
    # frame has to be unwound from outside it so a dynamic binding is undone on
    # a non-local exit too.
    loop_frame = []

    def _run_loop_and_finalize():
        """Run the loop's iteration/FINALLY/result logic under the loop's own
        implicit NIL block, so a plain (RETURN x) / (RETURN-FROM NIL x) inside
        the body exits *this* LOOP form instead of leaking out to whatever
        enclosing DO/DOLIST/DOTIMES/LOOP happens to be running the dynamic
        extent (see plan.md Finding under M0 step 1).
        """
        # Main loop execution, watched for runaway iteration. The watchdog is
        # created in the enclosing scope so the RESOLVED/ABORTED counterpart can
        # be emitted around the whole loop, including its non-local exits.
        check_loop_timeout = loop_watchdog.tick

        def _init_driver(loop_env, driver):
            kind = driver['kind']
            if kind in ('for-in', 'for-on'):
                driver['_cur'] = eval(driver['list'], loop_env)
                # CLHS 6.1.2.1.3: FOR x IN requires a proper list (CLHS 14.1).
                # A proper list terminates in NIL; a dotted list like (A . B) is not.
                if kind == 'for-in':
                    _validate_proper_list(driver['_cur'], 'LOOP FOR x IN')
                # CLHS 6.1.2.1.3: BY names the step function (default CDR);
                # `driver['step']` is unevaluated (e.g. `#'cddr`) exactly like
                # every other driver's BY, and was never consulted here --
                # `_step_driver` always stepped by CDR regardless.
                step_form = driver.get('step')
                driver['_step_fn'] = eval(step_form, loop_env) if step_form is not None else None
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

                # CLHS 5.1.2: FROM/TO/BY are single-value contexts, so a bound
                # like `(floor ...)` -- which returns quotient *and*
                # remainder -- must be reduced to its primary value here, the
                # same way `_primary_value` already does for a CASE keyform.
                # Left as `eval(...)` directly, `end` became the
                # `MultipleValues` wrapper itself, which `_driver_has_value`'s
                # `cur <= end` cannot compare against an int at all.

                # Evaluate forms in source order (CLHS 6.1.2.1.3). This matters for
                # side effects like INCF. We track source order via eval_order.
                start_value = 0  # default
                end_value = None
                step_value = 1    # default

                eval_order = driver.get('eval_order', [])
                for keyword, form in eval_order:
                    if keyword in ('FROM', 'UPFROM', 'DOWNFROM'):
                        start_value = (form if isinstance(form, int)
                                     else _primary_value(eval(form, loop_env)))
                    elif keyword in ('TO', 'UPTO', 'DOWNTO', 'BELOW', 'ABOVE'):
                        end_value = _primary_value(eval(form, loop_env))
                    elif keyword == 'BY':
                        # Evaluate the form for side effects, get its value
                        step_value = (form if isinstance(form, int)
                                    else _primary_value(eval(form, loop_env)))

                # If no end was explicitly set in eval_order, get default
                if end_value is None:
                    end_form = driver.get('end')
                    if end_form is not None and not isinstance(end_form, int):
                        end_value = _primary_value(eval(end_form, loop_env))

                # If we're counting downward (DOWNFROM, DOWNTO, or ABOVE),
                # negate the step (driver['downward'] flag tells us this)
                downward = driver.get('downward', False)
                if downward and step_value > 0:
                    step_value = -step_value

                if step_value == 0:
                    raise lisptype.LispNotImplementedError('LOOP BY step cannot be 0')
                driver['_cur'] = start_value
                driver['_end'] = end_value
                driver['_step'] = step_value
                return True
            if kind == 'for-from':
                # `for x from n` without TO/BELOW: an unbounded driver that counts
                # forever (or until REPEAT or another bound terminates it).
                # Evaluate FROM and BY in source order.
                start_value = 0  # default

                eval_order = driver.get('eval_order', [])
                for keyword, form in eval_order:
                    if keyword in ('FROM', 'UPFROM', 'DOWNFROM'):
                        start_value = (form if isinstance(form, int)
                                     else _primary_value(eval(form, loop_env)))

                # Get the step form from driver dict, which may have negation
                # applied (at parse time for DOWNFROM)
                step_form = driver.get('step')
                if step_form is None:
                    step_value = 1
                else:
                    step_value = (step_form if isinstance(step_form, int)
                                else _primary_value(eval(step_form, loop_env)))

                if step_value == 0:
                    raise lisptype.LispNotImplementedError('LOOP BY step cannot be 0')
                driver['_cur'] = start_value
                driver['_step'] = step_value
                return True
            if kind == 'repeat':
                count = _primary_value(eval(driver['count'], loop_env))
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
            if kind == 'for-being-hash':
                # CLHS 6.1.2.1.6. The entries are snapshotted here rather than
                # iterated lazily: the body may add to or remove from the table,
                # and a live Python view would raise "dictionary changed size
                # during iteration" as a Python error leaking into Lisp.
                # Through the hash table model's own traversal, which is
                # already a snapshot for exactly this reason. `hasattr(table,
                # 'items')` accepted any Python mapping and rejected the real
                # table once it stopped being a `dict`; worse, the raise
                # itself passed `datum=`/`message=` to a `LispTypeError`
                # whose signature is `(message, expected_type, actual_value)`,
                # so the type error surfaced as a Python `TypeError` --
                # standing rule 2, in the code meant to prevent it.
                from .misc_hashtables import check_hash_table
                table = eval(driver['list'], loop_env)
                check_hash_table(table, 'LOOP BEING THE HASH-KEYS')
                driver['_items'] = table.entries()
                driver['_idx'] = 0
                return True
            if kind == 'for-being-package':
                # CLHS 6.1.2.1.7. The package designator is *evaluated* -- it may
                # be a string, a symbol or a (find-package ...) form -- and then
                # resolved and enumerated by the shared package helpers, so this
                # agrees with DO-SYMBOLS / DO-EXTERNAL-SYMBOLS about which
                # symbols each of the three sets contains. The previous copy
                # here swallowed a failed lookup with `except Exception` and
                # iterated an empty package instead of signaling.
                #
                # The designator resolves through `coerce_to_package` first, so
                # a missing package signals a real PACKAGE-ERROR (CLHS
                # 6.1.2.1.7, with the designator on its :package slot):
                # `coerce_to_package` raises the legacy LispError for a
                # missing name, which HANDLER-CASE/IGNORE-ERRORS catch but
                # `(signals-error ... 'package-error)` rightly rejects -- that
                # conversion is exactly what loop.7.18/.19/.20 test.
                from .misc_packages import package_symbols, coerce_to_package
                from .evaluation_conditions import signal_error_object
                pkg_spec = driver.get('list')
                pkg_value = eval(pkg_spec, loop_env) if pkg_spec is not None else None
                try:
                    pkg = coerce_to_package(pkg_value)
                except lisptype.LispError as exc:
                    # `signal_error_object` never returns (ERROR semantics:
                    # handlers run at the signal point, then the condition
                    # unwinds if none took control).
                    signal_error_object(lisptype.PackageError(
                        package=pkg_value, message=str(exc)))
                driver['_items'] = package_symbols(pkg, driver['symbol_set'])
                driver['_idx'] = 0
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
            if kind in ('for-being-hash', 'for-being-package'):
                return driver.get('_idx', 0) < len(driver.get('_items', ()))
            return False

        def _bind_driver(frame, driver):
            loop_env = frame.env
            kind = driver['kind']
            var = driver['var']
            if kind == 'for-in':
                _bind_varspec(frame, var, car(driver['_cur']))
                return
            if kind == 'for-on':
                _bind_varspec(frame, var, driver['_cur'])
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
                from .arrays import string_element
                _bind_varspec(frame, var, string_element(seq, seq[idx]))
                return
            if kind in ('for-range', 'for-below'):
                _bind_varspec(frame, var, driver['_cur'])
                return
            if kind == 'for-from':
                _bind_varspec(frame, var, driver['_cur'])
                return
            if kind == 'for-equals':
                if driver['_first']:
                    value_form = driver.get('start')
                    driver['_first'] = False
                else:
                    # No THEN form means the init form supplies every value.
                    step_form = driver.get('step')
                    value_form = driver.get('start') if step_form is None else step_form
                _bind_varspec(frame, var, _primary_value(eval(value_form, loop_env)))
                return
            if kind == 'repeat':
                return
            if kind == 'for-being-hash':
                key, value = driver['_items'][driver['_idx']]
                part = {'key': key, 'value': value}
                _bind_varspec(frame, var, part[driver['hash_part']])
                if driver['using_var'] is not None:
                    _bind_varspec(frame, driver['using_var'], part[driver['using_part']])
                return
            if kind == 'for-being-package':
                _bind_varspec(frame, var, driver['_items'][driver['_idx']])
                return

        def _step_driver(loop_env, driver):
            kind = driver['kind']
            if kind in ('for-in', 'for-on'):
                step_fn = driver.get('_step_fn')
                driver['_cur'] = step_fn(driver['_cur']) if step_fn is not None else cdr(driver['_cur'])
                return
            if kind == 'for-across':
                driver['_idx'] = driver.get('_idx', 0) + 1
                return
            if kind in ('for-range', 'for-below', 'for-from'):
                cur, step = driver['_cur'], driver['_step']
                if isinstance(cur, complex) or isinstance(step, complex):
                    # Step through the Lisp `+`'s complex path: a `from`
                    # or `by` value that is a complex must step in Lisp
                    # arithmetic, so exact parts stay exact (`loop for c
                    # from #c(0 1) by 2` yields `#c(2 1)`, not the float
                    # complex Python's `complex + int` would produce --
                    # the numeric accident that let loop1's results
                    # EQL-match their expected literals). `LispComplex`
                    # subclasses `complex`, so this covers both shapes.
                    from .math_arithmetic import _lisp_complex_add
                    driver['_cur'] = _lisp_complex_add(cur, step)
                else:
                    driver['_cur'] = cur + step
                return
            if kind == 'for-equals':
                # Stepping happens in _bind_driver -- see _init_driver.
                return
            if kind == 'repeat':
                driver['_remaining'] -= 1
                return
            if kind in ('for-being-hash', 'for-being-package'):
                driver['_idx'] = driver.get('_idx', 0) + 1
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
        # LOOP takes no declarations, so its variables are lexical unless the
        # symbol has been *proclaimed* special -- which is `BindingFrame`'s
        # decision, the same one LET and the DO family now make. Chaining
        # through `block_env` puts the loop's implicit block on the lexical
        # chain of every form evaluated in `loop_env`.
        loop_env = lisptype.Environment(block_env)
        frame = BindingFrame(loop_env)
        loop_frame.append(frame)

        # WITH's variables are initialized once, before the iteration begins
        # (CLHS 6.1.1.4), and inside the loop's implicit NIL block -- which is
        # what makes `(loop with nil = (return t) return nil)` return T rather
        # than letting the RETURN escape to an enclosing loop.
        #
        # Within a group (the specs an AND joins) every init form is evaluated
        # before any of them is bound, so each sees the *outer* value of the
        # names its siblings bind; between groups the binding is sequential, so
        # `with x = y with y = (1+ x)` reads the x this loop just bound. LET and
        # LET* in miniature, and the reason the groups exist at all.
        for group in with_groups:
            values = [eval(spec['init'], loop_env) if spec['init'] is not None
                      else _loop_type_default(spec['type'])
                      for spec in group]
            for spec, value in zip(group, values):
                _bind_varspec(frame, spec['var'], value)

        # CLHS 6.1.2.1: the for-as clauses are initialized *sequentially* --
        # each driver's FROM/TO/BY forms are evaluated while the PRECEDING
        # drivers' variables are already bound, because a later clause may
        # read one of them (=.6: `for i from 5 to 10000 by 17 for j from 2
        # to i by 19`). `_init_driver` evaluates the forms, so each driver's
        # variable must be bound before the next driver is initialized.
        # `for =` is excluded: its init form keeps evaluating where it always
        # did, on the first in-loop bind, because that is after the INTO
        # bindings (CLHS 6.1.1.7) and its current timing is what the suite
        # passes with. `_bound_at_init` tells the in-loop bind to skip the
        # first pass for these drivers -- re-binding them would be harmless
        # for every kind except `for =`, whose step form must not run early.
        # A driver that starts exhausted (an empty list, an empty table)
        # binds nothing here and the loop exits before the body, exactly as
        # when the first binding happened inside the while.
        for group in driver_groups:
            if len(group) == 1:
                d = group[0]
                _init_driver(loop_env, d)
                if d['kind'] != 'for-equals' and _driver_has_value(d):
                    _bind_driver(frame, d)
                    d['_bound_at_init'] = True
                continue
            # CLHS 6.1.2.1: the subclauses one AND joins are processed in
            # parallel -- every bound form is evaluated before any of the
            # group's variables is bound (LET, not LET*). A for-equals
            # driver's init is such a form, so it is evaluated here too and
            # stashed; each later iteration just installs the value its step
            # phase computed.
            for d in group:
                _init_driver(loop_env, d)
            parallel_inits = []
            for d in group:
                if d['kind'] == 'for-equals':
                    parallel_inits.append(
                        (d, _primary_value(eval(d['start'], loop_env))))
                elif _driver_has_value(d):
                    _bind_driver(frame, d)
                    d['_bound_at_init'] = True
            for d, value in parallel_inits:
                d['_parallel_equals'] = True
                d['_pending_value'] = value
                _bind_varspec(frame, d['var'], value)

        # INTO names a variable local to the loop (CLHS 6.1.3), so bind it
        # through the frame: the accumulation must not assign through to an
        # outer binding of the same name and clobber it.
        # CLHS 6.1.1.7: error if INTO variable matches an existing binding
        seen_into_vars = set()
        for clause in accumulations:
            if clause['into'] is not None:
                for var_name in _loop_varspec_names(clause['into']):
                    if var_name in bound_variable_names:
                        raise lisptype.LispProgramError(
                            f'LOOP accumulates INTO {var_name} which is already bound')
                    if var_name not in seen_into_vars:
                        frame.bind(clause['into'], _accumulated_value(_acc_key(clause)))
                        seen_into_vars.add(var_name)

        # The prologue runs once, after the iteration variables exist and
        # before the first termination test (CLHS 6.1.7.1).
        for f in initially_forms:
            eval(f, loop_env)

        # With no drivers, no termination test and nothing to execute there is
        # nothing to iterate; running would just spin until the hard cap.
        if iteration_drivers or termination_tests or body_clauses or accumulations:
            try:
                while all(_driver_has_value(d) for d in iteration_drivers):
                    check_loop_timeout()

                    # Bind before testing. A termination test routinely reads the
                    # variable its own driver supplies -- (loop for x = 1 then (* 2 x)
                    # while (< x 20) ...) -- so testing first sees either an unbound
                    # variable on the first iteration or a stale one thereafter.
                    # A driver bound at init (`_bound_at_init`, popped here) skips
                    # exactly its first bind: its variable already holds the value
                    # its own init produced, and re-binding a `for =` driver on the
                    # first iteration would evaluate the *step* form and clobber
                    # that value with the second one. A parallel for-equals
                    # driver binds the value its step phase already computed.
                    for d in iteration_drivers:
                        if d.pop('_bound_at_init', False):
                            continue
                        if d.get('_parallel_equals'):
                            _bind_varspec(frame, d['var'], d['_pending_value'])
                            continue
                        _bind_driver(frame, d)

                    if not execute_iteration_body(loop_env):
                        break

                    if return_triggered:
                        break

                    # CLHS 6.1.2.1: a parallel group's step forms are all
                    # evaluated before any of its variables steps, so a
                    # `then` form sees every variable's pre-step value
                    # (loop.17.21). The parallel for-equals drivers evaluate
                    # first -- their step *is* an evaluation -- and the
                    # structural drivers step after, mutating only their own
                    # private cursors; the computed values are bound last.
                    parallel_steps = []
                    for d in iteration_drivers:
                        if d.get('_parallel_equals'):
                            step_form = d.get('step')
                            value_form = (step_form if step_form is not None
                                          else d.get('start'))
                            parallel_steps.append(
                                (d, _primary_value(eval(value_form, loop_env))))
                    for d in iteration_drivers:
                        if not d.get('_parallel_equals'):
                            _step_driver(loop_env, d)
                    for d, value in parallel_steps:
                        # Stored, not bound: the next iteration's bind phase
                        # installs it, the same channel the init value used.
                        d['_pending_value'] = value
            except LoopFinishException:
                # LOOP-FINISH terminates the loop immediately, skipping any
                # remaining body forms and drivers. The FINALLY clauses still run.
                pass

        # CLHS 6.1.2.2: ALWAYS/NEVER/THEREIS terminate the loop *immediately*
        # when their test decides the answer -- the epilogue does not run, so a
        # FINALLY (RETURN ...) cannot override the NIL or the found value.
        if early_decision['decided']:
            return early_decision['value']

        # Execute FINALLY forms -- in the loop environment, so they can see the
        # iteration variables and any INTO accumulator (CLHS 6.1.4: the epilogue
        # is inside the loop's variable bindings). A (RETURN x) among them is
        # an ordinary return form: it exits through the loop's implicit block
        # (or the first enclosing NIL block further out for a NAMED loop), so
        # an epilogue return transfers out of the LOOP form rather than merely
        # naming its value.
        for f in finally_forms:
            eval(f, loop_env)

        # An accumulation with INTO feeds its variable, not the loop's value;
        # only a destination-less clause supplies the value of the LOOP form.
        if None in acc_states:
            return _accumulated_value(None)

        # No clause named a value, so the LOOP form's value is NIL.
        return lisptype.NIL

    # The loop's implicit (or NAMED) block: its frame is registered on this
    # child environment, which `loop_env` chains through inside
    # _run_loop_and_finalize, so the WITH initialization forms -- CLHS
    # 6.1.1.4's `(loop with nil = (return t) return nil)` case -- and every
    # body form resolve a RETURN to *this* loop's block lexically.
    block_env = lisptype.Environment(env)
    try:
        with loop_watchdog:
            return _run_with_nil_block(_run_loop_and_finalize, loop_block_name, block_env)
    finally:
        for frame in loop_frame:
            frame.unwind()


def _run_with_nil_block(thunk, block_name=None, env=None):
    """Run thunk() inside the implicit block DO/DO*/DOLIST/DOTIMES/LOOP and
    every function-definition body establishes (CLHS 6.1.1 / 3.1.2.1.2.3).

    block_name is the target block's name: None/NIL for the ordinary implicit
    NIL block every one of these forms gets by default, or a symbol for a
    LOOP that used a NAMED clause (CLHS 6.1: NAMED gives the loop its own
    block instead of NIL, so a bare (RETURN x) -- which is (RETURN-FROM NIL
    x) -- must NOT be caught here; it has to keep propagating to find an
    actual enclosing NIL block).

    env is the environment the form's body evaluates in, and is where the
    implicit block's frame is registered (see
    evaluation_control_flow.establish_block_frame), so RETURN-FROM resolves
    its target through the lexical chain: a closure defined inside the body
    returns to *this* implicit block, while the same transfer raised from
    code lexically outside it is re-raised instead of being caught by name.
    """
    from .evaluation_core import ReturnFromException
    from .evaluation_control_flow import (
        establish_block_frame, deactivate_frame)

    frame = establish_block_frame(env, block_name)
    try:
        return thunk()
    except ReturnFromException as e:
        if e.block_frame is frame:
            return e.value
        raise
    finally:
        deactivate_frame(frame)


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
    
    # The implicit NIL block's frame is registered on its own child
    # environment, which adds no variable bindings; `loop_env` chains through
    # it, so both the init forms (evaluated in `block_env`, still the
    # enclosing lexical environment variable-wise) and the body/result forms
    # (evaluated in `loop_env`) see the block lexically.
    block_env = lisptype.Environment(env)
    loop_env = lisptype.Environment(block_env)
    
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
    
    # Evaluate all init forms first (parallel binding like LET), in the
    # *enclosing* environment -- DO.16 pins that, since its init form refers to
    # a variable the body then declares special. `block_env` adds no variable
    # bindings, so this is the same lexical environment for variables.
    init_values = [eval(init_form, block_env) for var, init_form, _ in var_specs]

    # One shared binder decides lexical vs. dynamic for each variable and
    # undoes any dynamic binding on the way out, however this form exits.
    frame = BindingFrame(loop_env, body=body,
                         bound_vars=[var for var, _, _ in var_specs])
    _, body = body_specials(body)

    for (var, _, _), value in zip(var_specs, init_values):
        frame.bind(var, value)

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
                frame.bind(var, value)

    with frame, watchdog:
        return _run_with_nil_block(_loop, lisptype.NIL, block_env)


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
    
    # The implicit NIL block's frame is registered on its own child
    # environment, which adds no variable bindings; `loop_env` chains through
    # it, so the body/result forms -- and the sequentially-evaluated init
    # forms, which already run in `loop_env` -- see the block lexically.
    block_env = lisptype.Environment(env)
    loop_env = lisptype.Environment(block_env)
    
    # The variables this DO* is about to bind, needed before the first bind so
    # the binder can tell a declaration *of* a variable it binds from a free
    # one (DO*.17).
    declared_vars = []
    current = var_list
    while _consp_internal(current):
        spec = car(current)
        declared_vars.append(car(spec) if _consp_internal(spec) else spec)
        current = cdr(current)
    # DO* evaluates each init form in `loop_env`, so a free special declaration
    # must not be installed until the inits are done -- see DO*.16 and
    # `install_free_declarations`.
    frame = BindingFrame(loop_env, body=body, bound_vars=declared_vars,
                         defer_free_declarations=True)
    _, body = body_specials(body)

    # Parse var specs and evaluate init forms SEQUENTIALLY (like LET*)
    var_specs = []
    current = var_list
    while _consp_internal(current):
        spec = car(current)
        if isinstance(spec, lisptype.LispSymbol):
            frame.bind(spec, lisptype.NIL)
            var_specs.append((spec, None))
        elif _consp_internal(spec):
            var = car(spec)
            init_form = car(cdr(spec)) if _consp_internal(cdr(spec)) else lisptype.NIL
            step_form = car(cdr(cdr(spec))) if _consp_internal(cdr(cdr(spec))) else None
            # Evaluate in current loop_env (sequential)
            init_value = eval(init_form, loop_env)
            frame.bind(var, init_value)
            var_specs.append((var, step_form))
        current = cdr(current)

    # The inits are done; the body, the step forms and the result forms *are*
    # in the scope of the body's declarations (DO*.17).
    frame.install_free_declarations()

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
                    frame.bind(var, new_value)

    with frame, watchdog:
        return _run_with_nil_block(_loop, lisptype.NIL, block_env)


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

    # The implicit NIL block's frame is registered on its own child
    # environment, which adds no variable bindings; `loop_env` chains through
    # it, so the body and result forms (evaluated in `loop_env`) and the
    # list-form (evaluated in `block_env`, still the enclosing lexical
    # environment variable-wise) see the block lexically.
    block_env = lisptype.Environment(env)
    loop_env = lisptype.Environment(block_env)
    frame = BindingFrame(loop_env, body=body, bound_vars=[var])
    _, body = body_specials(body)
    frame.bind(var, lisptype.NIL)

    def _loop():
        # Evaluate list-form in `block_env` -- the enclosing lexical
        # environment for variables (CLHS 6.2.8.2: "in the current lexical
        # environment"), plus this DOLIST's implicit block, which per CLHS
        # 6.1.2.1 surrounds the entire DOLIST form, so a RETURN in the
        # list-form exits it.
        lst = eval(list_form, block_env)

        # Iterate over list
        current_list = lst
        while _consp_internal(current_list):
            frame.bind(var, car(current_list))

            # Execute body
            _exec_iteration_body(body, loop_env)

            current_list = cdr(current_list)

        # Set var to NIL for result form
        frame.bind(var, lisptype.NIL)

        # Evaluate and return result form
        return eval(result_form, loop_env)

    with frame:
        return _run_with_nil_block(_loop, lisptype.NIL, block_env)


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

    # The implicit NIL block's frame is registered on its own child
    # environment, which adds no variable bindings; `loop_env` chains through
    # it, so the body and result forms (evaluated in `loop_env`) and the
    # count-form (evaluated in `block_env`, still the enclosing lexical
    # environment variable-wise) see the block lexically.
    block_env = lisptype.Environment(env)
    loop_env = lisptype.Environment(block_env)
    frame = BindingFrame(loop_env, body=body, bound_vars=[var])
    _, body = body_specials(body)

    def _loop():
        # Evaluate count-form in `block_env` -- the enclosing lexical
        # environment for variables (CLHS 6.2.7.2: "in the current lexical
        # environment"), plus this DOTIMES's implicit block, which per CLHS
        # 6.1.2.1 surrounds the entire DOTIMES form, so a RETURN in the
        # count-form exits it.
        count = eval(count_form, block_env)
        if not isinstance(count, (int, float)):
            count = 0
        count = int(count)
        # CLHS 6.2.7: if count is not a non-negative integer, coerce it to >= 0
        if count < 0:
            count = 0

        # Iterate count times
        for i in range(count):
            frame.bind(var, i)

            # Execute body
            _exec_iteration_body(body, loop_env)

        # Set var to count for result form
        frame.bind(var, count)

        # Evaluate and return result form
        return eval(result_form, loop_env)

    with frame:
        return _run_with_nil_block(_loop, lisptype.NIL, block_env)


# DO-SYMBOLS/DO-EXTERNAL-SYMBOLS/DO-ALL-SYMBOLS are now real macros --
# see standard_macros.py's `_build_package_iteration_expansion` and
# `_do_all_symbols_expander`, which expand into DOLIST over a materialized
# symbol list (CLHS 6.1.2.1.7) and replaced the three near-identical
# hand-rolled loops that used to live here.


@_registry.cl_function('%LOOP-FINISH')
def eval_loop_finish(*args):
    """Terminate the current LOOP immediately, proceeding to FINALLY clauses.

    CLHS 6.1.5: The LOOP-FINISH macro causes the immediate termination
    of a loop and the execution of the loop epilogue (FINALLY clauses).

    This is the *runtime*, `%`-prefixed: CLHS 6.2 makes LOOP-FINISH a
    macro, and it was registered here as a special operator instead. That
    is not a cosmetic distinction -- `loop-finish.error.1` asks a
    surrounding MACROLET for `(macro-function 'loop-finish env)` and then
    FUNCALLs the result at three wrong arities, requiring a PROGRAM-ERROR
    from each. `standard_macros.py` registers the macro that expands to a
    call of this; its `_standard_macro` wrapper is what enforces the
    two-argument macro-function shape those three FUNCALLs violate.
    """
    raise LoopFinishException()


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
    'eval_loop_finish',
]
