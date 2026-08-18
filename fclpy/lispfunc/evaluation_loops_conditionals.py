"""Loops and conditionals: WHEN, COND, AND, OR, PROGN, LET, quasiquote."""

import fclpy.lisptype as lisptype
from .core import car, cdr, _consp_internal, _null_internal, cons
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
    frame = BindingFrame(letstar_env, body=body, bound_vars=bound_vars)

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
                    tail = eval(car(cdr(cur)), env)
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

    # WHILE/UNTIL termination tests (CLHS 6.1.2.1.2). Also composing: a loop may
    # carry several, and they bound whatever drivers are present rather than
    # replacing them.
    #
    # Position matters. A termination test is evaluated where it is written
    # (CLHS 6.1.2.1.2), so `while x collect x` tests before accumulating while
    # `collect x until x` accumulates and then tests -- which is why each entry
    # records whether a main clause had already been seen when it was parsed.
    termination_tests = []  # list of ('while'/'until', test_form, after_body)

    # A WHEN/UNLESS guards *the clause that follows it*, not every clause in the
    # loop (CLHS 6.1.3.1). So the conditions accumulate as `pending_conditionals`
    # and are handed to -- and cleared by -- the next clause that consumes them.
    #
    # These used to be two flat shared lists, `conditionals` and `body_forms`,
    # tested together as `if _conditionals_pass(conditionals): for f in
    # body_forms: ...`. That made `when A do X when B do Y` mean
    # `(and A B) -> X, Y`: every condition guarded every form. When the
    # conditions are mutually exclusive the body then becomes *unreachable*, and
    # in a driverless loop -- whose only exit is a RETURN in its body -- that is
    # an infinite loop. ansi-aux's `is-noncontiguous-sublist-of` is exactly that
    # shape and spun 1.7 million iterations:
    #
    #     (loop when (null list2)            do (return-from ... nil)
    #           when (eql x (pop list2))     do (return))
    #
    # Each entry: {'conditionals': [...], 'forms': [...]} -- the same shape the
    # accumulation clauses below already use, so there is one convention for
    # "a clause carries its own guards" rather than two.
    pending_conditionals = []  # list of ('when'/'unless', test_form)
    body_clauses = []

    # `AND` joins a clause to the previous one *under the same conditional*
    # (CLHS 6.1.3): in
    #
    #     when (evenp x) collect x into foo and count t into bar
    #
    # the `when` guards both accumulations. So an AND-joined clause re-uses the
    # conditions the clause before it consumed instead of taking a fresh (by then
    # empty) set. `AND` previously had no handler at all outside `with`, so it
    # fell through LOOP's silent-drop path -- which is precisely why the old
    # shared-bucket code got these three tests right by accident, and why making
    # the buckets per-clause broke them until AND became real.
    clause_join = {'active': False, 'last': []}

    def take_pending_conditionals():
        """Hand the pending WHEN/UNLESS conditions to the clause consuming them."""
        if clause_join['active']:
            clause_join['active'] = False
            return list(clause_join['last'])
        taken = list(pending_conditionals)
        del pending_conditionals[:]
        clause_join['last'] = taken
        return taken

    def add_body_clause(forms):
        body_clauses.append({'conditionals': take_pending_conditionals(),
                             'forms': list(forms)})

    # Accumulation clauses, in order. CLHS 6.1.3 permits several in one loop
    # (`collect i into foo always (< i 20)`); a single slot silently kept only
    # the last one parsed and discarded the rest.
    # Each entry: {'type', 'form', 'into', 'conditionals'}.
    accumulations = []

    # WITH's local variables (CLHS 6.1.1.4), as a list of *groups*. Successive
    # WITH clauses initialize sequentially -- each one sees the previous -- but
    # the specs an AND joins initialize in parallel, all from the environment
    # outside the loop, which is the distinction LET* and LET make and the
    # reason a flat list would not do.
    # Each spec: {'var', 'type', 'init'} with 'init' None meaning "no = form".
    with_groups = []

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
            candidate_var = forms[i+1]
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
            j, _for_type = _loop_type_spec(forms, i + 2)
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

            iteration_drivers.append({
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
            })

            i = j
            continue

        elif name == 'WHILE':
            termination_tests.append(('while', forms[i+1], bool(body_clauses or accumulations)))
            i += 2

        elif name == 'UNTIL':
            termination_tests.append(('until', forms[i+1], bool(body_clauses or accumulations)))
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
            pending_conditionals.append(('when', forms[i+1]))
            i += 2

        elif name == 'UNLESS':
            pending_conditionals.append(('unless', forms[i+1]))
            i += 2

        elif name == 'AND':
            # A bare AND in clause position joins the next clause to the previous
            # one; `with a = 1 and b = 2` and parallel `for` clauses consume their
            # own ANDs in their own branches before reaching here.
            clause_join['active'] = True
            i += 1

        elif name in ('DO', 'DOING'):
            # Collect body forms until the next clause keyword. The DO clause
            # *consumes* the pending conditionals -- which the comment here used
            # to claim while the code left them in a shared list for every later
            # clause to be guarded by as well.
            i += 1
            do_forms = []
            while i < len(forms) and sym_name(forms[i]) not in LOOP_CLAUSE_KEYWORDS:
                do_forms.append(forms[i])
                i += 1
            add_body_clause(do_forms)

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
                # This clause consumes the pending conditionals, the same way a
                # DO clause does. The test it replaced -- `[] if body_forms else
                # list(conditionals)` -- was reaching for that rule with the only
                # signal available while the buckets were shared: "did some DO
                # anywhere in this loop already take them?"
                'conditionals': take_pending_conditionals(),
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
            # `maximize x fixnum` / `sum x into total of-type integer`: only the
            # numeric accumulations take a trailing type-spec (CLHS 6.1.3.2), so
            # only they may consume one -- `collect x` followed by `t` would
            # otherwise lose the T.
            if clause['type'] in NUMERIC_ACCUMULATIONS:
                i, _acc_type_spec = _loop_type_spec(forms, i)
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
                add_body_clause([token])
            i += 1
    
    # Execute the loop
    result = None
    return_triggered = False  # Flag for when RETURN is executed

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
            acc_states[_key] = {'type': _clause['type'], 'items': [],
                                'number': 0, 'extremum': None}

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

    def execute_iteration_body(loop_env):
        """Execute one iteration of the loop body."""
        nonlocal result, return_triggered

        # Check for RETURN form and evaluate it if present
        if return_form is not None:
            result = eval(return_form, loop_env)
            return_triggered = True
            return

        # Each body clause carries its own WHEN/UNLESS guards, so a condition
        # applies to the clause it precedes and to nothing after it.
        for clause in body_clauses:
            if not _conditionals_pass(clause['conditionals'], loop_env):
                continue
            for f in clause['forms']:
                result = eval(f, loop_env)
                if return_triggered:
                    return

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

            if return_triggered:
                return

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
            if kind == 'for-being-hash':
                # CLHS 6.1.2.1.6. The entries are snapshotted here rather than
                # iterated lazily: the body may add to or remove from the table,
                # and a live Python view would raise "dictionary changed size
                # during iteration" as a Python error leaking into Lisp.
                table = eval(driver['list'], loop_env)
                if not hasattr(table, 'items'):
                    raise lisptype.LispTypeError(
                        datum=table, expected_type='HASH-TABLE',
                        message='LOOP BEING THE HASH-KEYS requires a hash table')
                driver['_items'] = list(table.items())
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
                from .misc_packages import package_symbols
                pkg_spec = driver.get('list')
                pkg = eval(pkg_spec, loop_env) if pkg_spec is not None else None
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
                _bind_varspec(frame, var, eval(value_form, loop_env))
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
        # decision, the same one LET and the DO family now make.
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

        for d in iteration_drivers:
            _init_driver(loop_env, d)

        # INTO names a variable local to the loop (CLHS 6.1.3), so bind it
        # through the frame: the accumulation must not assign through to an
        # outer binding of the same name and clobber it.
        for clause in accumulations:
            if clause['into'] is not None:
                frame.bind(clause['into'], _accumulated_value(_acc_key(clause)))

        # The prologue runs once, after the iteration variables exist and
        # before the first termination test (CLHS 6.1.7.1).
        for f in initially_forms:
            eval(f, loop_env)

        # With no drivers, no termination test and nothing to execute there is
        # nothing to iterate; running would just spin until the hard cap.
        if iteration_drivers or termination_tests or body_clauses or accumulations \
                or (return_form is not None):
            while all(_driver_has_value(d) for d in iteration_drivers):
                check_loop_timeout()

                # Bind before testing. A termination test routinely reads the
                # variable its own driver supplies -- (loop for x = 1 then (* 2 x)
                # while (< x 20) ...) -- so testing first sees either an unbound
                # variable on the first iteration or a stale one thereafter.
                for d in iteration_drivers:
                    _bind_driver(frame, d)

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
        if early_decision['decided']:
            return early_decision['value']

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

    try:
        with loop_watchdog:
            return _run_with_nil_block(_run_loop_and_finalize, loop_block_name)
    finally:
        for frame in loop_frame:
            frame.unwind()


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
    
    # Evaluate all init forms first (parallel binding like LET), in the
    # *enclosing* environment -- DO.16 pins that, since its init form refers to
    # a variable the body then declares special.
    init_values = [eval(init_form, env) for var, init_form, _ in var_specs]

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
    frame = BindingFrame(loop_env, body=body, bound_vars=[var])
    _, body = body_specials(body)
    frame.bind(var, lisptype.NIL)

    def _loop():
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
    frame = BindingFrame(loop_env, body=body, bound_vars=[var])
    _, body = body_specials(body)

    def _loop():
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
        return _run_with_nil_block(_loop)


def eval_do_symbols(form, env):
    """Evaluate DO-SYMBOLS special form.
    
    (DO-SYMBOLS (var [package [result-form]]) declaration* {tag | statement}*)
    
    Iterates over all symbols accessible in the package.
    """
    from .evaluation_core import eval
    
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

    # The symbols *accessible* in the package: its own plus the externals of
    # every package it uses. Enumerated by the shared helper, which is also what
    # LOOP's `for x being the symbols of p` uses -- the copy that used to live
    # here read `external_symbols` straight off each entry of `use_list`, but
    # that list holds package *names* as well as Package objects, so a string
    # entry yielded the empty set and its symbols were silently skipped.
    from .misc_packages import coerce_to_package, package_symbols
    pkg = coerce_to_package(eval(package_form, env))

    # Create loop environment
    loop_env = lisptype.Environment(env)
    frame = BindingFrame(loop_env, body=body, bound_vars=[var])
    _, body = body_specials(body)
    frame.bind(var, lisptype.NIL)

    def _loop():
        for sym in package_symbols(pkg, 'symbols'):
            frame.bind(var, sym)
            _exec_iteration_body(body, loop_env)

        # Set var to NIL for result form
        frame.bind(var, lisptype.NIL)
        return eval(result_form, loop_env)

    with frame:
        return _run_with_nil_block(_loop)


def eval_do_external_symbols(form, env):
    """Evaluate DO-EXTERNAL-SYMBOLS special form.
    
    (DO-EXTERNAL-SYMBOLS (var [package [result-form]]) declaration* {tag | statement}*)
    
    Iterates over all external (exported) symbols in the package.
    """
    from .evaluation_core import eval
    
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

    # Same shared resolver and enumerator as DO-SYMBOLS and LOOP's
    # for-as-package clause; only the symbol set differs.
    from .misc_packages import coerce_to_package, package_symbols
    pkg = coerce_to_package(eval(package_form, env))

    # Create loop environment
    loop_env = lisptype.Environment(env)
    frame = BindingFrame(loop_env, body=body, bound_vars=[var])
    _, body = body_specials(body)
    frame.bind(var, lisptype.NIL)

    def _loop():
        for sym in package_symbols(pkg, 'external-symbols'):
            frame.bind(var, sym)
            _exec_iteration_body(body, loop_env)

        # Set var to NIL for result form
        frame.bind(var, lisptype.NIL)
        return eval(result_form, loop_env)

    with frame:
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
    frame = BindingFrame(loop_env, body=body, bound_vars=[var])
    _, body = body_specials(body)
    frame.bind(var, lisptype.NIL)

    def _loop():
        # Get all unique packages
        unique_packages = {id(p): p for p in state.packages.values()}

        # Iterate over all symbols in all packages
        for pkg in unique_packages.values():
            for name, sym in list(pkg.symbols.items()):
                frame.bind(var, sym)
                _exec_iteration_body(body, loop_env)

        # Set var to NIL for result form
        frame.bind(var, lisptype.NIL)
        return eval(result_form, loop_env)

    with frame:
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
