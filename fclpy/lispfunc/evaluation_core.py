"""Core Lisp evaluation system - eval, apply, and dispatch.

This module contains the main eval() function that dispatches to special forms
and the apply() function for function application.
"""

import logging

import fclpy.state as state
import fclpy.lisptype as lisptype
import fclpy.lispreader as lispreader
from .core import car, cdr, cons, _consp_internal, _atom_internal
import fclpy.lispenv as lispenv  # environment setup utilities
from fclpy.lisptype import resolve_environment, LispEnvironmentError
import inspect
from functools import lru_cache
from typing import NamedTuple
import sys
from fclpy import classes

# Register special operator handlers into the builtin registry
from . import registry as _registry
from . import arrays as _arrays
import fclpy.lispfunc as lispfunc

logger = logging.getLogger(__name__)


def is_arity_mismatch_message(error_str):
    """True if a Python `TypeError` string names a call-arity problem.

    Calling a Python callable with the wrong number of arguments is how a
    Lisp function invoked with the wrong number of arguments looks from
    here; CLHS says that signals PROGRAM-ERROR, not a raw Python exception
    reaching a Lisp value (plan.md finding X1). One predicate shared by
    every call site that converts a `TypeError` into a condition, so the
    definition of "looks like an arity error" cannot drift between them.
    """
    low = error_str.lower()
    return (('missing' in low and 'argument' in low)
            or ('takes' in low and 'argument' in low)
            or ('positional argument' in low))


class LambdaListShape(NamedTuple):
    """A builtin's ANSI lambda list, as read off its Python signature.

    Python can express every part of a CLHS ordinary lambda list, and the
    mapping is exact once you use the whole of Python's parameter model:

    ==========================  ===========================================
    ANSI                        Python
    ==========================  ===========================================
    required                    positional, no default
    ``&optional``               positional-or-keyword **with** a default
    ``&rest``                   ``*args``
    ``&key``                    **keyword-only** with a default
    ``&allow-other-keys``       ``**kwargs``
    ==========================  ===========================================

    The distinction that matters is the middle two rows, and conflating them
    is what made `(union nil nil :bad t)` return an answer: this used to read
    *every* defaulted parameter as a `&key` name, so it could not tell a
    genuine `&key` function from one whose trailing arguments are `&optional`,
    and had to *guess* whether a keyword-shaped value in a trailing position
    was a keyword argument or an `&optional` value (`(intern "a" :cl-test)`
    passes :CL-TEST as a package designator, not a stray keyword). Guessing
    means the CLHS 3.4.1.4/3.5.1.5 conformance checks cannot be applied at
    all: an unrecognized keyword became a positional argument instead of a
    PROGRAM-ERROR.

    `declared_keys` non-empty is therefore the question "can this call be
    validated?". A builtin whose `&key` parameters are spelled keyword-only
    has declared them exactly, and `split_keyword_args` enforces the standard
    against that declaration. One that has not been migrated yet falls back to
    the old inference -- see plan.md section 5; the families are being
    converted cluster by cluster, and `legacy_keys` is what the unconverted
    ones still match against.
    """
    num_required: int
    num_optional: int
    declared_keys: frozenset
    legacy_keys: frozenset
    wildcard: bool
    has_var_positional: bool

    @property
    def accepted_keys(self):
        return self.declared_keys | self.legacy_keys


_NO_LAMBDA_LIST = LambdaListShape(0, 0, frozenset(), frozenset(), False, False)


# Cache for function signature information to avoid repeated inspect.signature calls
@lru_cache(maxsize=1024)
def _get_func_signature_info(func_id: int, func):
    """The cached `LambdaListShape` of a Python callable."""
    try:
        params = list(inspect.signature(func).parameters.values())
    except (ValueError, TypeError):
        return _NO_LAMBDA_LIST

    positional = (inspect.Parameter.POSITIONAL_ONLY,
                  inspect.Parameter.POSITIONAL_OR_KEYWORD)
    num_required = num_optional = 0
    declared_keys = set()
    legacy_keys = set()
    wildcard = has_var_positional = False
    for p in params:
        if p.kind == inspect.Parameter.VAR_POSITIONAL:
            has_var_positional = True
        elif p.kind == inspect.Parameter.VAR_KEYWORD:
            wildcard = True
        elif p.kind == inspect.Parameter.KEYWORD_ONLY:
            declared_keys.add(p.name.lower())
        elif p.kind in positional:
            if p.default is inspect.Parameter.empty:
                num_required += 1
            else:
                num_optional += 1
                legacy_keys.add(p.name.lower())
    return LambdaListShape(num_required, num_optional, frozenset(declared_keys),
                           frozenset(legacy_keys), wildcard, has_var_positional)



def _eval_args(args, env):
    """Evaluate a place's argument forms, left to right, into a Python list.

    A place's subforms are evaluated once, in order (CLHS 5.1.1.1); the copies
    this replaced each evaluated exactly the first *two* of them, which is why
    an array place could only ever take one subscript.
    """
    values = []
    current = args
    while _consp_internal(current):
        values.append(eval(car(current), env))
        current = cdr(current)
    return values


def get_func_signature_info(func):
    """Get signature info for a function, using cached helper."""
    return _get_func_signature_info(id(func), func)


def split_keyword_args(func, values):
    """Split already-evaluated call arguments into (positional_args, kwargs).

    CLHS 3.4.1.4: a keyword/value pair is recognized once every required
    positional parameter is filled, and applies identically whether the
    call arrived as a direct function-call form, FUNCALL, or APPLY -- this
    is the one place that decision is made, so all three call sites agree.
    A user-defined LAMBDA/DEFUN closure takes `*call_args` with no
    `**kwargs` and parses its own `&key` parameters from the positional
    stream, so `use_kwargs` is false for it and this is a no-op passthrough
    -- only built-in `**kwargs`-accepting callables are affected.

    :ALLOW-OTHER-KEYS is itself always a recognized keyword once keyword
    processing has started, whether or not the callee declares it by name,
    and (CLHS 3.4.1.4) a true value for it -- its *leftmost* occurrence, per
    3.4.1.4.1 -- suppresses the unrecognized-keyword-argument error for
    every other pair in this same call, rather than the pair silently
    becoming positional arguments (which is how an unrecognized keyword
    used to turn into a spurious arity mismatch instead of the answer the
    call actually asked for, or into no error at all). A callee with its
    own `**kwargs` (`'*' in kwarg_param_names`) gets :ALLOW-OTHER-KEYS
    forwarded like any other keyword instead of consumed here, because a
    wildcard callee can't be validated from outside -- WRITE is exactly
    this: its own finite printer-keyword set isn't visible to signature
    introspection, so `io_write._print_keywords` does its own CLHS 3.4.1.4
    check and needs to see the keyword itself.

    Where the keyword region *begins* is decided by the callee's lambda
    list, not by what the arguments look like: it is after every required
    and `&optional` parameter (`LambdaListShape`). That is what lets
    `(intern "a" :cl-test)` pass :CL-TEST as `package`'s value -- INTERN's
    `package` is `&optional`, so index 1 is still positional -- while
    `(union nil nil :bad t)`, whose callee has only `&key` parameters after
    its two required ones, is a PROGRAM-ERROR for the unrecognized :BAD.
    Both used to be decided by a *guess* keyed on whether the call already
    contained some recognizable keyword; the guess had to fail one way or
    the other, and it failed by letting an unrecognized keyword become a
    positional argument.

    Inside the keyword region CLHS 3.4.1.4/3.5.1.5 applies in full:
    every pair's name must be a **symbol** (not necessarily a keyword --
    3.4.1.4.1.1 admits any symbol, which is why `(member 'b '(a b c)
    :allow-other-keys 17 :allow-other-keys nil '#:x t)` is legal), an odd
    count is a PROGRAM-ERROR, the *leftmost* pair wins for a repeated name
    and for :ALLOW-OTHER-KEYS itself, and a name the callee does not
    declare is a PROGRAM-ERROR unless :ALLOW-OTHER-KEYS is true.
    """
    shape = get_func_signature_info(func)
    if shape.has_var_positional or not (shape.declared_keys or shape.wildcard
                                        or shape.legacy_keys):
        # &rest swallows everything, and a callee with no keyword parameters
        # at all has nothing to pair -- either way this is a passthrough. A
        # user-defined LAMBDA/DEFUN closure lands here and parses its own
        # lambda list.
        return list(values), {}
    if shape.declared_keys:
        return _split_declared_keywords(shape, values)
    return _split_inferred_keywords(shape, values)


def _keyword_region_name(value):
    """The parameter name a value in a keyword position denotes, or None.

    CLHS 3.4.1.4.1.1: with &allow-other-keys in play the name need only be a
    *symbol*, so a keyword, an interned symbol (`'bad`) and an uninterned one
    (`'#:x`) are all well-formed names here; only a non-symbol is malformed.
    """
    if isinstance(value, (lisptype.lispKeyword, lisptype.LispSymbol)):
        return value.name
    return None


def _split_declared_keywords(shape, values):
    """CLHS 3.4.1.4 against a callee that declared its `&key` parameters."""
    n = len(values)
    boundary = min(shape.num_required + shape.num_optional, n)
    pos_args = list(values[:boundary])
    rest = values[boundary:]
    if len(rest) % 2:
        # CLHS 3.5.1.6.
        raise lisptype.LispProgramError(
            f"odd number of keyword arguments: {rest[-1]!r} has no value")

    pairs = [(rest[i], rest[i + 1]) for i in range(0, len(rest), 2)]
    names = []
    for name, _value in pairs:
        spelling = _keyword_region_name(name)
        if spelling is None:
            raise lisptype.LispProgramError(
                f"{name!r} is not a valid keyword argument name")
        names.append(spelling)

    # CLHS 3.4.1.4.1: the leftmost :ALLOW-OTHER-KEYS pair governs, wherever
    # it appears -- so it is read before any other pair is judged.
    allow_other_keys = shape.wildcard
    for spelling, (_name, value) in zip(names, pairs):
        if spelling == 'ALLOW-OTHER-KEYS':
            allow_other_keys = lisptype.is_truthy(value)
            break

    kwargs = {}
    for spelling, (_name, value) in zip(names, pairs):
        if spelling == 'ALLOW-OTHER-KEYS' and not shape.wildcard:
            # Recognized whether or not the callee names it, and consumed
            # rather than forwarded.
            continue
        py_key = spelling.lower().replace('-', '_')
        if shape.wildcard or py_key in shape.declared_keys:
            # Leftmost pair wins for a repeated name too.
            kwargs.setdefault(py_key, value)
        elif not allow_other_keys:
            raise lisptype.LispProgramError(
                f"unrecognized keyword argument: {spelling}")
    return pos_args, kwargs


def _split_inferred_keywords(shape, values):
    """The pre-migration fallback for a callee whose `&key` parameters are
    still spelled as defaulted *positional* parameters (plan.md section 5).

    It cannot tell an unrecognized keyword from an `&optional` value, so it
    treats a keyword-shaped value as a keyword argument only on evidence: a
    genuine :ALLOW-OTHER-KEYS pair somewhere in the call, or a name the callee
    actually accepts. Anything else falls through to positional, which is a
    silently wrong answer where the standard wants a PROGRAM-ERROR -- the
    reason the migration exists.
    """
    accepted = shape.accepted_keys
    pos_args = []
    kwargs = {}
    n = len(values)

    allow_other_keys = False
    saw_marker = False
    j = shape.num_required
    while j < n and not shape.wildcard:
        v = values[j]
        if isinstance(v, lisptype.lispKeyword) and j + 1 < n:
            if v.name == 'ALLOW-OTHER-KEYS':
                allow_other_keys = lisptype.is_truthy(values[j + 1])
                saw_marker = True
                break
            j += 2
            continue
        break

    i = 0
    while i < n:
        value = values[i]
        if (isinstance(value, lisptype.lispKeyword)
                and len(pos_args) >= shape.num_required):
            py_key = value.name.lower().replace('-', '_')
            # A `**kwargs` callee cannot be validated from outside -- its own
            # keyword set is invisible to signature introspection -- so every
            # keyword is forwarded, :ALLOW-OTHER-KEYS included, and the callee
            # does its own CLHS 3.4.1.4 check (io_write._print_keywords).
            recognized = shape.wildcard or py_key in accepted
            is_marker = (value.name == 'ALLOW-OTHER-KEYS'
                         and not shape.wildcard)
            if not (recognized or is_marker or saw_marker):
                pos_args.append(value)
                i += 1
                continue
            if i + 1 >= n:
                raise lisptype.LispProgramError(
                    f"odd number of keyword arguments: {value.name} "
                    f"has no value")
            if is_marker:
                i += 2
                continue
            if recognized:
                if py_key not in kwargs:
                    kwargs[py_key] = values[i + 1]
                i += 2
                continue
            if allow_other_keys:
                i += 2
                continue
            raise lisptype.LispProgramError(
                f"unrecognized keyword argument: {value.name}")
        pos_args.append(value)
        i += 1
    return pos_args, kwargs


class ReturnFromException(Exception):
    """Exception raised by RETURN-FROM to exit a BLOCK."""
    def __init__(self, tag, value):
        self.tag = tag
        self.value = value
        super().__init__(f"RETURN-FROM {tag.name if hasattr(tag, 'name') else tag}")


class ThrowException(Exception):
    """Exception raised by THROW when tag not caught."""
    def __init__(self, tag, value):
        self.tag = tag
        self.value = value
        super().__init__(f"Uncaught THROW {tag.name if hasattr(tag, 'name') else tag}")


class GoException(Exception):
    """Exception raised by GO to jump to a tag in TAGBODY."""
    def __init__(self, tag):
        self.tag = tag
        super().__init__(f"GO {tag.name if hasattr(tag, 'name') else tag}")


class HandlerCaseTag:
    """A unique CATCH tag identifying one HANDLER-CASE (or IGNORE-ERRORS)
    frame, used as the tag of a HandlerCaseTransfer.

    `__eq__` is identity and never returns NotImplemented, so the tag
    comparison in `eval_catch` can never be answered by a Lisp object's own
    `__eq__` on the other side: an intervening (CATCH 'FOO ...) sees this tag,
    finds it does not match, and re-raises, exactly as it should.
    """
    __slots__ = ()

    def __eq__(self, other):
        return self is other

    def __hash__(self):
        return id(self)

    def __repr__(self):
        return f"#<handler-case-tag {id(self):#x}>"


class HandlerCaseTransfer(ThrowException):
    """Carries control from a HANDLER-CASE handler back to its own frame.

    CLHS defines HANDLER-CASE as HANDLER-BIND plus a transfer of control out to
    the HANDLER-CASE form: the handler runs at the signal point like any other
    handler, transfers control here, and only then -- after unwinding, with the
    handlers disestablished -- is the clause body evaluated. See
    lispfunc/evaluation_conditions.py for the establishing side.

    It subclasses ThrowException because it *is* a throw to a dynamically
    established tag, and because that makes every place in the evaluator that
    already lets a THROW pass through untouched (APPLY, FUNCALL, the special
    forms' control-transfer re-raise clauses) do the right thing for it
    automatically. A new, unrelated exception class would have had to be added
    to each of those tuples by hand, and any site that was missed would
    silently convert a handler transfer into an error -- the same defect class
    as plan.md Finding K.
    """

    def __init__(self, tag, clause, condition):
        super().__init__(tag, condition)
        self.clause = clause
        self.condition = condition


class ConditionException(Exception):
    """Exception raised when a Lisp condition is signaled.

    This exception wraps a condition object and is used to communicate
    error/warning conditions through the Python exception system.
    """
    def __init__(self, condition, recoverable=False):
        """Initialize a condition exception.

        Args:
            condition: A Condition object from lisptype
            recoverable: If True, the condition has a continue restart
        """
        if not isinstance(condition, lisptype.Condition):
            # Every ERROR/SIGNAL/CERROR/WARN call site is supposed to build a
            # real condition object before raising, but a caller that fails
            # to do so (e.g. a datum whose type wasn't recognized) would
            # otherwise smuggle an arbitrary Python object through as "the
            # condition" -- _condition_matches can never match it against
            # any handler type, not even (ERROR (C) ...) or T, so it escapes
            # every enclosing HANDLER-CASE/HANDLER-BIND uncaught (see plan.md
            # Finding E: "a Python object surfacing as a Lisp value is
            # always a bug"). Wrap it in a generic ERROR condition instead,
            # so it is always at least catchable as an ERROR.
            condition = lisptype.Error(message=str(condition))
        self.condition = condition
        self.recoverable = recoverable
        super().__init__(str(condition))


def expand_deftype(entry, args):
    """Expand a DEFTYPE'd type specifier to the type it denotes (CLHS 4.2.3).

    `entry` is what the DEFTYPE special form stored in the global environment's
    `user_types`; `args` are the compound specifier's arguments (empty for an
    atomic reference to the type name). Called by `fclpy.typespec`, which is the
    one reader of that table.

    Only the expander's *primary* value is the type -- `deftype.17` defines an
    expander whose body is `(values 'integer t)` and requires the type to be
    INTEGER -- so a MultipleValues result is reduced here rather than being
    handed on as a type specifier.
    """
    expander = entry.get('expander')
    if expander is None:
        raise lisptype.LispError('DEFTYPE %r has no expander' % (entry.get('name'),))
    result = expander(*args)
    if isinstance(result, lisptype.MultipleValues):
        return result.values[0] if result.values else lisptype.NIL
    return result


def parse_lambda_list(lambda_list):
    """Parse a Common Lisp lambda list into structured form.

    Returns a dict with keys:
    - required: list of required parameter symbols
    - optional: list of optional parameter specs (symbol or [symbol, default])
    - rest: single rest parameter symbol or None
    - keyword: list of keyword parameter specs (symbol or [symbol, default])
    - aux: list of auxiliary parameter specs (symbol or [symbol, init])
    - environment: single environment parameter symbol or None
    
    Supported format:
    (req1 req2 &optional opt1 (opt2 default2) &rest rest-var 
     &key key1 (key2 default2) &aux (aux1 init1) &environment env)
    """
    required = []
    optional = []
    rest = None
    keyword = []
    aux = []
    environment = None
    allow_other_keys = False
    
    # Parse the lambda list
    current_section = 'required'
    current = lambda_list
    whole = None

    while _consp_internal(current):
        param = car(current)

        # Check for section markers
        if isinstance(param, lisptype.LispSymbol):
            marker = param.name.upper()
            if marker == '&OPTIONAL':
                current_section = 'optional'
                current = cdr(current)
                continue
            elif marker == '&REST' or marker == '&BODY':
                # &BODY is a Common Lisp synonym for &REST
                current_section = 'rest'
                current = cdr(current)
                continue
            elif marker == '&KEY':
                current_section = 'keyword'
                current = cdr(current)
                continue
            elif marker == '&AUX':
                current_section = 'aux'
                current = cdr(current)
                continue
            elif marker == '&ALLOW-OTHER-KEYS':
                # Not informational: it is what suppresses the CLHS 3.5.1.5
                # error for a keyword argument the lambda list does not name.
                # Discarding it here left the binder unable to tell a function
                # that accepts any keyword from one that does not.
                allow_other_keys = True
                current = cdr(current)
                continue
            elif marker == '&WHOLE':
                # &WHOLE takes a single following symbol which is bound to the
                # entire macro form; consume that symbol and record it.
                next_sym = car(cdr(current)) if _consp_internal(cdr(current)) else None
                if isinstance(next_sym, lisptype.LispSymbol):
                    whole = next_sym
                # Advance past &WHOLE and its parameter
                current = cdr(cdr(current))
                continue
            elif marker == '&ENVIRONMENT':
                # &ENVIRONMENT takes a single following symbol which is bound to
                # the lexical environment; consume that symbol and record it.
                next_sym = car(cdr(current)) if _consp_internal(cdr(current)) else None
                if isinstance(next_sym, lisptype.LispSymbol):
                    environment = next_sym
                # Advance past &ENVIRONMENT and its parameter
                current = cdr(cdr(current))
                continue

        # Add parameter to appropriate section
        if current_section == 'required':
                if isinstance(param, lisptype.LispSymbol):
                    required.append(param)
                elif _consp_internal(param):
                    # Allow destructuring parameter specs (lists) to be included
                    # as required parameters so callers like (arg1 (&whole w arg2))
                    # are preserved for later destructuring binding.
                    required.append(param)
        elif current_section == 'optional':
            if isinstance(param, lisptype.LispSymbol):
                optional.append(param)
            elif _consp_internal(param):
                # Optional with default: (name default)
                optional.append(param)
        elif current_section == 'rest':
            # &REST may be followed by a simple symbol or a destructuring
            # specification like (name . tail). Preserve the spec as-is
            # so callers can handle destructuring.
            if isinstance(param, lisptype.LispSymbol) or _consp_internal(param):
                rest = param
                current_section = 'after_rest'  # After &REST, expect &KEY or &AUX
        elif current_section == 'keyword':
            if isinstance(param, lisptype.LispSymbol):
                keyword.append(param)
            elif _consp_internal(param):
                # Keyword with default: (name default)
                keyword.append(param)
        elif current_section == 'aux':
            if isinstance(param, lisptype.LispSymbol):
                aux.append(param)
            elif _consp_internal(param):
                # Aux with init: (name init)
                aux.append(param)

        current = cdr(current)

    # A destructuring lambda list may end in a dotted pair whose tail is a
    # single variable (CLHS 3.4.4), e.g. `(a b . rest)`, instead of an
    # explicit &REST/&BODY keyword. A proper list ends this loop with NIL
    # (however NIL is represented); anything else left in `current` is that
    # dotted-tail variable.
    if rest is None and isinstance(current, lisptype.LispSymbol) and current.name.upper() != 'NIL':
        rest = current

    # Include whole and environment in returned structure so macro handling can bind them
    return {
        'required': required,
        'optional': optional,
        'rest': rest,
        'keyword': keyword,
        'aux': aux,
        'whole': whole,
        'environment': environment,
        'allow_other_keys': allow_other_keys,
    }


def bind_destructuring_pattern(pattern, value, env):
    """Bind a Common Lisp *destructuring lambda list* pattern against a value.

    CLHS 3.4.4/3.4.5: a destructuring pattern is a symbol (bound directly to
    the whole value), NIL (nothing bound), or a list which may itself contain
    any of the lambda-list-keyword sections (&WHOLE/&OPTIONAL/&REST/&BODY/
    &KEY/&AUX) or end in a dotted tail -- and any variable name occupying any
    of those sections may itself be a nested pattern of the same shape. One
    recursive walk handles every level by reusing parse_lambda_list, so a
    nested pattern is parsed exactly like a top-level one instead of a second,
    partial copy that only understands the shapes its author had in mind.

    Shared by DEFMACRO/MACROLET's parameter binding and DESTRUCTURING-BIND,
    which destructure the same grammar (CLHS 3.4.4) into two different
    environments -- callers pass whichever `env` the bindings belong in.
    """
    if isinstance(pattern, lisptype.LispSymbol) and pattern.name.upper() != 'NIL':
        env.add_variable(pattern, value if value is not None else lisptype.NIL)
        return
    if not _consp_internal(pattern):
        return

    parsed = parse_lambda_list(pattern)

    whole = parsed.get('whole')
    if whole is not None:
        bind_destructuring_pattern(whole, value, env)

    cur = value
    for p in parsed.get('required', []):
        if _consp_internal(cur):
            v = car(cur)
            cur = cdr(cur)
        else:
            v = lisptype.NIL
        bind_destructuring_pattern(p, v, env)

    for opt in parsed.get('optional', []):
        if isinstance(opt, lisptype.LispSymbol):
            name, default_form, supplied_p = opt, None, None
        elif _consp_internal(opt):
            name = car(opt)
            rest_spec = cdr(opt)
            default_form = car(rest_spec) if _consp_internal(rest_spec) else None
            rest_spec2 = cdr(rest_spec) if _consp_internal(rest_spec) else None
            supplied_p = car(rest_spec2) if _consp_internal(rest_spec2) else None
        else:
            continue

        if _consp_internal(cur):
            v = car(cur)
            cur = cdr(cur)
            bind_destructuring_pattern(name, v, env)
            if supplied_p is not None:
                env.add_variable(supplied_p, lisptype.T)
        else:
            default_value = eval(default_form, env) if default_form is not None else lisptype.NIL
            bind_destructuring_pattern(name, default_value, env)
            if supplied_p is not None:
                env.add_variable(supplied_p, lisptype.NIL)

    rest_param = parsed.get('rest')
    if rest_param is not None:
        bind_destructuring_pattern(rest_param, cur, env)

    for kw in parsed.get('keyword', []):
        if _consp_internal(kw):
            key_name_spec = car(kw)
            rest_spec = cdr(kw)
            default_form = car(rest_spec) if _consp_internal(rest_spec) else None
            rest_spec2 = cdr(rest_spec) if _consp_internal(rest_spec) else None
            supplied_p = car(rest_spec2) if _consp_internal(rest_spec2) else None
        else:
            key_name_spec = kw
            default_form = None
            supplied_p = None

        if isinstance(key_name_spec, lisptype.LispSymbol):
            kw_name = key_name_spec.name.upper()
            var_pattern = key_name_spec
        elif _consp_internal(key_name_spec):
            kw_sym = car(key_name_spec)
            tail = cdr(key_name_spec)
            var_pattern = car(tail) if _consp_internal(tail) else None
            kw_name = kw_sym.name.upper() if isinstance(kw_sym, (lisptype.LispSymbol, lisptype.lispKeyword)) else None
        else:
            continue

        found = False
        tmpk = cur
        while _consp_internal(tmpk):
            k = car(tmpk)
            rest_k = cdr(tmpk)
            v = car(rest_k) if _consp_internal(rest_k) else lisptype.NIL
            if isinstance(k, lisptype.lispKeyword) and k.name.upper() == kw_name:
                bind_destructuring_pattern(var_pattern, v, env)
                if supplied_p is not None:
                    env.add_variable(supplied_p, lisptype.T)
                found = True
                break
            tmpk = cdr(rest_k) if _consp_internal(rest_k) else lisptype.NIL
        if not found:
            default_value = eval(default_form, env) if default_form is not None else lisptype.NIL
            bind_destructuring_pattern(var_pattern, default_value, env)
            if supplied_p is not None:
                env.add_variable(supplied_p, lisptype.NIL)

    for aux in parsed.get('aux', []):
        if isinstance(aux, lisptype.LispSymbol):
            env.add_variable(aux, lisptype.NIL)
        elif _consp_internal(aux):
            aux_name = car(aux)
            rest_spec = cdr(aux)
            init_form = car(rest_spec) if _consp_internal(rest_spec) else None
            init_value = eval(init_form, env) if init_form is not None else lisptype.NIL
            bind_destructuring_pattern(aux_name, init_value, env)


@_registry.cl_function('EVAL')
def eval_function(*args):
    """Common Lisp EVAL function - evaluates a form in the null lexical environment.

    EVAL takes exactly one argument: the form to evaluate.
    Per ANSI Common Lisp spec, EVAL has signature: (eval form) => result*

    Signals a PROGRAM-ERROR condition if the wrong number of arguments is provided.
    """
    # Validate argument count - EVAL takes exactly one argument
    if len(args) != 1:
        if len(args) == 0:
            cond = lisptype.ProgramError(message="EVAL requires exactly one argument: the form to evaluate")
        else:
            cond = lisptype.ProgramError(message=f"EVAL takes exactly one argument, got {len(args)}")
        raise ConditionException(cond, recoverable=False)

    form = args[0]
    # Call the internal eval with null lexical environment
    return eval(form, None)


def eval(form, env=None):
    """Internal evaluation function - evaluates a Lisp form in the given environment.

    This is the internal workhorse function. User code should call the EVAL function above.
    """
    # Import special form handlers lazily to avoid circular imports
    from .evaluation_special_forms import (
        eval_if, eval_setq, eval_defun, eval_defmacro, eval_macroexpand_1,
        eval_macro_function, eval_lambda, eval_declare, eval_declaim,
        eval_defvar, eval_defparameter, eval_defconstant, eval_defstruct, eval_pop, eval_push, eval_pushnew, eval_remf,
        eval_incf, eval_decf, eval_defclass, eval_defgeneric, eval_defmethod, eval_define_method_combination,
        eval_call_method, eval_make_method,
        eval_destructuring_bind, eval_psetq, eval_rotatef, eval_pprint_logical_block
    )
    from .evaluation_control_flow import (
        eval_block, eval_return_from, eval_catch, eval_throw,
        eval_unwind_protect, eval_tagbody, eval_go
    )
    from .evaluation_loops_conditionals import (
        eval_when, eval_unless, eval_cond, eval_case, eval_ccase, eval_and, eval_or,
        eval_progn, eval_locally, eval_prog1, eval_prog2, eval_prog, eval_prog_star, eval_time, eval_let, eval_letstar, eval_quasiquote,
        eval_loop, eval_eval_when, eval_do, eval_do_star, eval_dolist, eval_dotimes,
        eval_do_symbols, eval_do_external_symbols, eval_do_all_symbols,
        eval_flet, eval_labels,
        eval_ecase, eval_typecase, eval_etypecase, eval_ctypecase
    )
    from .utilities_functions import eval_progv
    from .evaluation_conditions import (
        eval_signal, eval_error, eval_cerror, eval_warn,
        eval_restart_case, eval_restart_bind, eval_invoke_restart, eval_abort,
        eval_multiple_value_call, eval_multiple_value_bind, eval_multiple_value_setq,
        eval_multiple_value_prog1,
        eval_handler_bind, eval_handler_case, eval_ignore_errors,
        eval_define_condition
    )
    
    env = resolve_environment(env)
    
    # Self-evaluating forms
    # Normalize Python-level sentinels into Lisp equivalents to avoid type
    # mismatches (e.g., Python True vs Lisp symbol T) leaking into Lisp code.
    if form is None:
        return lisptype.NIL
    if isinstance(form, bool):
        return lisptype.T if form else lisptype.NIL
    if isinstance(form, (int, float, str)):
        return form
    # Keywords are self-evaluating in Common Lisp semantics
    if isinstance(form, lisptype.lispKeyword):
        return form
    
    # Symbols - look up in environment
    if isinstance(form, lisptype.LispSymbol):
        # Check for symbol-macros first (SYMBOL-MACROLET)
        symbol_macro_expansion = env.get_symbol_macro(form)
        if symbol_macro_expansion is not None:
            # Expand the symbol-macro and evaluate the result
            return eval(symbol_macro_expansion, env)
        
        # Check variable bindings first - use has_variable to handle None values
        if env.has_variable(form):
            return env.find_variable(form)
        # Fall back to the symbol's own global/dynamic value cell -- the same
        # cell SET/SYMBOL-VALUE/BOUNDP/PROGV read and write -- for special
        # variables that have no lexical shadow in the current environment
        # chain.
        if getattr(form, 'value', None) is not None:
            return form.value
        # If not found as variable, check function bindings
        value = env.find_func(form)
        if value is not None:
            return value
        # As a fallback, consult the function registry and auto-install
        # bindings into the environment if a registered function exists but
        # hasn't been wired into this Environment instance yet.
        try:
            py_name = _registry.get_function_py_name(form.name)
            if py_name:
                # Ensure the standard environment and lispfunc exports are loaded
                try:
                    lispenv.setup_standard_environment()
                except Exception:
                    logger.error("Error during standard environment setup for function registry lookup for symbol: %s", form.name, exc_info=True)
                fn = getattr(lispfunc, py_name, None)
                if fn is None:
                    # Try importing common lispfunc submodules to resolve circular imports
                    try:
                        import importlib
                        for sub in ('core', 'math', 'sequences', 'vectors', 'streams', 'pathnames', 'hashtables', 'evaluation', 'comparison', 'characters', 'io', 'io_read', 'io_write', 'utilities', 'classes', 'misc_macros'):
                            try:
                                mod = importlib.import_module(f'fclpy.lispfunc.{sub}')
                                fn = getattr(mod, py_name, None)
                                if fn:
                                    # expose on package module for future lookups
                                    try:
                                        setattr(lispfunc, py_name, fn)
                                    except Exception:
                                        logger.error("Error setting attribute on lispfunc module for symbol: %s", form.name, exc_info=True)
                                    break
                            except Exception:
                                logger.warning("Import error during function registry lookup for symbol: %s in submodule: %s", form.name, sub, exc_info=True)
                                continue
                    except Exception:
                        logger.error("Error during function registry lookup for symbol: %s", form.name, exc_info=True)
                if fn:
                    # Bind into environment for faster future lookups
                    env.add_function(form, fn)
                    return fn
        except Exception:
            # Defensive: if registry lookup fails, ignore and raise below
            logger.error("Error during function registry lookup for symbol: %s", form.name, exc_info=True)
        # Create an UnboundVariable condition and raise as a ConditionException
        try:
            name_slot = form if isinstance(form, lisptype.LispSymbol) else getattr(form, 'name', str(form))
        except Exception:
            name_slot = getattr(form, 'name', str(form))
        cond = lisptype.UnboundVariable(name=name_slot, message=f"Unbound variable: {getattr(form, 'name', str(form))} with value {getattr(form, 'value', None)}")
        raise ConditionException(cond, recoverable=False)
    
    # Lists - function calls or special forms
    if _consp_internal(form):
        operator = car(form)
        args = cdr(form)
        
        # Check for special forms
        if isinstance(operator, lisptype.LispSymbol):
            if operator.name == 'QUOTE':
                return car(args)
            elif operator.name == 'IF':
                return eval_if(form, env)
            elif operator.name == 'FUNCTION':
                # (FUNCTION name) - look up the function
                args = cdr(form)
                if args is None or args == lisptype.NIL:
                    raise lisptype.LispError("FUNCTION requires exactly one argument")
                name = car(args)
                # Handle lambda expressions
                if isinstance(name, lisptype.lispCons):
                    op = car(name)
                    if isinstance(op, lisptype.LispSymbol) and op.name == 'LAMBDA':
                        return eval_lambda(name, env)
                # Handle symbol names
                if isinstance(name, lisptype.LispSymbol):
                    # First look in environment
                    func = env.find_func(name)
                    if func is not None:
                        return func
                    # Then check the registry
                    py_name = _registry.get_function_py_name(name.name)
                    if py_name:
                        try:
                            lispenv.setup_standard_environment()
                        except Exception:
                            pass
                        import importlib
                        import fclpy.lispfunc as lispfunc_module
                        func = getattr(lispfunc_module, py_name, None)
                        if func is None:
                            for sub in ('core', 'math', 'sequences', 'vectors', 'streams', 'pathnames', 'hashtables', 'evaluation', 'comparison', 'characters', 'io', 'io_read', 'io_write', 'utilities', 'classes', 'misc_macros'):
                                try:
                                    mod = importlib.import_module(f'fclpy.lispfunc.{sub}')
                                    func = getattr(mod, py_name, None)
                                    if func is not None:
                                        try:
                                            setattr(lispfunc_module, py_name, func)
                                        except Exception:
                                            pass
                                        break
                                except Exception:
                                    continue
                        if func is not None:
                            return func
                        # Signal an UNDEFINED-FUNCTION condition
                        cond = lisptype.UndefinedFunction(name=name.name if isinstance(name, lisptype.LispSymbol) else str(name))
                        raise ConditionException(cond, recoverable=False)
                return name
            elif operator.name == 'SETQ':
                return eval_setq(form, env)
            elif operator.name == 'SETF':
                # SETF is a generalized assignment macro/special form
                # (SETF place value) or (SETF place1 value1 place2 value2 ...)
                # For simple variable places, behave like SETQ
                # For complex places (CAR, CDR, AREF, etc.), call appropriate setter
                from . import evaluation_special_forms as _es_forms
                args = cdr(form)
                result = lisptype.NIL
                
                while _consp_internal(args) and _consp_internal(cdr(args)):
                    place = car(args)
                    value_form = car(cdr(args))

                    # CLHS 5.1.2.8: a symbol place may name a symbol-macro
                    # (SYMBOL-MACROLET), in which case SETF operates on the
                    # macro's expansion instead -- `setf-symbol-macro.*`.
                    while isinstance(place, lisptype.LispSymbol):
                        expansion = env.get_symbol_macro(place)
                        if expansion is None:
                            break
                        place = expansion

                    if isinstance(place, lisptype.LispSymbol):
                        # Simple variable assignment - like SETQ. Only the
                        # primary value is stored (and returned) -- CLHS
                        # 5.1.1; `setf.4` observes this directly.
                        result = lisptype.primary_value(eval(value_form, env))
                        env.set_variable(place, result)
                    elif _consp_internal(place):
                        # Complex place like (CAR x), (CDR x), (AREF arr i), (SLOT-VALUE obj slot), etc.
                        place_op = car(place)
                        if isinstance(place_op, lisptype.LispSymbol):
                            op_name = place_op.name
                            place_args = cdr(place)

                            # VALUES/THE/APPLY-as-place, a DEFSETF long-form
                            # / DEFINE-SETF-EXPANDER registration, or a
                            # macro place all need their subforms resolved
                            # *before* the value-form (CLHS 5.1.1) -- unlike
                            # the legacy branches below, which evaluate the
                            # value-form first (a known, separate gap; see
                            # plan.md). `_place_accessor`'s GET-SETF-
                            # EXPANSION bridge is the one place all of this
                            # is implemented, so route to it before falling
                            # into the legacy ladder rather than duplicating
                            # any of it here.
                            if _es_forms._setf_needs_expansion_bridge(op_name, place_op, env):
                                # CLHS 5.1.1: "the value returned by SETF is
                                # the primary value yielded by evaluating
                                # the storing form" -- not necessarily the
                                # value-form's own value (a long-form
                                # DEFSETF's body may compute something
                                # else entirely, as `defsetf.7a` pins).
                                getter, setter = _es_forms._place_accessor(place, env)
                                new_value = eval(value_form, env)
                                result = setter(new_value)
                                args = cdr(cdr(args))
                                continue

                            result = eval(value_form, env)

                            if op_name == 'CAR':
                                target = eval(car(place_args), env)
                                if _consp_internal(target):
                                    target.car = result
                                else:
                                    raise lisptype.LispError("SETF CAR: target is not a cons")
                            elif op_name == 'CDR':
                                target = eval(car(place_args), env)
                                if _consp_internal(target):
                                    target.cdr = result
                                else:
                                    raise lisptype.LispError("SETF CDR: target is not a cons")
                            elif _arrays.is_array_place(op_name):
                                # (SETF (AREF arr i j) val), (SETF (FILL-POINTER v) n), ...
                                _arrays.array_place_write(
                                    op_name, _eval_args(place_args, env), result)
                            elif op_name in ('CHAR', 'SCHAR'):
                                # (SETF (CHAR str i) val) - now works with LispString
                                seq = eval(car(place_args), env)
                                idx = eval(car(cdr(place_args)), env)
                                try:
                                    seq[idx] = result
                                except (TypeError, IndexError) as e:
                                    raise lisptype.LispError(f"SETF {op_name}: {e}")
                            elif op_name == 'ELT':
                                # (SETF (ELT seq i) val) - works on lists and vectors
                                seq = eval(car(place_args), env)
                                idx = eval(car(cdr(place_args)), env)
                                if _consp_internal(seq):
                                    # List - walk to nth element
                                    current = seq
                                    for _ in range(idx):
                                        if not _consp_internal(current):
                                            raise lisptype.LispError("SETF ELT: index out of bounds")
                                        current = cdr(current)
                                    if _consp_internal(current):
                                        current.car = result
                                    else:
                                        raise lisptype.LispError("SETF ELT: index out of bounds")
                                else:
                                    # Vector/array/LispString - use indexing
                                    try:
                                        seq[idx] = result
                                    except (TypeError, IndexError) as e:
                                        raise lisptype.LispError(f"SETF ELT: {e}")
                            elif op_name == 'GETHASH':
                                key = eval(car(place_args), env)
                                table = eval(car(cdr(place_args)), env)
                                try:
                                    table[key] = result
                                except (TypeError, KeyError) as e:
                                    raise lisptype.LispError(f"SETF GETHASH: {e}")
                            elif op_name == 'SLOT-VALUE':
                                obj = eval(car(place_args), env)
                                slot_name = eval(car(cdr(place_args)), env)
                                if isinstance(obj, classes.LispInstance):
                                    # The one home of "write a CLOS instance's
                                    # slot" is `lispfunc.misc_clos.set_slot_value`,
                                    # the real (SETF SLOT-VALUE) -- CLHS 7.5.3
                                    # protocol and all, including SLOT-MISSING
                                    # for a slot-name the class doesn't define.
                                    # The generic `__dict__` fallback below
                                    # writes a raw Python attribute instead
                                    # (LispInstance has no `set_slot`), which
                                    # SLOT-VALUE's reader then never sees.
                                    from .misc_clos import set_slot_value
                                    set_slot_value(result, obj, slot_name)
                                elif hasattr(obj, 'set_slot'):
                                    obj.set_slot(slot_name, result)
                                elif hasattr(obj, '__dict__'):
                                    slot_key = slot_name.name if isinstance(slot_name, lisptype.LispSymbol) else str(slot_name)
                                    obj.__dict__[slot_key] = result
                                else:
                                    raise lisptype.LispError("SETF SLOT-VALUE: cannot set slot")
                            elif op_name == 'NTH':
                                n = eval(car(place_args), env)
                                lst = eval(car(cdr(place_args)), env)
                                current = lst
                                for _ in range(n):
                                    if not _consp_internal(current):
                                        raise lisptype.LispError("SETF NTH: index out of bounds")
                                    current = cdr(current)
                                if _consp_internal(current):
                                    current.car = result
                                else:
                                    raise lisptype.LispError("SETF NTH: index out of bounds")
                            elif op_name == 'SYMBOL-VALUE':
                                sym = eval(car(place_args), env)
                                if isinstance(sym, lisptype.LispSymbol):
                                    # SYMBOL-VALUE's getter reads sym.value directly
                                    # (the same cell BOUNDP/SET/MAKUNBOUND/PROGV use) --
                                    # its setter must write the same cell, not a
                                    # lexical environment binding.
                                    sym.value = result
                                else:
                                    raise lisptype.LispError("SETF SYMBOL-VALUE: requires a symbol")
                            elif op_name == 'SYMBOL-FUNCTION':
                                sym = eval(car(place_args), env)
                                if isinstance(sym, lisptype.LispSymbol):
                                    env.add_function(sym, result)
                                else:
                                    raise lisptype.LispError("SETF SYMBOL-FUNCTION: requires a symbol")
                            elif op_name == 'FDEFINITION':
                                from .utilities_functions import _function_spec_to_key
                                sym = _function_spec_to_key(eval(car(place_args), env))
                                if sym is not None:
                                    env.add_function(sym, result)
                                else:
                                    raise lisptype.LispError("SETF FDEFINITION: requires a symbol")
                            elif op_name == 'FIND-CLASS':
                                # (SETF (FIND-CLASS name) class) registers
                                # `class` under `name` as an alias -- it
                                # does not rename the class (CLHS), so this
                                # must not go through `register_class`,
                                # which keys off the object's own mutable
                                # `.name` and would have to rename-then-
                                # restore around it -- a dance that
                                # corrupts the registry once two such
                                # aliasing operations touch the same
                                # objects in sequence, e.g. `rotatef.35`.
                                place_name = eval(car(place_args), env)  # e.g., n3
                                if isinstance(place_name, lisptype.LispSymbol):
                                    if isinstance(result, classes.LispClass):
                                        classes.register_class_as(place_name, result)
                                    else:
                                        raise lisptype.LispError("SETF FIND-CLASS: value must be a class")
                                else:
                                    raise lisptype.LispError("SETF FIND-CLASS: place name must be a symbol")
                            elif op_name == 'MACRO-FUNCTION':
                                # (SETF (MACRO-FUNCTION sym) val) should install a macro
                                # Install into the global (root) environment so later
                                # EVAL/MACROEXPAND can find the macro when expanding.
                                sym = eval(car(place_args), env)
                                if isinstance(sym, lisptype.LispSymbol):
                                    global_env = env
                                    while global_env.parent is not None:
                                        global_env = global_env.parent
                                    global_env.add_function(sym, result)
                                    # Also add to current env for immediate visibility
                                    if env is not global_env:
                                        env.add_function(sym, result)
                                else:
                                    raise lisptype.LispError("SETF MACRO-FUNCTION: requires a symbol")
                            elif op_name == 'SYMBOL-PLIST':
                                sym = eval(car(place_args), env)
                                if isinstance(sym, lisptype.LispSymbol):
                                    sym.plist = result
                                else:
                                    raise lisptype.LispError("SETF SYMBOL-PLIST: requires a symbol")
                            elif op_name == 'GET':
                                sym = eval(car(place_args), env)
                                indicator = eval(car(cdr(place_args)), env)
                                # (GET symbol indicator [default]) -- the
                                # optional default form must still be
                                # evaluated (for its side effects), even
                                # though SETF GET never consults its value.
                                default_args = cdr(cdr(place_args))
                                if _consp_internal(default_args):
                                    eval(car(default_args), env)
                                if isinstance(sym, lisptype.LispSymbol):
                                    # `LispSymbol.__init__` defaults `plist`
                                    # to a bare Python `{}`, not NIL --
                                    # `sym.plist is None` never catches that,
                                    # so a fresh symbol's first SETF GET
                                    # appended onto the dict as a dotted
                                    # tail instead of replacing it.
                                    if not _consp_internal(sym.plist):
                                        sym.plist = lisptype.NIL
                                    plist = sym.plist
                                    found = False
                                    current = plist
                                    while _consp_internal(current) and _consp_internal(cdr(current)):
                                        if car(current) == indicator:
                                            cdr(current).car = result
                                            found = True
                                            break
                                        current = cdr(cdr(current))
                                    if not found:
                                        sym.plist = lisptype.lispCons(indicator, lisptype.lispCons(result, sym.plist))
                                else:
                                    raise lisptype.LispError("SETF GET: requires a symbol")
                            elif op_name == 'SUBSEQ':
                                # (SETF (SUBSEQ seq start [end]) new-seq)
                                # For now, just return result (stub - modifying subsequences is complex)
                                pass  # Silently accept but don't actually modify
                            elif op_name in ('FIRST', 'SECOND', 'THIRD', 'FOURTH', 'FIFTH',
                                            'SIXTH', 'SEVENTH', 'EIGHTH', 'NINTH', 'TENTH'):
                                # (SETF (FIRST list) val) etc.
                                lst = eval(car(place_args), env)
                                n = {'FIRST': 0, 'SECOND': 1, 'THIRD': 2, 'FOURTH': 3, 'FIFTH': 4,
                                     'SIXTH': 5, 'SEVENTH': 6, 'EIGHTH': 7, 'NINTH': 8, 'TENTH': 9}[op_name]
                                current = lst
                                for _ in range(n):
                                    if not _consp_internal(current):
                                        raise lisptype.LispError(f"SETF {op_name}: list too short")
                                    current = cdr(current)
                                if _consp_internal(current):
                                    current.car = result
                                else:
                                    raise lisptype.LispError(f"SETF {op_name}: list too short")
                            elif _es_forms._CXR_RE.match(op_name):
                                # Compound CAR/CDR accessors (CLHS 5.1.2.1),
                                # 2-4 letters deep -- `_cxr_target` is the one
                                # place that knows how to navigate them, shared
                                # with PUSH/POP/INCF/ROTATEF's `_place_accessor`.
                                obj = eval(car(place_args), env)
                                parent, is_car = _es_forms._cxr_target(op_name, obj)
                                if is_car:
                                    parent.car = result
                                else:
                                    parent.cdr = result
                            elif op_name == 'GETF':
                                # (SETF (GETF plist indicator [default]) val)
                                # CLHS 5.1.2.6. Previously a bare `pass` --
                                # the assignment was silently discarded
                                # (plan.md standing rule 4). `_place_accessor`
                                # (shared with PUSH/PUSHNEW/INCF/ROTATEF) is
                                # the one home of this place's read/write
                                # pair, including rewriting the plist's own
                                # place when the indicator is new.
                                from .evaluation_special_forms import _place_accessor
                                _, getf_setter = _place_accessor(place, env)
                                getf_setter(result)
                            elif op_name == 'LDB':
                                # (SETF (LDB bytespec int) val) - byte manipulation, skip
                                pass
                            elif op_name == 'MASK-FIELD':
                                # (SETF (MASK-FIELD bytespec int) val)
                                pass
                            elif op_name == 'FILL-POINTER':
                                # (SETF (FILL-POINTER vector) val)
                                vec = eval(car(place_args), env)
                                if hasattr(vec, 'fill_pointer'):
                                    vec.fill_pointer = result
                            elif op_name == 'ROW-MAJOR-AREF':
                                # (SETF (ROW-MAJOR-AREF arr idx) val)
                                arr = eval(car(place_args), env)
                                idx = eval(car(cdr(place_args)), env)
                                try:
                                    arr[idx] = result
                                except (TypeError, IndexError) as e:
                                    raise lisptype.LispError(f"SETF ROW-MAJOR-AREF: {e}")
                            elif op_name == 'BIT':
                                # (SETF (BIT bit-array &rest subscripts) val)
                                arr = eval(car(place_args), env)
                                idx = eval(car(cdr(place_args)), env)
                                try:
                                    arr[idx] = result
                                except (TypeError, IndexError) as e:
                                    raise lisptype.LispError(f"SETF BIT: {e}")
                            elif op_name == 'SBIT':
                                # (SETF (SBIT bit-array &rest subscripts) val)
                                arr = eval(car(place_args), env)
                                idx = eval(car(cdr(place_args)), env)
                                try:
                                    arr[idx] = result
                                except (TypeError, IndexError) as e:
                                    raise lisptype.LispError(f"SETF SBIT: {e}")
                            else:
                                # Check for DEFSETF-defined expander
                                global_env = env
                                while global_env.parent is not None:
                                    global_env = global_env.parent
                                if hasattr(global_env, 'setf_expanders') and op_name in global_env.setf_expanders:
                                    expander = global_env.setf_expanders[op_name]
                                    if expander['type'] == 'short':
                                        update_fn_name = expander['update_fn']
                                        update_fn = env.find_func(update_fn_name)
                                        if update_fn is None:
                                            py_name = _registry.get_function_py_name(update_fn_name.name)
                                            if py_name:
                                                update_fn = getattr(lispfunc, py_name, None)
                                        if update_fn:
                                            eval_place_args = []
                                            pa = place_args
                                            while _consp_internal(pa):
                                                eval_place_args.append(eval(car(pa), env))
                                                pa = cdr(pa)
                                            eval_place_args.append(result)
                                            update_fn(*eval_place_args)
                                        else:
                                            raise lisptype.LispError(f"SETF: update function {update_fn_name} not found")
                                    else:
                                        # Long form - just accept for now
                                        pass
                                else:
                                    # Try generic struct accessor: look for SET-<accessor-name>
                                    setter_name = f"SET-{op_name}"
                                    setter_sym = lisptype.LispSymbol(setter_name)
                                    setter_func = env.find_func(setter_sym)
                                    if setter_func is None:
                                        # Try looking up in registry
                                        py_name = _registry.get_function_py_name(setter_name)
                                        if py_name:
                                            import fclpy.lispfunc as lispfunc
                                            setter_func = getattr(lispfunc, py_name, None)
                                    
                                    if setter_func:
                                        # Call the setter with the target object and all evaluated place args
                                        target_obj = eval(car(place_args), env)
                                        eval_place_args = [target_obj]
                                        pa = cdr(place_args)
                                        while _consp_internal(pa):
                                            eval_place_args.append(eval(car(pa), env))
                                            pa = cdr(pa)
                                        eval_place_args.append(result)
                                        setter_func(*eval_place_args)
                                    else:
                                        # (SETF (accessor arg*) value) where `accessor` has its
                                        # own registered (SETF accessor) function (e.g. via
                                        # (DEFUN (SETF accessor) ...) or (SETF (FDEFINITION
                                        # '(SETF accessor)) ...)). Per ANSI, call it with the
                                        # new value first, then the access-form's arguments.
                                        setf_fn_sym = lisptype.LispSymbol(f"(SETF {op_name})")
                                        setf_fn = env.find_func(setf_fn_sym)
                                        if setf_fn is not None:
                                            eval_place_args = [result]
                                            pa = place_args
                                            while _consp_internal(pa):
                                                eval_place_args.append(eval(car(pa), env))
                                                pa = cdr(pa)
                                            result = setf_fn(*eval_place_args)
                                        # else: unknown place type - silently accept (many exist)
                        else:
                            raise lisptype.LispNotImplementedError(f"SETF: place operator must be a symbol, got {place_op}")
                    else:
                        raise lisptype.LispNotImplementedError(f"SETF: place must be a symbol or form, got {place}")
                    
                    args = cdr(cdr(args))
                
                return result
            elif operator.name == 'PSETF':
                # PSETF is SETF's "read/write" split into two passes (CLHS
                # 5.1.3): every place's subforms *and* value-form are
                # evaluated, left to right, before any assignment happens.
                # `_place_accessor` is the one place-resolution mechanism
                # (shared with SETF/ROTATEF/SHIFTF/PUSH/POP/...) rather
                # than a second, narrower ladder duplicating it -- the old
                # one covered only CAR/CDR/arrays/SYMBOL-FUNCTION/
                # FDEFINITION/FIND-CLASS/NTH/FILL-POINTER/MACRO-FUNCTION
                # and silently did nothing for every other place kind.
                from . import evaluation_special_forms as _es_forms
                args = cdr(form)
                pending = []  # (setter, new_value) pairs, in left-to-right order

                while _consp_internal(args) and _consp_internal(cdr(args)):
                    place = car(args)
                    value_form = car(cdr(args))

                    while isinstance(place, lisptype.LispSymbol):
                        expansion = env.get_symbol_macro(place)
                        if expansion is None:
                            break
                        place = expansion

                    _, setter = _es_forms._place_accessor(place, env)
                    new_value = eval(value_form, env)
                    pending.append((setter, new_value))

                    args = cdr(cdr(args))

                for setter, new_value in pending:
                    setter(new_value)

                return lisptype.NIL
            elif operator.name == 'PROGN':
                return eval_progn(form, env)
            elif operator.name == 'LOCALLY':
                return eval_locally(form, env)
            elif operator.name == 'LET':
                return eval_let(form, env)
            elif operator.name == 'LET*':
                return eval_letstar(form, env)
            elif operator.name == 'FLET':
                return eval_flet(form, env)
            elif operator.name == 'LABELS':
                return eval_labels(form, env)
            elif operator.name == 'WHEN':
                return eval_when(form, env)
            elif operator.name == 'UNLESS':
                return eval_unless(form, env)
            elif operator.name == 'EVAL-WHEN':
                return eval_eval_when(form, env)
            elif operator.name == 'COND':
                return eval_cond(form, env)
            elif operator.name == 'CASE':
                return eval_case(form, env)
            elif operator.name == 'CCASE':
                return eval_ccase(form, env)
            elif operator.name == 'ECASE':
                return eval_ecase(form, env)
            elif operator.name == 'TYPECASE':
                return eval_typecase(form, env)
            elif operator.name == 'ETYPECASE':
                return eval_etypecase(form, env)
            elif operator.name == 'CTYPECASE':
                return eval_ctypecase(form, env)
            elif operator.name == 'PROGV':
                return eval_progv(form, env)
            elif operator.name == '%SPECIAL-REF':
                # Internal helper generated by LOCALLY's (DECLARE (SPECIAL x))
                # handling: read x's dynamic value cell if one has been
                # established (e.g. by PROGV), else fall back to a plain
                # lexical lookup of x in the calling environment.
                sym = car(cdr(form))
                if getattr(sym, 'value', None) is not None:
                    return sym.value
                if env.has_variable(sym):
                    return env.find_variable(sym)
                cond = lisptype.UnboundVariable(name=sym, message=f"Unbound variable: {sym.name}")
                raise ConditionException(cond, recoverable=False)
            elif operator.name == 'MULTIPLE-VALUE-SETQ':
                return eval_multiple_value_setq(form, env)
            elif operator.name == 'MULTIPLE-VALUE-PROG1':
                return eval_multiple_value_prog1(form, env)
            elif operator.name == 'PSETQ':
                return eval_psetq(form, env)
            elif operator.name == 'ROTATEF':
                return eval_rotatef(form, env)
            elif operator.name == 'SHIFTF':
                from . import evaluation_special_forms as _es_forms
                return _es_forms.eval_shiftf(form, env)
            elif operator.name == 'DEFINE-MODIFY-MACRO':
                from . import evaluation_special_forms as _es_forms
                return _es_forms.eval_define_modify_macro(form, env)
            elif operator.name == 'GET-SETF-EXPANSION':
                from . import evaluation_special_forms as _es_forms
                arg_forms = []
                cur = cdr(form)
                while _consp_internal(cur):
                    arg_forms.append(car(cur))
                    cur = cdr(cur)
                if len(arg_forms) < 1 or len(arg_forms) > 2:
                    raise lisptype.LispProgramError(
                        f"GET-SETF-EXPANSION: wrong number of arguments (got {len(arg_forms)}, expected 1-2)")
                place_value = eval(arg_forms[0], env)
                env_value = eval(arg_forms[1], env) if len(arg_forms) == 2 else lisptype.NIL
                if env_value is lisptype.NIL or env_value is None:
                    env_value = env
                temps, vals, stores, store_form, access_form = _es_forms.get_setf_expansion(place_value, env_value)
                return lisptype.MultipleValues(
                    _es_forms._setf_pylist_to_form(temps),
                    _es_forms._setf_pylist_to_form(vals),
                    _es_forms._setf_pylist_to_form(stores),
                    store_form,
                    access_form,
                )
            elif operator.name == 'MULTIPLE-VALUE-LIST':
                # Must see the raw (possibly multiple-valued) result of its
                # argument -- unlike ordinary function-call arguments, this
                # is NOT a single-value context, so it bypasses the generic
                # argument-evaluation loop below (which coerces to primary
                # value).
                from .evaluation_stubs import multiple_value_list as _mvl
                mvl_args = cdr(form)
                if not _consp_internal(mvl_args):
                    raise lisptype.LispNotImplementedError("MULTIPLE-VALUE-LIST requires one form")
                return _mvl(eval(car(mvl_args), env))
            elif operator.name == 'NTH-VALUE':
                # Same rationale as MULTIPLE-VALUE-LIST above: its value-form
                # argument must not be coerced to a single value.
                from .evaluation_stubs import nth_value as _nth_value
                nv_args = cdr(form)
                if not _consp_internal(nv_args) or not _consp_internal(cdr(nv_args)):
                    raise lisptype.LispNotImplementedError("NTH-VALUE requires n and a form")
                n = eval(car(nv_args), env)
                return _nth_value(n, eval(car(cdr(nv_args)), env))
            elif operator.name == 'AND':
                return eval_and(form, env)
            elif operator.name == 'OR':
                return eval_or(form, env)
            elif operator.name == 'PROG1':
                return eval_prog1(form, env)
            elif operator.name == 'PROG2':
                return eval_prog2(form, env)
            elif operator.name == 'PROG':
                return eval_prog(form, env)
            elif operator.name == 'PROG*':
                return eval_prog_star(form, env)
            elif operator.name == 'TIME':
                return eval_time(form, env)
            elif operator.name == 'DEFVAR':
                return eval_defvar(form, env)
            elif operator.name == 'DEFPARAMETER':
                return eval_defparameter(form, env)
            elif operator.name == 'DEFCONSTANT':
                return eval_defconstant(form, env)
            elif operator.name == 'DEFSTRUCT':
                return eval_defstruct(form, env)
            elif operator.name == 'DESTRUCTURING-BIND':
                return eval_destructuring_bind(form, env)
            elif operator.name == 'DEFCLASS':
                return eval_defclass(form, env)
            elif operator.name == 'DEFGENERIC':
                return eval_defgeneric(form, env)
            elif operator.name == 'DEFMETHOD':
                return eval_defmethod(form, env)
            elif operator.name == 'DEFINE-METHOD-COMBINATION':
                return eval_define_method_combination(form, env)
            elif operator.name == 'CALL-METHOD':
                return eval_call_method(form, env)
            elif operator.name == 'MAKE-METHOD':
                return eval_make_method(form, env)
            elif operator.name == 'LOOP':
                return eval_loop(form, env)
            elif operator.name == 'POP':
                return eval_pop(form, env)
            elif operator.name == 'REMF':
                return eval_remf(form, env)
            elif operator.name == 'PUSH':
                return eval_push(form, env)
            elif operator.name == 'PUSHNEW':
                return eval_pushnew(form, env)
            elif operator.name == 'DEFUN':
                return eval_defun(form, env)
            elif operator.name == 'LAMBDA':
                return eval_lambda(form, env)
            elif operator.name == 'QUASIQUOTE':
                return eval_quasiquote(form, env)
            elif operator.name == 'THE':
                from .evaluation_special_forms import eval_the
                return eval_the(form, env)
            elif operator.name == 'DEFMACRO':
                return eval_defmacro(form, env)
            elif operator.name == 'DECLARE':
                return eval_declare(form, env)
            elif operator.name == 'DECLAIM':
                return eval_declaim(form, env)
            elif operator.name == 'PROCLAIM':
                from .evaluation_special_forms import eval_proclaim
                return eval_proclaim(form, env)
            elif operator.name == 'MACROEXPAND-1':
                return eval_macroexpand_1(form, env)
            elif operator.name == 'MACRO-FUNCTION':
                return eval_macro_function(form, env)
            elif operator.name == 'PPRINT-LOGICAL-BLOCK':
                return eval_pprint_logical_block(form, env)
            elif operator.name == 'BLOCK':
                return eval_block(form, env)
            elif operator.name == 'RETURN-FROM':
                return eval_return_from(form, env)
            elif operator.name == 'CATCH':
                return eval_catch(form, env)
            elif operator.name == 'THROW':
                return eval_throw(form, env)
            elif operator.name == 'UNWIND-PROTECT':
                return eval_unwind_protect(form, env)
            elif operator.name == 'MULTIPLE-VALUE-CALL':
                return eval_multiple_value_call(form, env)
            elif operator.name == 'MULTIPLE-VALUE-BIND':
                return eval_multiple_value_bind(form, env)
            elif operator.name == 'SIGNAL':
                return eval_signal(form, env)
            elif operator.name == 'ERROR':
                return eval_error(form, env)
            elif operator.name == 'CERROR':
                return eval_cerror(form, env)
            elif operator.name == 'WARN':
                return eval_warn(form, env)
            elif operator.name == 'RESTART-CASE':
                return eval_restart_case(form, env)
            elif operator.name == 'RESTART-BIND':
                return eval_restart_bind(form, env)
            elif operator.name == 'INVOKE-RESTART':
                return eval_invoke_restart(form, env)
            elif operator.name == 'ABORT':
                return eval_abort(form, env)
            elif operator.name == 'HANDLER-BIND':
                return eval_handler_bind(form, env)
            elif operator.name == 'HANDLER-CASE':
                return eval_handler_case(form, env)
            elif operator.name == 'IGNORE-ERRORS':
                return eval_ignore_errors(form, env)
            elif operator.name == 'TAGBODY':
                return eval_tagbody(form, env)
            elif operator.name == 'GO':
                return eval_go(form, env)
            elif operator.name == 'INCF':
                return eval_incf(form, env)
            elif operator.name == 'DECF':
                return eval_decf(form, env)
            elif operator.name == 'DO':
                return eval_do(form, env)
            elif operator.name == 'DO*':
                return eval_do_star(form, env)
            elif operator.name == 'DOLIST':
                return eval_dolist(form, env)
            elif operator.name == 'DOTIMES':
                return eval_dotimes(form, env)
            elif operator.name == 'DO-SYMBOLS':
                return eval_do_symbols(form, env)
            elif operator.name == 'DO-EXTERNAL-SYMBOLS':
                return eval_do_external_symbols(form, env)
            elif operator.name == 'DO-ALL-SYMBOLS':
                return eval_do_all_symbols(form, env)
            elif operator.name == 'IN-PACKAGE':
                # IN-PACKAGE is a macro in Common Lisp - it doesn't evaluate its argument
                # (in-package #:cl-test) -> call in_package with symbol CL-TEST
                args = cdr(form)
                if args is None or args == lisptype.NIL:
                    raise lisptype.LispError("IN-PACKAGE requires a package designator")
                name_arg = car(args)
                # Don't evaluate - pass the symbol/keyword directly
                from .utilities_symbols import in_package
                return in_package(name_arg)
            

            elif operator.name == 'SYMBOL-MACROLET':
                # (SYMBOL-MACROLET ((sym1 expansion1) (sym2 expansion2) ...) body-form...)
                # Create symbol-macro bindings in a new environment and evaluate body
                args = cdr(form)
                if args is None or args == lisptype.NIL:
                    raise lisptype.LispError("SYMBOL-MACROLET requires bindings")
                
                bindings_form = car(args)
                body_forms = cdr(args)
                
                # Create a new child environment for the symbol-macrolet scope
                new_env = lisptype.Environment(parent=env)
                
                # Process bindings: ((sym1 expansion1) (sym2 expansion2) ...)
                if _consp_internal(bindings_form):
                    binding_list = bindings_form
                    while _consp_internal(binding_list):
                        binding = car(binding_list)
                        if _consp_internal(binding):
                            sym = car(binding)
                            expansion = car(cdr(binding)) if _consp_internal(cdr(binding)) else lisptype.NIL
                            if isinstance(sym, lisptype.LispSymbol):
                                # Store symbol-macro as a special binding
                                # We'll mark it with a wrapper so lookup knows it's a symbol-macro
                                new_env.add_symbol_macro(sym, expansion)
                        binding_list = cdr(binding_list)
                
                # Evaluate body forms in the new environment with symbol-macros active
                result = lisptype.NIL
                body = body_forms
                while _consp_internal(body):
                    form_in_body = car(body)
                    result = eval(form_in_body, new_env)
                    body = cdr(body)
                
                return result
            elif operator.name == 'MACROLET':
                # (MACROLET ((name lambda-list . body) ...) body-form...)
                # Create local macro bindings in a new environment and evaluate body
                args = cdr(form)
                if args is None or args == lisptype.NIL:
                    raise lisptype.LispError("MACROLET requires bindings")
                
                bindings_form = car(args)
                body_forms = cdr(args)
                
                # Create a new child environment for the macrolet scope
                new_env = lisptype.Environment(parent=env)
                
                # Process macro bindings: ((name lambda-list . body) ...)
                if _consp_internal(bindings_form):
                    binding_list = bindings_form
                    while _consp_internal(binding_list):
                        binding = car(binding_list)
                        if _consp_internal(binding):
                            macro_name = car(binding)
                            rest = cdr(binding)
                            if _consp_internal(rest):
                                lambda_list = car(rest)
                                macro_body = cdr(rest)
                                
                                if isinstance(macro_name, lisptype.LispSymbol):
                                    # Create a macro function from the lambda-list and body
                                    # Similar to DEFMACRO but local to this environment
                                    from .evaluation_special_forms import _create_macro_function
                                    macro_func = _create_macro_function(macro_name, lambda_list, macro_body, new_env)
                                    new_env.add_function(macro_name, macro_func)
                        binding_list = cdr(binding_list)
                
                # Evaluate body forms in the new environment with local macros active
                result = lisptype.NIL
                body = body_forms
                while _consp_internal(body):
                    form_in_body = car(body)
                    result = eval(form_in_body, new_env)
                    body = cdr(body)
                
                return result
            elif operator.name == 'DEFSETF':
                # DEFSETF has two forms:
                # Short form: (DEFSETF access-fn update-fn [documentation])
                # Long form:  (DEFSETF access-fn lambda-list (store-var...) [decl] [doc] form...)
                # Arguments should NOT be evaluated - they are symbol names and code templates
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
                
                # Determine if short or long form based on second argument type
                is_short_form = isinstance(second_arg, lisptype.LispSymbol)
                
                # Get or create the global setf-expanders storage
                global_env = env
                while global_env.parent is not None:
                    global_env = global_env.parent
                
                if not hasattr(global_env, 'setf_expanders'):
                    global_env.setf_expanders = {}
                
                if is_short_form:
                    # Short form: (DEFSETF access-fn update-fn [documentation])
                    update_fn = second_arg
                    doc_string = None
                    if _consp_internal(third_and_beyond):
                        doc_form = car(third_and_beyond)
                        if isinstance(doc_form, str):
                            doc_string = doc_form
                    
                    # Store the setf expander info for later use by SETF macro
                    global_env.setf_expanders[access_fn.name] = {
                        'type': 'short',
                        'update_fn': update_fn,
                        'documentation': doc_string
                    }
                else:
                    # Long form: (DEFSETF access-fn lambda-list (store-var...) [decl] [doc] form...)
                    lambda_list = second_arg
                    
                    if not _consp_internal(third_and_beyond):
                        raise lisptype.LispError("DEFSETF long form requires store variables")
                    
                    store_vars = car(third_and_beyond)
                    body = cdr(third_and_beyond)
                    
                    # Parse optional declarations and docstring from body
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
                    
                    # Store the setf expander info for long form
                    global_env.setf_expanders[access_fn.name] = {
                        'type': 'long',
                        'lambda_list': lambda_list,
                        'store_vars': store_vars,
                        'declarations': declarations,
                        'documentation': doc_string,
                        'body': actual_body,
                        'env': env  # Capture lexical environment
                    }
                
                # Return the access-fn symbol as specified by ANSI CL
                return access_fn
            elif operator.name == 'DEFINE-SETF-EXPANDER':
                # (DEFINE-SETF-EXPANDER access-fn lambda-list [[declaration* | documentation]] form*)
                # This is a macro - arguments should NOT be evaluated
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
                
                # Get or create the global setf-expanders storage
                global_env = env
                while global_env.parent is not None:
                    global_env = global_env.parent
                
                if not hasattr(global_env, 'setf_expanders'):
                    global_env.setf_expanders = {}
                
                # Parse optional declarations and docstring from body
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
                
                # Store the setf expander info - similar to long-form DEFSETF
                # but uses &ENVIRONMENT in the lambda list for macro environment
                global_env.setf_expanders[access_fn.name] = {
                    'type': 'expander',
                    'lambda_list': lambda_list,
                    'declarations': declarations,
                    'documentation': doc_string,
                    'body': actual_body,
                    'env': env  # Capture lexical environment
                }
                
                # Return the access-fn symbol as specified by ANSI CL
                return access_fn
            elif operator.name == 'DEFINE-COMPILER-MACRO':
                # (DEFINE-COMPILER-MACRO name lambda-list &body body)
                # Arguments should NOT be evaluated - just return the name
                args = cdr(form)
                if args is None or args == lisptype.NIL:
                    raise lisptype.LispError("DEFINE-COMPILER-MACRO requires a name")
                
                name = car(args)
                # Store compiler macro info if needed (stub for now)
                # Return the name symbol as specified by ANSI CL
                return name
            elif operator.name == 'DEFINE-CONDITION':
                return eval_define_condition(form, env)
            elif operator.name == 'DEFTYPE':
                # (DEFTYPE name lambda-list &body body)
                # Arguments should NOT be evaluated - the name is a symbol
                args = cdr(form)
                if args is None or args == lisptype.NIL:
                    raise lisptype.LispError("DEFTYPE requires a name")
                
                name = car(args)
                if not isinstance(name, lisptype.LispSymbol):
                    raise lisptype.LispError("DEFTYPE: name must be a symbol")
                
                rest = cdr(args)
                lambda_list = car(rest) if _consp_internal(rest) else lisptype.NIL
                body = cdr(rest) if _consp_internal(rest) else lisptype.NIL

                # Get or create the global types storage
                global_env = env
                while global_env.parent is not None:
                    global_env = global_env.parent

                if not hasattr(global_env, 'user_types'):
                    global_env.user_types = {}

                # Store a real *expander*, not the raw source.
                #
                # This dict used to hold `lambda_list`/`body`/`env` and nothing
                # anywhere read it, so a DEFTYPE'd name was invisible to both
                # TYPEP and SUBTYPEP -- `(deftype foo () '(integer 0 10))`
                # succeeded and then `(typep 5 'foo)` was NIL. The expander is
                # built by the one macro-lambda-list binder (CLHS 4.2.3: a
                # deftype lambda list is a macro lambda list, except that an
                # omitted &OPTIONAL/&KEY parameter defaults to `*` rather than
                # NIL, which is what `unsupplied_default` supplies). Reusing it
                # is also what gives DEFTYPE &WHOLE, &REST, &KEY,
                # destructuring, the docstring and the implicit BLOCK that
                # `(return-from <type-name> ...)` needs -- all of which
                # ansi-test's deftype.9-.19 exercise.
                from .evaluation_special_forms import _create_macro_function
                wild = lisptype.COMMON_LISP_PACKAGE.intern('*')
                expander = _create_macro_function(
                    name, lambda_list, body, env, unsupplied_default=wild)

                global_env.user_types[name.name] = {
                    'name': name,
                    'lambda_list': lambda_list,
                    'body': body,
                    'env': env,          # Capture lexical environment
                    'expander': expander,
                }

                # Return the name symbol
                return name
            elif operator.name == 'DEFPACKAGE':
                # DEFPACKAGE's option clauses are literal data (CLHS 7.2), not
                # forms to evaluate -- so this stays a special case in the
                # dispatcher rather than a cl_function, exactly as CLAUDE.md's
                # registry note requires for operators like this one.
                from .misc_packages import (
                    _designator_to_string, shadow as _pkg_shadow,
                    shadowing_import as _pkg_shadowing_import,
                )
                from .utilities_symbols import import_symbol as _pkg_import
                from .evaluation_conditions import signal_error_object as _signal_error

                def _signal_package_error(package, message):
                    # PACKAGE-ERROR lives in the *real* condition hierarchy
                    # (lisptype_extended.Error), not the "legacy"
                    # lisptype.LispError one HANDLER-CASE/IGNORE-ERRORS also
                    # catch directly -- signaling it any other way (a bare
                    # `raise`) skips signal_condition() and every handler, and
                    # the condition object then unwinds as a plain, unmatched
                    # Python exception instead of being caught.
                    _signal_error(lisptype.PackageError(package=package, message=message))

                args = cdr(form)
                if args is None or args == lisptype.NIL:
                    raise lisptype.LispProgramError(
                        "DEFPACKAGE: wrong number of arguments (got 0, expected at least 1)")

                name_arg = car(args)
                opt_forms = cdr(args)

                def _clause_items(r):
                    items = []
                    while _consp_internal(r):
                        items.append(car(r))
                        r = cdr(r)
                    return items

                def _key_name(key):
                    if isinstance(key, (lisptype.lispKeyword, lisptype.LispSymbol)):
                        n = key.name.upper()
                        return n[1:] if n.startswith(':') else n
                    return str(key).upper()

                pkg_name = _designator_to_string(name_arg)

                nicknames = []
                use_names = []
                export_names = []
                intern_names = []
                shadow_names = []
                shadowing_import_clauses = []  # [(pkg_name, [names])]
                import_from_clauses = []       # [(pkg_name, [names])]
                size_seen = False
                doc_seen = False

                cur = opt_forms
                while _consp_internal(cur):
                    clause = car(cur)
                    if _consp_internal(clause):
                        key_name = _key_name(car(clause))
                        rest_items = [_designator_to_string(i) if key_name not in
                                      ('SHADOWING-IMPORT-FROM', 'IMPORT-FROM', 'SIZE', 'DOCUMENTATION')
                                      else i
                                      for i in _clause_items(cdr(clause))]

                        if key_name == 'USE':
                            use_names.extend(rest_items)
                        elif key_name == 'NICKNAMES':
                            nicknames.extend(rest_items)
                        elif key_name == 'INTERN':
                            intern_names.extend(rest_items)
                        elif key_name == 'EXPORT':
                            export_names.extend(rest_items)
                        elif key_name == 'SHADOW':
                            shadow_names.extend(rest_items)
                        elif key_name == 'SHADOWING-IMPORT-FROM':
                            raw = _clause_items(cdr(clause))
                            if not raw:
                                raise lisptype.LispProgramError(
                                    "DEFPACKAGE: :SHADOWING-IMPORT-FROM requires a package name")
                            shadowing_import_clauses.append(
                                (_designator_to_string(raw[0]),
                                 [_designator_to_string(i) for i in raw[1:]]))
                        elif key_name == 'IMPORT-FROM':
                            raw = _clause_items(cdr(clause))
                            if not raw:
                                raise lisptype.LispProgramError(
                                    "DEFPACKAGE: :IMPORT-FROM requires a package name")
                            import_from_clauses.append(
                                (_designator_to_string(raw[0]),
                                 [_designator_to_string(i) for i in raw[1:]]))
                        elif key_name == 'SIZE':
                            if size_seen:
                                raise lisptype.LispProgramError(
                                    "DEFPACKAGE: :SIZE may only be given once")
                            size_seen = True
                        elif key_name == 'DOCUMENTATION':
                            if doc_seen:
                                raise lisptype.LispProgramError(
                                    "DEFPACKAGE: :DOCUMENTATION may only be given once")
                            doc_seen = True
                        # An unrecognized option keyword is left alone rather
                        # than made an error -- CLHS reserves this space for
                        # implementation extensions and no ANSI test exercises
                        # rejecting one.
                    cur = cdr(cur)

                # CLHS 7.2p2: these four options' names must be pairwise
                # disjoint, and :EXPORT/:INTERN must be disjoint -- checked
                # before any mutation so a malformed DEFPACKAGE has no partial
                # effect.
                shadow_set = set(shadow_names)
                shadowing_import_set = {n for _, names in shadowing_import_clauses for n in names}
                import_set = {n for _, names in import_from_clauses for n in names}
                intern_set = set(intern_names)
                export_set = set(export_names)

                def _require_disjoint(name_a, set_a, name_b, set_b):
                    overlap = set_a & set_b
                    if overlap:
                        raise lisptype.LispProgramError(
                            f"DEFPACKAGE: {name_a} and {name_b} must be disjoint "
                            f"(shared {sorted(overlap)!r})")

                _require_disjoint('SHADOW', shadow_set, 'SHADOWING-IMPORT-FROM', shadowing_import_set)
                _require_disjoint('SHADOW', shadow_set, 'IMPORT-FROM', import_set)
                _require_disjoint('SHADOW', shadow_set, 'INTERN', intern_set)
                _require_disjoint('SHADOWING-IMPORT-FROM', shadowing_import_set, 'IMPORT-FROM', import_set)
                _require_disjoint('SHADOWING-IMPORT-FROM', shadowing_import_set, 'INTERN', intern_set)
                _require_disjoint('IMPORT-FROM', import_set, 'INTERN', intern_set)
                _require_disjoint('EXPORT', export_set, 'INTERN', intern_set)

                # A nickname (or the name itself) that already denotes a
                # different existing package is a PACKAGE-ERROR (CLHS
                # MAKE-PACKAGE/DEFPACKAGE); genuinely *continuable* handling
                # needs the restart machinery M8 owns, so this signals the
                # condition honestly rather than silently accepting the clash.
                existing = lisptype.find_package(pkg_name)
                for nn in nicknames:
                    clash = lisptype.find_package(nn)
                    if clash is not None and clash is not existing:
                        _signal_package_error(nn, f"A package named {nn!r} already exists")

                pkg = existing if existing is not None else lisptype.make_package(pkg_name)
                if nicknames:
                    # Merge rather than overwrite: DEFPACKAGE allows multiple
                    # :NICKNAMES clauses, and each contributes its names.
                    merged = list(pkg.nick_names) if existing is not None else []
                    for nn in nicknames:
                        if nn not in merged:
                            merged.append(nn)
                    pkg.nick_names = merged

                use_packages = []
                for use_pkg_name in use_names:
                    use_pkg = lisptype.find_package(use_pkg_name)
                    if use_pkg is None:
                        use_pkg = lisptype.make_package(use_pkg_name)
                    if use_pkg not in use_packages:
                        use_packages.append(use_pkg)
                pkg.use_packages = use_packages

                for sym_name in intern_names:
                    pkg.intern(sym_name, external=False)
                for sym_name in export_names:
                    pkg.intern(sym_name, external=True)
                if shadow_names:
                    _pkg_shadow([lisptype.LispString(n) for n in shadow_names], pkg)

                for src_name, names in shadowing_import_clauses:
                    src_pkg = lisptype.find_package(src_name)
                    if src_pkg is None:
                        _signal_package_error(src_name, f"No package named {src_name!r}")
                    syms = []
                    for n in names:
                        sym, status = src_pkg.find_symbol(n)
                        if sym is None:
                            _signal_package_error(
                                src_name, f"{n!r} is not accessible in package {src_name!r}")
                        syms.append(sym)
                    if syms:
                        _pkg_shadowing_import(syms, pkg)

                for src_name, names in import_from_clauses:
                    src_pkg = lisptype.find_package(src_name)
                    if src_pkg is None:
                        _signal_package_error(src_name, f"No package named {src_name!r}")
                    syms = []
                    for n in names:
                        sym, status = src_pkg.find_symbol(n)
                        if sym is None:
                            _signal_package_error(
                                src_name, f"{n!r} is not accessible in package {src_name!r}")
                        syms.append(sym)
                    if syms:
                        _pkg_import(syms, pkg)

                return pkg
        
        # Macro handling: if operator names a macro function, expand first
        if isinstance(operator, lisptype.LispSymbol):
            func_binding = env.find_func(operator)
            func_binding = lisptype.primary_value(func_binding)
            if callable(func_binding) and getattr(func_binding, '__is_macro__', False):
                # Gather raw args (without evaluating)
                raw_args = []
                current = args
                while _consp_internal(current):
                    raw_args.append(car(current))
                    current = cdr(current)
                try:
                    expects_whole = getattr(func_binding, '__expects_whole__', False)
                    expects_env = getattr(func_binding, '__expects_environment__', False)
                    
                    # Build call arguments based on macro function expectations
                    call_args = []
                    if expects_whole:
                        call_args.append(form)
                    call_args.extend(raw_args)
                    
                    # If macro expects expansion-time environment, append it as trailing arg
                    if expects_env:
                        call_args.append(env)
                    
                    expanded = func_binding(*call_args)
                except TypeError:
                    # Defensive fallback: call without whole/env if signature mismatch
                    if getattr(func_binding, '__expects_whole__', False):
                        expanded = func_binding(form, *raw_args)
                    else:
                        expanded = func_binding(*raw_args)

                # Evaluate the expansion in the current environment
                return eval(expanded, env)

        # Regular function call
        # In Common Lisp, function position uses the FUNCTION namespace, not variable namespace
        if isinstance(operator, lisptype.LispSymbol):
            # Look up in function namespace directly
            func = env.find_func(operator)
            if func is None:
                # Try registry fallback
                try:
                    py_name = _registry.get_function_py_name(operator.name)
                    if py_name:
                        # Ensure environment is populated
                        try:
                            lispenv.setup_standard_environment()
                        except Exception:
                            pass
                        
                        # Try environment lookup again
                        func = env.find_func(operator)
                        
                        # If still not found and we're in a child environment, 
                        # check the parent/global environment
                        if func is None:
                            global_env = env
                            while global_env.parent is not None:
                                global_env = global_env.parent
                            if global_env is not env:
                                func = global_env.find_func(operator)
                        
                        # If STILL not found, try to get it from lispfunc directly
                        if func is None:
                            fn = getattr(lispfunc, py_name, None)
                            if fn is None:
                                # Try importing submodules
                                try:
                                    import importlib
                                    for sub in ('core', 'math', 'sequences', 'vectors', 'streams', 'pathnames', 'hashtables', 'evaluation', 'comparison', 'characters', 'io', 'io_read', 'io_write', 'utilities', 'classes', 'misc_macros'):
                                        try:
                                            mod = importlib.import_module(f'fclpy.lispfunc.{sub}')
                                            fn = getattr(mod, py_name, None)
                                            if fn:
                                                try:
                                                    setattr(lispfunc, py_name, fn)
                                                except Exception:
                                                    pass
                                                break
                                        except Exception:
                                            continue
                                except Exception:
                                    pass
                            if fn:
                                # Add to both current and global environment
                                env.add_function(operator, fn)
                                func = fn
                except Exception:
                    pass
        else:
            # For non-symbol operators (e.g., lambda forms), evaluate to get function
            func = eval(operator, env)
        
        # Multiple values in a single-value context reduce to the primary one.
        func = lisptype.primary_value(func)
        
        # Verify we have a callable function before proceeding
        if func is None or not callable(func):
            # When func is None, it means the symbol has no function binding
            if isinstance(operator, lisptype.LispSymbol):
                # Signal an UNDEFINED-FUNCTION condition so Lisp handlers can match it
                # Per ANSI spec, cell-error-name should return the actual symbol, not just its name string
                cond = lisptype.UndefinedFunction(name=operator, message=f"Undefined function {operator.name if hasattr(operator, 'name') else str(operator)} in package {getattr(operator, 'package', None)}")
                raise ConditionException(cond, recoverable=False)
            raise lisptype.LispError(f"Not a function: {operator}")
        
        # Evaluate arguments left to right. Ordinary function-call arguments
        # are single-value contexts: a MultipleValues result reduces to its
        # primary value (NIL if it returned zero values), per ANSI.
        eval_args = []
        current = args
        while _consp_internal(current):
            eval_args.append(lisptype.primary_value(eval(car(current), env)))
            current = cdr(current)

        # Split evaluated arguments into positionals and &key pairs -- the
        # one shared decision (split_keyword_args), also used by APPLY and
        # FUNCALL so an indirect call recognizes keywords the same way a
        # direct one does.
        eval_args, kwargs = split_keyword_args(func, eval_args)

        # Call function with exception handling
        try:
            if kwargs:
                result = func(*eval_args, **kwargs)
            else:
                result = func(*eval_args)
        except ConditionException:
            # Re-raise Lisp conditions without wrapping them
            raise
        except (ReturnFromException, ThrowException, GoException):
            # Control-flow exceptions used for non-local exits should
            # propagate unchanged so enclosing forms like BLOCK, CATCH,
            # and TAGBODY can handle them.
            raise
        except lisptype.LispProgramError as e:
            # Convert Lisp program errors to PROGRAM-ERROR condition
            condition = lisptype.ProgramError(message=str(e))
            raise ConditionException(condition, recoverable=False)
        except lisptype.LispTypeError as e:
            # Convert Lisp type errors to TYPE-ERROR condition
            condition = lisptype.TypeError(
                datum=getattr(e, 'actual_value', None), 
                expected_type=getattr(e, 'expected_type', None), 
                message=str(e)
            )
            raise ConditionException(condition, recoverable=False)
        except lisptype.LispNotImplementedError as e:
            # Convert not implemented errors to appropriate condition
            condition = lisptype.Error(message=f"Not implemented: {str(e)}")
            raise ConditionException(condition, recoverable=False)
        except lisptype.LispEndOfFileError as e:
            # Convert to END-OF-FILE, not the generic ERROR the LispError
            # branch below would give it -- END-OF-FILE is a STREAM-ERROR
            # subtype (CLHS Figure 9-1) that ansi-test's `signals-error*`
            # checks for by name, so it must survive as that specific type.
            condition = lisptype.EndOfFile(stream=getattr(e, 'stream', None), message=str(e))
            raise ConditionException(condition, recoverable=False)
        except lisptype.LispStreamError as e:
            # Convert to STREAM-ERROR (CLHS 21.1), same reasoning as EOF above.
            condition = lisptype.StreamError(stream=getattr(e, 'stream', None), message=str(e))
            raise ConditionException(condition, recoverable=False)
        except lisptype.LispError as e:
            # Convert other Lisp errors to Error condition
            condition = lisptype.Error(message=str(e))
            raise ConditionException(condition, recoverable=False)
        except TypeError as e:
            # Check if this is an argument count error (function signature problem)
            error_str = str(e)
            if is_arity_mismatch_message(error_str):
                # Argument count mismatch - PROGRAM-ERROR per ANSI CL spec
                condition = lisptype.ProgramError(message=error_str)
                raise ConditionException(condition, recoverable=False)
            else:
                # Other TypeErrors (e.g., type mismatches in function calls)
                condition = lisptype.TypeError(
                    datum=None,
                    expected_type='callable',
                    message=error_str
                )
                raise ConditionException(condition, recoverable=False)
        except AttributeError as e:
            # Handle attribute errors
            condition = lisptype.Error(message=f"Attribute error: {str(e)}")
            raise ConditionException(condition, recoverable=False)
        except ZeroDivisionError as e:
            # Handle division by zero
            condition = lisptype.DivisionByZero(message=str(e))
            raise ConditionException(condition, recoverable=False)
        except ArithmeticError as e:
            # Handle other arithmetic errors
            condition = lisptype.ArithmeticError(message=str(e))
            raise ConditionException(condition, recoverable=False)
        except Exception as e:
            # Catch-all for any other Python exceptions
            condition = lisptype.Error(message=f"Python error in function call: {type(e).__name__}: {str(e)}")
            raise ConditionException(condition, recoverable=False)

        # Normalize common Python return values into Lisp objects.
        if result is None:
            return lisptype.NIL
        if isinstance(result, bool):
            return lisptype.T if result else lisptype.NIL
        return result
    
    return form


def coerce_to_function(function, caller_name='FUNCALL'):
    """Resolve a CLHS "function designator" to a Python callable.

    A function designator is either a function object or a symbol naming a
    function in the function namespace. This is the one place that
    resolution happens; APPLY/FUNCALL use it directly, and every site
    elsewhere that accepts a :test/:test-not/:key/predicate designator
    (FIND, SORT, REDUCE, the set operations, ...) should coerce through
    this instead of assuming its argument is already callable -- see
    plan.md finding X2.
    """
    # If function is MultipleValues, extract the primary value (single-value context)
    if isinstance(function, lisptype.MultipleValues):
        function = function.get_primary()

    # If function is a symbol, look up its function binding
    if isinstance(function, lisptype.LispSymbol):
        env = state.current_environment
        func = env.find_func(function) if env is not None else None
        if func is None:
            raise ConditionException(
                lisptype.UndefinedFunction(name=function.name),
                recoverable=False
            )
        function = func

    # If function is nil or otherwise not callable, signal a PROGRAM-ERROR
    if function is None or function == lisptype.NIL or not callable(function):
        condition = lisptype.ProgramError(message=f"{caller_name} requires a function designator, got: {function}")
        raise ConditionException(condition, recoverable=False)

    return function


@_registry.cl_function('APPLY')
def apply(function, *args):
    """Apply function to arguments."""
    function = coerce_to_function(function, 'APPLY')

    try:
        if args and hasattr(args[-1], '__iter__'):
            # Last argument is a list of arguments
            all_args = list(args[:-1]) + list(args[-1])
        else:
            all_args = list(args)
        # CLHS 3.4.1.4: recognize &key pairs in the flattened argument list
        # the same way a direct call would -- see split_keyword_args.
        pos_args, kwargs = split_keyword_args(function, all_args)
        return function(*pos_args, **kwargs)
    except ConditionException:
        # Re-raise Lisp conditions without wrapping them
        raise
    except (ReturnFromException, ThrowException, GoException):
        # Allow non-local control-flow exceptions to propagate to enclosing
        # Lisp control forms (BLOCK/CATCH/TAGBODY) instead of being wrapped.
        raise
    except lisptype.LispProgramError as e:
        # Convert Lisp program errors to PROGRAM-ERROR condition
        condition = lisptype.ProgramError(message=str(e))
        raise ConditionException(condition, recoverable=False)
    except lisptype.LispTypeError as e:
        # Convert Python-level LispTypeError into Lisp TYPE-ERROR condition
        condition = lisptype.TypeError(datum=getattr(e, 'actual_value', None), expected_type=getattr(e, 'expected_type', None), message=str(e))
        raise ConditionException(condition, recoverable=False)
    except lisptype.LispEndOfFileError as e:
        # END-OF-FILE, not the generic ERROR the LispError branch below
        # would give it -- it is a distinct STREAM-ERROR subtype (CLHS
        # Figure 9-1) that ansi-test's `signals-error*` checks by name.
        condition = lisptype.EndOfFile(stream=getattr(e, 'stream', None), message=str(e))
        raise ConditionException(condition, recoverable=False)
    except lisptype.LispStreamError as e:
        condition = lisptype.StreamError(stream=getattr(e, 'stream', None), message=str(e))
        raise ConditionException(condition, recoverable=False)
    except lisptype.LispError as e:
        # Convert other Lisp errors to Error condition
        condition = lisptype.Error(message=str(e))
        raise ConditionException(condition, recoverable=False)
    except TypeError as e:
        # Check if this is an argument count error (function signature problem)
        error_str = str(e)
        if ('missing' in error_str.lower() and 'argument' in error_str.lower()) or \
           ('takes' in error_str.lower() and 'argument' in error_str.lower()):
            # Argument count mismatch - PROGRAM-ERROR per ANSI CL spec
            condition = lisptype.ProgramError(message=error_str)
            raise ConditionException(condition, recoverable=False)
        else:
            # Other TypeErrors (e.g., type mismatches in function calls)
            condition = lisptype.TypeError(
                datum=None,
                expected_type='callable',
                message=error_str
            )
            raise ConditionException(condition, recoverable=False)
    except Exception as e:
        # Catch-all for any other Python exceptions
        condition = lisptype.Error(message=f"Python error in APPLY: {type(e).__name__}: {str(e)}")
        raise ConditionException(condition, recoverable=False)


@_registry.cl_function('FUNCALL')
def funcall(function, *args):
    """Call function with arguments."""
    function = coerce_to_function(function, 'FUNCALL')

    try:
        # CLHS 3.4.1.4: recognize &key pairs among the call arguments the
        # same way a direct call would -- see split_keyword_args.
        pos_args, kwargs = split_keyword_args(function, list(args))
        return function(*pos_args, **kwargs)
    except ConditionException:
        # Re-raise Lisp conditions without wrapping them
        raise
    except (ReturnFromException, ThrowException, GoException):
        # Allow non-local control-flow exceptions to propagate to enclosing
        # Lisp control forms (BLOCK/CATCH/TAGBODY) instead of being wrapped.
        raise
    except lisptype.LispProgramError as e:
        # Convert Lisp program errors to PROGRAM-ERROR condition
        condition = lisptype.ProgramError(message=str(e))
        raise ConditionException(condition, recoverable=False)
    except lisptype.LispTypeError as e:
        condition = lisptype.TypeError(datum=getattr(e, 'actual_value', None), expected_type=getattr(e, 'expected_type', None), message=str(e))
        raise ConditionException(condition, recoverable=False)
    except lisptype.LispEndOfFileError as e:
        # END-OF-FILE, not the generic ERROR the LispError branch below
        # would give it -- it is a distinct STREAM-ERROR subtype (CLHS
        # Figure 9-1) that ansi-test's `signals-error*` checks by name.
        condition = lisptype.EndOfFile(stream=getattr(e, 'stream', None), message=str(e))
        raise ConditionException(condition, recoverable=False)
    except lisptype.LispStreamError as e:
        condition = lisptype.StreamError(stream=getattr(e, 'stream', None), message=str(e))
        raise ConditionException(condition, recoverable=False)
    except lisptype.LispError as e:
        # Convert other Lisp errors to Error condition
        condition = lisptype.Error(message=str(e))
        raise ConditionException(condition, recoverable=False)
    except TypeError as e:
        # Check if this is an argument count error (function signature problem)
        error_str = str(e)
        if ('missing' in error_str.lower() and 'argument' in error_str.lower()) or \
           ('takes' in error_str.lower() and 'argument' in error_str.lower()):
            # Argument count mismatch - PROGRAM-ERROR per ANSI CL spec
            condition = lisptype.ProgramError(message=error_str)
            raise ConditionException(condition, recoverable=False)
        else:
            # Other TypeErrors (e.g., type mismatches in function calls)
            condition = lisptype.TypeError(
                datum=None,
                expected_type='callable',
                message=error_str
            )
            raise ConditionException(condition, recoverable=False)
    except Exception as e:
        # Catch-all for any other Python exceptions
        condition = lisptype.Error(message=f"Python error in FUNCALL: {type(e).__name__}: {str(e)}")
        raise ConditionException(condition, recoverable=False)


# Aliases for functions that may have different names in lispenv.py
def apply_fn(function, *args):
    """Apply function (alias for apply)."""
    return apply(function, *args)


def eval_fn(form, env=None):
    """Eval function (alias)."""
    return eval(form, env)


__all__ = [
    # Exception classes
    'ReturnFromException',
    'ThrowException',
    'GoException',
    'ConditionException',
    # Core functions
    'parse_lambda_list',
    'eval',
    'apply',
    'funcall',
    'apply_fn',
    'eval_fn',
]
