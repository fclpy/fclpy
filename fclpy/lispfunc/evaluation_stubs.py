"""Stub implementations and auxiliary evaluation functions."""

import fclpy.state as state
import fclpy.lisptype as lisptype
import fclpy.lispenv as lispenv
from .core import car, cdr, cons, _consp_internal, _atom_internal
from . import registry as _registry


# Control flow function stubs (non-special-form versions)
def flet(bindings, *body):
    """Local function binding."""
    raise lisptype.LispNotImplementedError("FLET")


def labels(bindings, *body):
    """Recursive local function binding."""
    raise lisptype.LispNotImplementedError("LABELS")


def handler_bind(bindings, *body):
    """Bind condition handlers."""
    raise lisptype.LispNotImplementedError("HANDLER-BIND")


def handler_case(form, *clauses):
    """Handle conditions with case."""
    raise lisptype.LispNotImplementedError("HANDLER-CASE")


@_registry.cl_function('IGNORE-ERRORS')
def ignore_errors(*body):
    """Ignore errors in body."""
    from .evaluation_core import eval
    try:
        result = None
        for form in body:
            result = eval(form)
        return result
    except:
        return None


@_registry.cl_function('PROG1')
def prog1(first_form, *forms):
    """Return first argument after (stub) evaluating remaining forms."""
    return first_form


@_registry.cl_function('PROG2')
def prog2(first_form, second_form, *forms):
    """Return second argument after (stub) evaluating remaining forms."""
    return second_form


@_registry.cl_function('LAMBDA')
def lambda_fn(*args):
    """LAMBDA special form (function stub for registration).
    
    The actual LAMBDA handling is done by eval_lambda in evaluation_special_forms.py.
    This stub is registered so that LAMBDA appears as a function binding.
    """
    raise lisptype.LispNotImplementedError("LAMBDA should be handled by eval_lambda")


def progn(*forms):
    """Evaluate forms sequentially, return last form's value."""
    from .evaluation_core import eval
    result = None
    for form in forms:
        result = eval(form)
    return result


def with_open_file(args, *body):
    """Execute body with open file."""
    raise lisptype.LispNotImplementedError("WITH-OPEN-FILE")


def loop_finish():
    """Finish loop execution."""
    raise lisptype.LispNotImplementedError("LOOP-FINISH")


def inline_decl(*args):
    """Inline declaration."""
    raise lisptype.LispNotImplementedError("INLINE")


def ignore(*args):
    """Ignore declaration."""
    return None


def ignorable(*args):
    """Ignorable declaration."""
    return None


@_registry.cl_function('DEFINE-MODIFY-MACRO')
def define_modify_macro(name, lambda_list, function, **kwargs):
    """Define modify macro."""
    raise lisptype.LispNotImplementedError("DEFINE-MODIFY-MACRO")


@_registry.cl_function('SET')
def set(symbol, value):
    """Set the value of a symbol (dynamic variable)."""
    # For now, just return the value - proper symbol table management later
    return value


@_registry.cl_function('BOUNDP')
def boundp(symbol):
    """Test if symbol has a value binding."""
    # For now, assume most symbols are bound - proper implementation later
    return lisptype.T


@_registry.cl_function('MAKUNBOUND')
def makunbound(symbol):
    """Make symbol unbound."""
    # For now, just return the symbol - proper implementation later
    return symbol


@_registry.cl_function('VALUES')
def values(*args):
    """Return multiple values.
    
    (VALUES a b c) returns three values: a, b, and c.
    When no arguments are given, returns NIL.
    When one argument is given, returns that value directly.
    When multiple arguments are given, returns a MultipleValues wrapper.
    """
    if not args:
        return lisptype.NIL
    elif len(args) == 1:
        return args[0]
    else:
        # Return as MultipleValues wrapper for multiple values
        return lisptype.MultipleValues(*args)


@_registry.cl_function('VALUES-LIST')
def values_list(lst):
    """Return multiple values from a list.
    
    (VALUES-LIST '(a b c)) returns three values: a, b, and c.
    This is essentially the inverse of MULTIPLE-VALUE-LIST.
    """
    if lst is lisptype.NIL or lst is None:
        return lisptype.NIL
    
    # Convert list to MultipleValues
    return lisptype.MultipleValues.from_list(lst)


@_registry.cl_function('MULTIPLE-VALUE-LIST')
def multiple_value_list(values):
    """Convert multiple values to a list.
    
    (MULTIPLE-VALUE-LIST (VALUES a b c)) returns (a b c) as a list.
    If the input is a single value, wraps it in a list.
    If the input is a MultipleValues, converts all values to a list.
    """
    if isinstance(values, lisptype.MultipleValues):
        return values.to_list()
    elif values is None:
        return lisptype.NIL
    elif values is lisptype.NIL:
        return lisptype.lispCons(lisptype.NIL, lisptype.NIL)
    else:
        # Single value wraps in a list
        return lisptype.lispCons(values, lisptype.NIL)


@_registry.cl_function('NTH-VALUE')
def nth_value(n, values):
    """Extract the Nth value from multiple values.
    
    (NTH-VALUE 0 (VALUES a b c)) returns a
    (NTH-VALUE 1 (VALUES a b c)) returns b
    (NTH-VALUE 3 (VALUES a b c)) returns NIL
    """
    # Coerce n to int
    if not isinstance(n, int):
        raise lisptype.LispTypeError(f"NTH-VALUE: index must be an integer, got {type(n)}")
    
    if n < 0:
        return lisptype.NIL
    
    if isinstance(values, lisptype.MultipleValues):
        if n < len(values.values):
            return values.values[n]
        else:
            return lisptype.NIL
    elif values is None or values is lisptype.NIL:
        return lisptype.NIL
    else:
        # Single value: only index 0 is valid
        if n == 0:
            return values
        else:
            return lisptype.NIL


@_registry.cl_function('MULTIPLE-VALUE-PROG1')
def multiple_value_prog1(first_form, *rest):
    """Execute PROG1 with multiple values support (stub).
    
    Evaluates first_form and rest forms, returning values from first_form.
    Full implementation would handle non-local exits and cleanup.
    """
    # For now, just return first_form - proper implementation later
    return first_form


# Aliases for functions that may have different names in lispenv.py
def apply_fn(function, *args):
    """Apply function (alias for apply)."""
    from .evaluation_core import apply
    return apply(function, *args)


# Control flow and type operations
def typecase(keyform, *clauses):
    """Type-based case statement."""
    return None  # Simplified


def etypecase(keyform, *clauses):
    """Exhaustive typecase."""
    return None  # Simplified


def ctypecase(keyform, *clauses):
    """Correctable typecase."""
    return None  # Simplified


@_registry.cl_function('CCASE')
def ccase(keyform, *clauses):
    """Correctable case."""
    return None  # Simplified


@_registry.cl_function('ECASE')
def ecase(keyform, *clauses):
    """Exhaustive case."""
    return None  # Simplified


def eval_fn(form, env=None):
    """Eval function (alias)."""
    from .evaluation_core import eval
    return eval(form, env)


def compile_fn(name, definition=None):
    """Compile function."""
    return None  # Simplified


@_registry.cl_function('THE')
def the(type_spec, form):
    """Type declaration."""
    return form


@_registry.cl_function('LOCALLY')
def locally(*body):
    """Local declarations."""
    result = None
    for form in body:
        result = form
    return result


@_registry.cl_function('DESTRUCTURING-BIND')
def destructuring_bind(lambda_list, expression, *body):
    """Destructuring bind."""
    result = None
    for form in body:
        result = form
    return result


# Assignment and modification operations
@_registry.cl_function('DECF')
def decf(place, delta=1):
    """Decrement place (stub returns numeric result)."""
    return place - delta  # Simplified


@_registry.cl_function('PSETF')
def psetf(*pairs):
    """Parallel setf (stub)."""
    return None  # Simplified


@_registry.cl_function('SETF')
def setf(*pairs):
    """Set place (stub)."""
    return None  # Simplified


@_registry.cl_function('SHIFTF')
def shiftf(*places):
    """Shift places (stub)."""
    return None  # Simplified


@_registry.cl_function('ROTATEF')
def rotatef(*places):
    """Rotate places (stub)."""
    return None  # Simplified


@_registry.cl_function('PSETQ')
def psetq(*pairs):
    """Parallel setq (stub)."""
    return None  # Simplified


@_registry.cl_function('BLOCK')
def block(name, *body):
    """Execute block with optional return-from."""
    # For now, just evaluate body forms in sequence - proper implementation later
    result = None
    for form in body:
        result = form
    return result


@_registry.cl_function('RETURN-FROM')
def return_from(name, value=None):
    """Return from named block."""
    # For now, just return the value - proper implementation later
    return value


@_registry.cl_function('CATCH')
def catch(tag, *body):
    """Catch thrown values."""
    # For now, just evaluate body - proper implementation later
    result = None
    for form in body:
        result = form
    return result


@_registry.cl_function('THROW')
def throw(tag, value=None):
    """Throw value to catch."""
    # For now, just return the value - proper implementation later
    return value


@_registry.cl_function('TAGBODY')
def tagbody(*forms):
    """Execute forms with tags for GO."""
    # For now, just evaluate non-tag forms - proper implementation later
    result = None
    for form in forms:
        if not isinstance(form, (str, int)):  # Skip tags
            result = form
    return result


@_registry.cl_function('GO')
def go(tag):
    """Go to tag in tagbody."""
    # For now, just return None - proper implementation later
    return None


@_registry.cl_function('UNWIND-PROTECT')
def unwind_protect(protected_form, *cleanup_forms):
    """Execute protected form with cleanup."""
    # For now, just execute protected form - proper implementation later
    return protected_form


@_registry.cl_function('AND')
def and_fn(*args):
    """Logical AND of arguments."""
    result = True
    for arg in args:
        result = arg
        if not arg:
            return None
    return result


@_registry.cl_function('OR')
def or_fn(*args):
    """Logical OR of arguments."""
    for arg in args:
        if arg:
            return arg
    return None


@_registry.cl_function('PROG')
def prog(*body):
    """Execute prog block."""
    # For now, just evaluate forms - proper implementation later
    result = None
    for form in body:
        result = form
    return result


@_registry.cl_function('WHEN')
def when_fn(test, *body):
    """Execute body if test is true."""
    if test:
        result = None
        for form in body:
            result = form
        return result
    return None


@_registry.cl_function('UNLESS')
def unless_fn(test, *body):
    """Execute body if test is false."""
    if not test:
        result = None
        for form in body:
            result = form
        return result
    return None


@_registry.cl_function('CASE')
def case_fn(keyform, *clauses):
    """Case statement."""
    # For now, return None - proper implementation later
    return None


@_registry.cl_function('COND')
def cond_fn(*clauses):
    """Conditional statement."""
    # For now, return None - proper implementation later
    return None


@_registry.cl_function('DO')
def do_fn(*args):
    """Do loop."""
    # For now, return None - proper implementation later
    return None


@_registry.cl_function('DOLIST')
def dolist(*args):
    """Dolist loop."""
    # For now, return None - proper implementation later
    return None


@_registry.cl_function('DOTIMES')
def dotimes(*args):
    """Dotimes loop."""
    # For now, return None - proper implementation later
    return None


@_registry.cl_function('LOOP')
def loop_fn(*args):
    """Simple LOOP implementation that accepts common clauses.

    Supports forms such as:
      (LOOP FOR i FROM 0 TO 9 BY 1 DO (PRINT i))
      (LOOP WHILE <test> DO <forms>)
      (LOOP UNTIL <test> DO <forms>)
      (LOOP REPEAT <n> DO <forms>)

    This is not a complete implementation of ANSI LOOP, but covers common cases.
    loop_fn is invoked as a function/macro with raw unevaluated args; here we
    accept either raw cons arguments or Python sequences and interpret them.
    """
    from .evaluation_core import eval
    
    # Normalize args into a Python list of forms
    forms = []
    # If the macro was passed a single cons wrapping the clause list, unwrap it
    if len(args) == 1 and _consp_internal(args[0]):
        cur = args[0]
        while _consp_internal(cur):
            clause = car(cur)
            # clause is expected to be a cons whose car is the clause keyword
            if _consp_internal(clause):
                # append the clause head symbol
                forms.append(car(clause))
                # append each element of the clause tail as a single form
                tail = cdr(clause)
                while _consp_internal(tail):
                    forms.append(car(tail))
                    tail = cdr(tail)
            else:
                forms.append(clause)
            cur = cdr(cur)
    else:
        for a in args:
            forms.append(a)

    # Simple parser: look for keywords FOR, FROM, TO, BY, DO, WHILE, UNTIL, REPEAT
    i = 0
    results = None

    def eval_body_list(body_forms, env):
        res = None
        for f in body_forms:
            res = eval(f, env)
        return res

    # If forms start with a single body, just evaluate it repeatedly? handle trivial case
    # Parse a single clause at a time
    env_for_loop = None
    while i < len(forms):
        token = forms[i]
        # Treat token names (symbols) by uppercase name
        name = token.name if isinstance(token, lisptype.LispSymbol) else None

        if name == 'FOR':
            # Expect: FOR <var> FROM <start> TO <end> [BY <step>] DO <body...>
            var = forms[i+1]
            if not isinstance(var, lisptype.LispSymbol):
                raise lisptype.LispNotImplementedError('LOOP FOR requires a symbol')
            # default values
            start = 0
            end = None
            step = 1
            j = i+2
            while j < len(forms):
                f = forms[j]
                fname = f.name if isinstance(f, lisptype.LispSymbol) else None
                if fname == 'FROM':
                    start = eval(forms[j+1])
                    j += 2
                elif fname == 'TO':
                    end = eval(forms[j+1])
                    j += 2
                elif fname == 'BY':
                    step = eval(forms[j+1])
                    j += 2
                elif fname == 'DO':
                    # body consumes rest until next top-level clause; take remaining as body
                    body = []
                    k = j+1
                    while k < len(forms):
                        body.append(forms[k])
                        k += 1
                    # run loop
                    # create local environment for loop variables
                    loop_env = lisptype.Environment(lispenv.current_environment)
                    loop_env.add_variable(var, start)
                    val = None
                    cur = start
                    # inclusive loop when end is provided
                    if end is None:
                        # no end -> single evaluation
                        val = eval_body_list(body, loop_env)
                    else:
                        # iterate
                        if step == 0:
                            raise lisptype.LispNotImplementedError('LOOP BY step cannot be 0')
                        compare = (lambda a, b: a <= b) if step > 0 else (lambda a, b: a >= b)
                        while compare(cur, end):
                            loop_env.set_variable(var, cur)
                            val = eval_body_list(body, loop_env)
                            cur = cur + step
                    results = val
                    return results
                else:
                    # Unexpected token, break
                    break
            i = j

        elif name == 'WHILE':
            # (LOOP WHILE <test> DO <body...>)
            test = forms[i+1]
            # find DO
            j = i+2
            while j < len(forms) and not (isinstance(forms[j], lisptype.LispSymbol) and forms[j].name == 'DO'):
                j += 1
            body = []
            k = j+1
            while k < len(forms) and not (isinstance(forms[k], lisptype.LispSymbol) and forms[k].name in ('WHILE','UNTIL','REPEAT','FOR')):
                body.append(forms[k]); k += 1

            # execute
            res = None
            while eval(test):
                res = eval_body_list(body, lispenv.current_environment)
            return res

        elif name == 'UNTIL':
            test = forms[i+1]
            # find DO
            j = i+2
            while j < len(forms) and not (isinstance(forms[j], lisptype.LispSymbol) and forms[j].name == 'DO'):
                j += 1
            body = []
            k = j+1
            while k < len(forms) and not (isinstance(forms[k], lisptype.LispSymbol) and forms[k].name in ('WHILE','UNTIL','REPEAT','FOR')):
                body.append(forms[k]); k += 1

            res = None
            while True:
                res = eval_body_list(body, lispenv.current_environment)
                if eval(test):
                    break
            return res

        elif name == 'REPEAT':
            # (LOOP REPEAT <n> DO <body...>)
            count = eval(forms[i+1])
            # find DO
            j = i+2
            while j < len(forms) and not (isinstance(forms[j], lisptype.LispSymbol) and forms[j].name == 'DO'):
                j += 1
            body = []
            k = j+1
            while k < len(forms):
                body.append(forms[k]); k += 1

            res = None
            for _ in range(count):
                res = eval_body_list(body, lispenv.current_environment)
            return res

        else:
            # Unrecognized - try to eval as single body
            return eval(token)

    return results

# Mark LOOP implementation as a macro so evaluator will call it with raw args
setattr(loop_fn, '__is_macro__', True)


def load_fn(filename, **kwargs):
    """Load file."""
    # For now, return None - proper implementation later
    return None


__all__ = [
    'flet',
    'labels',
    'handler_bind',
    'handler_case',
    'ignore_errors',
    'prog1',
    'prog2',
    'lambda_fn',
    'progn',
    'with_open_file',
    'loop_finish',
    'inline_decl',
    'ignore',
    'ignorable',
    'define_modify_macro',
    'set',
    'boundp',
    'makunbound',
    'values',
    'values_list',
    'multiple_value_list',
    'nth_value',
    'multiple_value_prog1',
    'apply_fn',
    'typecase',
    'etypecase',
    'ctypecase',
    'ccase',
    'ecase',
    'eval_fn',
    'compile_fn',
    'the',
    'locally',
    'destructuring_bind',
    'decf',
    'psetf',
    'setf',
    'shiftf',
    'rotatef',
    'psetq',
    'block',
    'return_from',
    'catch',
    'throw',
    'tagbody',
    'go',
    'unwind_protect',
    'and_fn',
    'or_fn',
    'prog',
    'when_fn',
    'unless_fn',
    'case_fn',
    'cond_fn',
    'do_fn',
    'dolist',
    'dotimes',
    'loop_fn',
    'load_fn',
]
