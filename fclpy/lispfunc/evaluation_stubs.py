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


def prog1(first_form, *forms):
    """Return first argument after (stub) evaluating remaining forms."""
    return first_form


def prog2(first_form, second_form, *forms):
    """Return second argument after (stub) evaluating remaining forms."""
    return second_form


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


def define_modify_macro(name, lambda_list, function, **kwargs):
    """Define modify macro."""
    raise lisptype.LispNotImplementedError("DEFINE-MODIFY-MACRO")


def set(*args):
    """Set the value of a symbol (dynamic variable).

    Accept varargs for ANSI-like error handling; signal PROGRAM-ERROR if
    wrong arity. Current simple implementation returns the provided value.
    """
    if len(args) != 2:
        raise lisptype.LispProgramError(
            f"SET: wrong number of arguments (got {len(args)}, expected 2)"
        )
    symbol, value = args
    # If symbol-like object, set its value cell
    if hasattr(symbol, 'value'):
        try:
            symbol.value = value
        except Exception:
            pass
    return value


def boundp(*args):
    """Test if symbol has a value binding.

    Accept varargs for ANSI-like error handling; signal PROGRAM-ERROR if
    wrong arity.  Current simple implementation returns T for bound.
    """
    if len(args) != 1:
        raise lisptype.LispProgramError(
            f"BOUNDP: wrong number of arguments (got {len(args)}, expected 1)"
        )
    if len(args) != 1:
        raise lisptype.LispProgramError(
            f"BOUNDP: wrong number of arguments (got {len(args)}, expected 1)"
        )
    symbol = args[0]
    # Type check
    # Accept the special NIL (lispNull) as the symbol NIL per Common Lisp
    if symbol is lisptype.NIL:
        # Convert to the canonical NIL symbol in the COMMON-LISP package
        try:
            symbol = lisptype.intern_symbol('NIL', lisptype.COMMON_LISP_PACKAGE)
        except Exception:
            # Raise a Python-level LispTypeError which the evaluator will
            # convert into a Lisp TYPE-ERROR condition.
            raise lisptype.LispTypeError(f"BOUNDP: {symbol} is not a symbol", expected_type='symbol', actual_value=symbol)

    if not isinstance(symbol, lisptype.LispSymbol):
        # For non-symbol arguments raise a Python-level LispTypeError; the
        # evaluator will convert this into a Lisp TYPE-ERROR condition so
        # it can be handled by Lisp-level handlers.
        raise lisptype.LispTypeError(f"BOUNDP: {symbol} is not a symbol", expected_type='symbol', actual_value=symbol)

    # T, NIL, and keywords are self-evaluating and therefore always bound.
    if symbol is lisptype.T or symbol.name in ('T', 'NIL') or isinstance(symbol, lisptype.lispKeyword):
        return lisptype.T

    # A symbol is considered bound if its value cell is present (even if NIL)
    return lisptype.lisp_bool(getattr(symbol, 'value', None) is not None)


def makunbound(*args):
    """Make symbol unbound.

    Accept varargs for ANSI-like error handling; signal PROGRAM-ERROR if
    wrong arity. Current implementation is a placeholder that returns the
    provided symbol.
    """
    if len(args) != 1:
        raise lisptype.LispProgramError(
            f"MAKUNBOUND: wrong number of arguments (got {len(args)}, expected 1)"
        )
    symbol = args[0]
    if not isinstance(symbol, lisptype.LispSymbol):
        raise lisptype.LispTypeError(f"MAKUNBOUND: {symbol} is not a symbol", expected_type='symbol', actual_value=symbol)
    # Remove binding by setting to Python None to indicate 'unbound'
    try:
        symbol.value = None
    except Exception:
        pass
    return symbol


def values(*args):
    """Return multiple values.

    (VALUES a b c) returns three values: a, b, and c.
    When no arguments are given, returns *zero* values (a MultipleValues
    wrapping an empty tuple) -- not NIL, which would be one value.
    When one argument is given, returns that value directly.
    When multiple arguments are given, returns a MultipleValues wrapper.
    """
    if len(args) == 1:
        return args[0]
    else:
        # Return as MultipleValues wrapper for zero or multiple values
        return lisptype.MultipleValues(*args)


def values_list(lst):
    """Return multiple values from a list.

    (VALUES-LIST '(a b c)) returns three values: a, b, and c.
    This is essentially the inverse of MULTIPLE-VALUE-LIST.

    CLHS: (values-list list) is equivalent to (apply #'values list). So
    (values-list nil) returns *zero* values, exactly like (values) -- it used to
    return NIL, which is *one* value, contradicting `values` directly above on
    how zero values are represented. Routing the empty case through `values`
    keeps one answer to that question instead of two.
    """
    items = []
    cur = lst
    while _consp_internal(cur):
        items.append(car(cur))
        cur = cdr(cur)

    if not items:
        return values()

    return lisptype.MultipleValues.from_list(lst)


def multiple_value_return(*args):
    """Return multiple values (alias for VALUES).
    
    Note: MULTIPLE-VALUE-RETURN is not standard ANSI Common Lisp.
    Use VALUES instead. This is provided for compatibility.
    """
    return values(*args)


def make_char_code_conversion(from_encoding, to_encoding):
    """Create a character code conversion function.
    
    Note: MAKE-CHAR-CODE-CONVERSION is not standard ANSI Common Lisp.
    This is provided for compatibility with the target list.
    
    Args:
        from_encoding: Source encoding name
        to_encoding: Target encoding name
        
    Returns:
        A function that converts characters between encodings
    """
    def convert(char_or_string):
        # For now, just return the input unchanged
        # Full implementation would handle encoding conversion
        return char_or_string
    return convert


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


def ccase(keyform, *clauses):
    """Correctable case (dead stub: CCASE is dispatched directly by eval() in
    evaluation_core.py via eval_ccase; this exists only so the symbol shows up
    as fbound for introspection, matching the pattern used by e.g. case_fn)."""
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


def the(type_spec, form):
    """Type declaration."""
    return form


def locally(*body):
    """Local declarations."""
    from .evaluation_core import eval
    result = None
    for form in body:
        result = eval(form)
    return result


def destructuring_bind(lambda_list, expression, *body):
    """Destructuring bind."""
    result = None
    for form in body:
        result = form
    return result


# Assignment and modification operations
def decf(place, delta=1):
    """Decrement place (stub returns numeric result)."""
    return place - delta  # Simplified


def psetf(*pairs):
    """Parallel setf (stub)."""
    return None  # Simplified


def setf(*pairs):
    """Set place (stub)."""
    return None  # Simplified


def shiftf(*places):
    """Shift places (stub)."""
    return None  # Simplified


def rotatef(*places):
    """Rotate places (stub)."""
    return None  # Simplified


def psetq(*pairs):
    """Parallel setq (stub)."""
    return None  # Simplified


def block(name, *body):
    """Execute block with optional return-from."""
    # For now, just evaluate body forms in sequence - proper implementation later
    result = None
    for form in body:
        result = form
    return result


def return_from(name, value=None):
    """Return from named block."""
    # For now, just return the value - proper implementation later
    return value


def catch(tag, *body):
    """Catch thrown values."""
    # For now, just evaluate body - proper implementation later
    result = None
    for form in body:
        result = form
    return result


def throw(tag, value=None):
    """Throw value to catch."""
    # For now, just return the value - proper implementation later
    return value


def tagbody(*forms):
    """Execute forms with tags for GO."""
    # For now, just evaluate non-tag forms - proper implementation later
    result = None
    for form in forms:
        if not isinstance(form, (str, int)):  # Skip tags
            result = form
    return result


def go(tag):
    """Go to tag in tagbody."""
    # For now, just return None - proper implementation later
    return None


def unwind_protect(protected_form, *cleanup_forms):
    """Execute protected form with cleanup."""
    # For now, just execute protected form - proper implementation later
    return protected_form


def and_fn(*args):
    """Logical AND of arguments."""
    result = True
    for arg in args:
        result = arg
        if not arg:
            return None
    return result


def or_fn(*args):
    """Logical OR of arguments."""
    for arg in args:
        if arg:
            return arg
    return None


def prog(*body):
    """Execute prog block."""
    # For now, just evaluate forms - proper implementation later
    result = None
    for form in body:
        result = form
    return result


def when_fn(test, *body):
    """Execute body if test is true."""
    if test:
        result = None
        for form in body:
            result = form
        return result
    return None


def unless_fn(test, *body):
    """Execute body if test is false."""
    if not test:
        result = None
        for form in body:
            result = form
        return result
    return None


def case_fn(keyform, *clauses):
    """Case statement."""
    # For now, return None - proper implementation later
    return None


def cond_fn(*clauses):
    """Conditional statement."""
    # For now, return None - proper implementation later
    return None


def do_fn(*args):
    """Do loop."""
    # For now, return None - proper implementation later
    return None


def dolist(*args):
    """Dolist loop."""
    # For now, return None - proper implementation later
    return None


def dotimes(*args):
    """Dotimes loop."""
    # For now, return None - proper implementation later
    return None


# NOTE: LOOP is now implemented as a special form in evaluation_loops_conditionals.py
# The function below is kept for reference but is no longer registered or used.
def _loop_fn_legacy(*args):
    """Extended LOOP implementation supporting ANSI CL loop macro clauses.

    Supports forms such as:
      (LOOP FOR i FROM 0 TO 9 BY 1 DO (PRINT i))
      (LOOP WHILE <test> DO <forms>)
      (LOOP UNTIL <test> DO <forms>)
      (LOOP REPEAT <n> DO <forms>)
      (LOOP WHILE <test> UNLESS <cond> DO <action> APPEND <form>)
      (LOOP FOR x IN list COLLECT x)

    This is an enhanced implementation of ANSI LOOP to support rt.lsp testing framework.
    """
    from .evaluation_core import eval
    
    # Normalize args into a Python list of forms
    forms = []
    if len(args) == 1 and _consp_internal(args[0]):
        cur = args[0]
        while _consp_internal(cur):
            clause = car(cur)
            if _consp_internal(clause):
                forms.append(car(clause))
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

    def sym_name(x):
        """Get uppercase symbol name or None."""
        return x.name.upper() if isinstance(x, lisptype.LispSymbol) else None

    def eval_body_list(body_forms, env):
        res = None
        for f in body_forms:
            res = eval(f, env)
        return res
    
    # Parse loop clauses into structured form
    # Clauses we track: iteration (FOR/WHILE/UNTIL/REPEAT), conditionals (WHEN/UNLESS),
    # body (DO), accumulation (COLLECT/APPEND/NCONC/SUM/COUNT), return (RETURN/FINALLY)
    
    i = 0
    
    # Loop state
    iteration_type = None  # 'for-range', 'for-in', 'for-on', 'while', 'until', 'repeat', None
    iteration_var = None
    iteration_test = None  # for WHILE/UNTIL
    iteration_start = 0
    iteration_end = None
    iteration_step = 1
    iteration_list = None  # for FOR ... IN/ON
    repeat_count = None
    
    conditionals = []  # list of ('when'/'unless', test_form)
    body_forms = []
    accumulation = None  # ('collect'/'append'/'sum'/'count', form)
    finally_forms = []
    
    # Parse clauses
    while i < len(forms):
        token = forms[i]
        name = sym_name(token)
        
        if name == 'FOR':
            iteration_var = forms[i+1]
            if not isinstance(iteration_var, lisptype.LispSymbol):
                raise lisptype.LispNotImplementedError('LOOP FOR requires a symbol')
            j = i + 2
            # Look for FROM/TO/BY/IN/ON/ACROSS
            while j < len(forms):
                f = forms[j]
                fname = sym_name(f)
                if fname == 'FROM':
                    iteration_start = forms[j+1]
                    j += 2
                elif fname == 'TO':
                    iteration_end = forms[j+1]
                    iteration_type = 'for-range'
                    j += 2
                elif fname == 'BELOW':
                    iteration_end = forms[j+1]
                    iteration_type = 'for-below'
                    j += 2
                elif fname == 'BY':
                    iteration_step = forms[j+1]
                    j += 2
                elif fname == 'IN':
                    iteration_list = forms[j+1]
                    iteration_type = 'for-in'
                    j += 2
                elif fname == 'ON':
                    iteration_list = forms[j+1]
                    iteration_type = 'for-on'
                    j += 2
                elif fname in ('DO', 'DOING', 'COLLECT', 'COLLECTING', 'APPEND', 'APPENDING',
                               'NCONC', 'NCONCING', 'SUM', 'SUMMING', 'COUNT', 'COUNTING',
                               'WHEN', 'UNLESS', 'IF', 'RETURN', 'FINALLY'):
                    break
                else:
                    break
            i = j
            
        elif name == 'WHILE':
            iteration_type = 'while'
            iteration_test = forms[i+1]
            i += 2
            
        elif name == 'UNTIL':
            iteration_type = 'until'
            iteration_test = forms[i+1]
            i += 2
            
        elif name == 'REPEAT':
            iteration_type = 'repeat'
            repeat_count = forms[i+1]
            i += 2
            
        elif name in ('WHEN', 'IF'):
            conditionals.append(('when', forms[i+1]))
            i += 2
            
        elif name == 'UNLESS':
            conditionals.append(('unless', forms[i+1]))
            i += 2
            
        elif name in ('DO', 'DOING'):
            # Collect body forms until next clause keyword
            i += 1
            while i < len(forms):
                f = forms[i]
                fname = sym_name(f)
                if fname in ('FOR', 'WHILE', 'UNTIL', 'REPEAT', 'DO', 'DOING',
                             'COLLECT', 'COLLECTING', 'APPEND', 'APPENDING',
                             'NCONC', 'NCONCING', 'SUM', 'SUMMING', 'COUNT', 'COUNTING',
                             'WHEN', 'UNLESS', 'IF', 'RETURN', 'FINALLY'):
                    break
                body_forms.append(f)
                i += 1
                
        elif name in ('COLLECT', 'COLLECTING'):
            accumulation = ('collect', forms[i+1])
            i += 2
            
        elif name in ('APPEND', 'APPENDING'):
            accumulation = ('append', forms[i+1])
            i += 2
            
        elif name in ('NCONC', 'NCONCING'):
            accumulation = ('nconc', forms[i+1])
            i += 2
            
        elif name in ('SUM', 'SUMMING'):
            accumulation = ('sum', forms[i+1])
            i += 2
            
        elif name in ('COUNT', 'COUNTING'):
            accumulation = ('count', forms[i+1])
            i += 2
            
        elif name == 'RETURN':
            # Immediate return from loop
            return eval(forms[i+1])
            
        elif name == 'FINALLY':
            i += 1
            while i < len(forms):
                finally_forms.append(forms[i])
                i += 1
                
        else:
            # Simple loop - just evaluate body repeatedly until explicit return
            # Or if no iteration, evaluate once
            if iteration_type is None:
                # Simple infinite loop or body evaluation
                body_forms.append(token)
            i += 1
    
    # Execute the loop
    result = None
    accumulated = []
    sum_result = 0
    count_result = 0
    
    def should_execute_body():
        """Check conditionals."""
        for cond_type, cond_form in conditionals:
            cond_result = eval(cond_form)
            if cond_type == 'when' and not lisptype.is_truthy(cond_result):
                return False
            if cond_type == 'unless' and lisptype.is_truthy(cond_result):
                return False
        return True
    
    def execute_iteration_body(loop_env=None):
        """Execute one iteration of the loop body."""
        nonlocal result, accumulated, sum_result, count_result
        
        env = loop_env or lispenv.current_environment
        
        if not should_execute_body():
            return
        
        # Execute body forms
        for f in body_forms:
            result = eval(f, env)
        
        # Handle accumulation
        if accumulation:
            acc_type, acc_form = accumulation
            acc_value = eval(acc_form, env)
            if acc_type == 'collect':
                accumulated.append(acc_value)
            elif acc_type == 'append':
                # Append list to result
                if _consp_internal(acc_value):
                    cur = acc_value
                    while _consp_internal(cur):
                        accumulated.append(car(cur))
                        cur = cdr(cur)
                elif acc_value is not lisptype.NIL and acc_value is not None:
                    accumulated.append(acc_value)
            elif acc_type == 'nconc':
                # Similar to append but destructive (same behavior for us)
                if _consp_internal(acc_value):
                    cur = acc_value
                    while _consp_internal(cur):
                        accumulated.append(car(cur))
                        cur = cdr(cur)
            elif acc_type == 'sum':
                sum_result += acc_value
            elif acc_type == 'count':
                if lisptype.is_truthy(acc_value):
                    count_result += 1
    
    # Main loop execution
    if iteration_type == 'while':
        while lisptype.is_truthy(eval(iteration_test)):
            execute_iteration_body()
            
    elif iteration_type == 'until':
        while True:
            execute_iteration_body()
            if lisptype.is_truthy(eval(iteration_test)):
                break
                
    elif iteration_type == 'repeat':
        count = eval(repeat_count)
        for _ in range(count):
            execute_iteration_body()
            
    elif iteration_type == 'for-range':
        start = eval(iteration_start) if not isinstance(iteration_start, int) else iteration_start
        end = eval(iteration_end)
        step = eval(iteration_step) if not isinstance(iteration_step, int) else iteration_step
        if step == 0:
            raise lisptype.LispNotImplementedError('LOOP BY step cannot be 0')
        
        loop_env = lisptype.Environment(lispenv.current_environment)
        cur = start
        compare = (lambda a, b: a <= b) if step > 0 else (lambda a, b: a >= b)
        while compare(cur, end):
            loop_env.set_variable(iteration_var, cur)
            execute_iteration_body(loop_env)
            cur = cur + step
            
    elif iteration_type == 'for-below':
        start = eval(iteration_start) if not isinstance(iteration_start, int) else iteration_start
        end = eval(iteration_end)
        step = eval(iteration_step) if not isinstance(iteration_step, int) else iteration_step
        if step == 0:
            raise lisptype.LispNotImplementedError('LOOP BY step cannot be 0')
        
        loop_env = lisptype.Environment(lispenv.current_environment)
        cur = start
        compare = (lambda a, b: a < b) if step > 0 else (lambda a, b: a > b)
        while compare(cur, end):
            loop_env.set_variable(iteration_var, cur)
            execute_iteration_body(loop_env)
            cur = cur + step
            
    elif iteration_type == 'for-in':
        lst = eval(iteration_list)
        loop_env = lisptype.Environment(lispenv.current_environment)
        cur = lst
        while _consp_internal(cur):
            loop_env.set_variable(iteration_var, car(cur))
            execute_iteration_body(loop_env)
            cur = cdr(cur)
            
    elif iteration_type == 'for-on':
        lst = eval(iteration_list)
        loop_env = lisptype.Environment(lispenv.current_environment)
        cur = lst
        while _consp_internal(cur):
            loop_env.set_variable(iteration_var, cur)
            execute_iteration_body(loop_env)
            cur = cdr(cur)
            
    elif iteration_type is None:
        # No iteration - simple loop body, execute once
        # Or infinite loop if there are body forms
        if body_forms or accumulation:
            execute_iteration_body()
    
    # Execute FINALLY forms
    for f in finally_forms:
        result = eval(f)
    
    # Return accumulated result or last result
    if accumulation:
        acc_type = accumulation[0]
        if acc_type in ('collect', 'append', 'nconc'):
            # Convert accumulated list to Lisp list
            if not accumulated:
                return lisptype.NIL
            result_list = lisptype.NIL
            for item in reversed(accumulated):
                result_list = cons(item, result_list)
            return result_list
        elif acc_type == 'sum':
            return sum_result
        elif acc_type == 'count':
            return count_result
    
    return result

# Legacy LOOP macro marker - no longer used
# setattr(loop_fn, '__is_macro__', True)


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
    # 'loop_fn',  # LOOP is now a special form
    'load_fn',
]
