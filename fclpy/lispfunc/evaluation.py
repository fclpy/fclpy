"""Lisp evaluation system - eval, special forms, and control structures."""

from fclpy.lispenv import current_environment
import fclpy.lisptype as lisptype
import fclpy.lispreader as lispreader
from .core import car, cdr, cons, consp


def eval(form, env=None):
    """Evaluate a Lisp form."""
    if env is None:
        env = current_environment
    
    # Self-evaluating forms
    if form is None or isinstance(form, (int, float, str, bool)):
        return form
    
    # Symbols - look up in environment
    if isinstance(form, lisptype.LispSymbol):
        # Check variable bindings first
        value = env.find_variable(form)
        if value is not None:
            return value
        # If not found as variable, check function bindings
        value = env.find_func(form)
        if value is not None:
            return value
        # If not found in either, raise error
        raise lisptype.LispNotImplementedError(f"Unbound variable: {form.name}")
    
    # Lists - function calls or special forms
    if consp(form):
        operator = car(form)
        args = cdr(form)
        
        # Check for special forms
        if isinstance(operator, lisptype.LispSymbol):
            if operator.name == 'QUOTE':
                return car(args)
            elif operator.name == 'IF':
                return eval_if(form, env)
            elif operator.name == 'SETQ':
                return eval_setq(form, env)
            elif operator.name == 'DEFVAR':
                return eval_defvar(form, env)
            elif operator.name == 'LET':
                return eval_let(form, env)
            elif operator.name == 'DEFUN':
                return eval_defun(form, env)
            elif operator.name == 'LAMBDA':
                return eval_lambda(form, env)
            elif operator.name == 'WHEN':
                return eval_when(form, env)
        
        # Regular function call
        func = eval(operator, env)
        if callable(func):
            eval_args = []
            current = args
            while consp(current):
                eval_args.append(eval(car(current), env))
                current = cdr(current)
            return func(*eval_args)
        else:
            raise lisptype.LispNotImplementedError(f"Not a function: {operator}")
    
    return form


def eval_if(form, env):
    """Evaluate IF special form."""
    args = cdr(form)
    if not consp(args):
        raise lisptype.LispNotImplementedError("IF requires at least 2 arguments")
    
    test_form = car(args)
    then_form = car(cdr(args))
    else_form = car(cdr(cdr(args))) if consp(cdr(cdr(args))) else None
    
    test_result = eval(test_form, env)
    if test_result is not None and test_result != lisptype.NIL:
        return eval(then_form, env)
    elif else_form is not None:
        return eval(else_form, env)
    else:
        return None


def eval_setq(form, env):
    """Evaluate SETQ special form."""
    args = cdr(form)
    result = None
    
    while consp(args) and consp(cdr(args)):
        var = car(args)
        value_form = car(cdr(args))
        
        if not isinstance(var, lisptype.LispSymbol):
            raise lisptype.LispNotImplementedError("SETQ: variable must be a symbol")
        
        result = eval(value_form, env)
        env.set_variable(var, result)
        
        args = cdr(cdr(args))
    
    return result


def eval_defvar(form, env):
    """Evaluate DEFVAR special form."""
    args = cdr(form)
    if not consp(args):
        raise lisptype.LispNotImplementedError("DEFVAR requires at least 1 argument")
    
    var = car(args)
    if not isinstance(var, lisptype.LispSymbol):
        raise lisptype.LispNotImplementedError("DEFVAR: variable must be a symbol")
    
    # Check if variable already exists
    existing = env.find_variable(var)
    if existing is None:
        # Only set if not already defined
        if consp(cdr(args)):
            value = eval(car(cdr(args)), env)
            env.add_variable(var, value)
        else:
            env.add_variable(var, None)
    
    return var


def eval_let(form, env):
    """Evaluate LET special form."""
    args = cdr(form)
    if not consp(args):
        raise lisptype.LispNotImplementedError("LET requires at least 1 argument")
    
    bindings = car(args)
    body = cdr(args)
    
    # Create new environment
    new_env = lisptype.Environment(env)
    
    # Process bindings
    current_binding = bindings
    while consp(current_binding):
        binding = car(current_binding)
        if isinstance(binding, lisptype.LispSymbol):
            # Simple variable binding with NIL
            new_env.add_variable(binding, None)
        elif consp(binding):
            var = car(binding)
            value_form = car(cdr(binding)) if consp(cdr(binding)) else None
            
            if not isinstance(var, lisptype.LispSymbol):
                raise lisptype.LispNotImplementedError("LET: binding variable must be a symbol")
            
            value = eval(value_form, env) if value_form else None
            new_env.add_variable(var, value)
        
        current_binding = cdr(current_binding)
    
    # Evaluate body
    result = None
    current_body = body
    while consp(current_body):
        result = eval(car(current_body), new_env)
        current_body = cdr(current_body)
    
    return result


def eval_defun(form, env):
    """Evaluate DEFUN special form."""
    args = cdr(form)
    if not consp(args) or not consp(cdr(args)):
        raise lisptype.LispNotImplementedError("DEFUN requires at least 2 arguments")
    
    func_name = car(args)
    param_list = car(cdr(args))
    body = cdr(cdr(args))
    
    if not isinstance(func_name, lisptype.LispSymbol):
        raise lisptype.LispNotImplementedError("DEFUN: function name must be a symbol")
    
    # Create function closure
    def user_function(*call_args):
        # Create new environment for function execution
        func_env = lisptype.Environment(env)
        
        # Bind parameters
        params = param_list
        for i, arg in enumerate(call_args):
            if consp(params):
                param = car(params)
                if isinstance(param, lisptype.LispSymbol):
                    func_env.add_variable(param, arg)
                params = cdr(params)
        
        # Execute body
        result = None
        current_body = body
        while consp(current_body):
            result = eval(car(current_body), func_env)
            current_body = cdr(current_body)
        
        return result
    
    # Add function to environment
    env.add_function(func_name, user_function)
    return func_name


def eval_lambda(form, env):
    """Evaluate LAMBDA special form."""
    args = cdr(form)
    if not consp(args):
        raise lisptype.LispNotImplementedError("LAMBDA requires at least 1 argument")
    
    param_list = car(args)
    body = cdr(args)
    
    # Create function closure
    def lambda_function(*call_args):
        # Create new environment for function execution
        func_env = lisptype.Environment(env)
        
        # Bind parameters
        params = param_list
        for i, arg in enumerate(call_args):
            if consp(params):
                param = car(params)
                if isinstance(param, lisptype.LispSymbol):
                    func_env.add_variable(param, arg)
                params = cdr(params)
        
        # Execute body
        result = None
        current_body = body
        while consp(current_body):
            result = eval(car(current_body), func_env)
            current_body = cdr(current_body)
        
        return result
    
    return lambda_function


def eval_when(form, env):
    """Evaluate WHEN special form."""
    args = cdr(form)
    if not consp(args):
        return None
    
    test_form = car(args)
    body = cdr(args)
    
    test_result = eval(test_form, env)
    if test_result is not None and test_result != lisptype.NIL:
        result = None
        current_body = body
        while consp(current_body):
            result = eval(car(current_body), env)
            current_body = cdr(current_body)
        return result
    else:
        return None


def eval_unless(form, env):
    """Evaluate UNLESS special form."""
    args = cdr(form)
    if not consp(args):
        return None
    
    test_form = car(args)
    body = cdr(args)
    
    test_result = eval(test_form, env)
    if test_result is None or test_result == lisptype.NIL:
        result = None
        current_body = body
        while consp(current_body):
            result = eval(car(current_body), env)
            current_body = cdr(current_body)
        return result
    else:
        return None


def eval_cond(form, env):
    """Evaluate COND special form."""
    clauses = cdr(form)
    
    while consp(clauses):
        clause = car(clauses)
        if consp(clause):
            test = car(clause)
            
            # Special case for T
            if (isinstance(test, lisptype.LispSymbol) and test.name == 'T') or eval(test, env):
                # Execute forms in clause
                result = test if not consp(cdr(clause)) else None
                forms = cdr(clause)
                while consp(forms):
                    result = eval(car(forms), env)
                    forms = cdr(forms)
                return result
        
        clauses = cdr(clauses)
    
    return None


def eval_and(form, env):
    """Evaluate AND special form."""
    args = cdr(form)
    result = True  # AND with no arguments is T
    
    while consp(args):
        result = eval(car(args), env)
        if result is None or result == lisptype.NIL:
            return None
        args = cdr(args)
    
    return result


def eval_or(form, env):
    """Evaluate OR special form."""
    args = cdr(form)
    
    while consp(args):
        result = eval(car(args), env)
        if result is not None and result != lisptype.NIL:
            return result
        args = cdr(args)
    
    return None


def eval_progn(form, env):
    """Evaluate PROGN special form."""
    args = cdr(form)
    result = None
    
    while consp(args):
        result = eval(car(args), env)
        args = cdr(args)
    
    return result


def eval_prog1(form, env):
    """Evaluate PROG1 special form."""
    args = cdr(form)
    if not consp(args):
        return None
    
    result = eval(car(args), env)
    args = cdr(args)
    
    # Evaluate remaining forms for side effects
    while consp(args):
        eval(car(args), env)
        args = cdr(args)
    
    return result


def eval_prog2(form, env):
    """Evaluate PROG2 special form."""
    args = cdr(form)
    if not consp(args) or not consp(cdr(args)):
        return None
    
    # Evaluate first form for side effects
    eval(car(args), env)
    
    # Return value of second form
    result = eval(car(cdr(args)), env)
    args = cdr(cdr(args))
    
    # Evaluate remaining forms for side effects
    while consp(args):
        eval(car(args), env)
        args = cdr(args)
    
    return result


def apply(function, *args):
    """Apply function to arguments."""
    if args and hasattr(args[-1], '__iter__'):
        # Last argument is a list of arguments
        all_args = list(args[:-1]) + list(args[-1])
        return function(*all_args)
    else:
        return function(*args)


def funcall(function, *args):
    """Call function with arguments."""
    return function(*args)


def lambda_fn(lambda_list, *body):
    """Create lambda function."""
    def lambda_func(*args):
        # Simple lambda implementation
        # In full implementation would handle lambda list parsing
        if body:
            return eval(body[-1])
        return None
    return lambda_func


# Control flow and special forms
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
    try:
        result = None
        for form in body:
            result = eval(form)
        return result
    except:
        return None


def unless(test, *forms):
    """Execute forms if test is false."""
    if not test:
        result = None
        for form in forms:
            result = eval(form)
        return result
    return None


def prog1(first_form, *forms):
    """Evaluate first form, then other forms, return first form's value."""
    result = eval(first_form)
    for form in forms:
        eval(form)
    return result


def prog2(first_form, second_form, *forms):
    """Evaluate first form, then second, then others, return second form's value."""
    eval(first_form)
    result = eval(second_form)
    for form in forms:
        eval(form)
    return result


def progn(*forms):
    """Evaluate forms sequentially, return last form's value."""
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


def set(symbol, value):
    """Set the value of a symbol (dynamic variable)."""
    # For now, just return the value - proper symbol table management later
    return value


def boundp(symbol):
    """Test if symbol has a value binding."""
    # For now, assume most symbols are bound - proper implementation later
    return True


def makunbound(symbol):
    """Make symbol unbound."""
    # For now, just return the symbol - proper implementation later
    return symbol


def values(*args):
    """Return multiple values."""
    # For now, return first value or None - proper multiple-values later
    return args[0] if args else None


def values_list(lst):
    """Return multiple values from a list."""
    # For now, return first element or None - proper implementation later
    from .core import consp, car
    return car(lst) if consp(lst) else None


# Aliases for functions that may have different names in lispenv.py
def apply_fn(function, *args):
    """Apply function (alias for apply)."""
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
    """Correctable case."""
    return None  # Simplified


def ecase(keyform, *clauses):
    """Exhaustive case."""
    return None  # Simplified


def eval_fn(form, env=None):
    """Eval function (alias)."""
    return eval(form, env)


def compile_fn(name, definition=None):
    """Compile function."""
    return None  # Simplified


def the(type_spec, form):
    """Type declaration."""
    return form


def locally(*body):
    """Local declarations."""
    result = None
    for form in body:
        result = form
    return result


def destructuring_bind(lambda_list, expression, *body):
    """Destructuring bind."""
    result = None
    for form in body:
        result = form
    return result


# Assignment and modification operations
def decf(place, delta=1):
    """Decrement place."""
    return place - delta  # Simplified


def psetf(*pairs):
    """Parallel setf."""
    return None  # Simplified


def setf(*pairs):
    """Set place."""
    return None  # Simplified


def shiftf(*places):
    """Shift places."""
    return None  # Simplified


def rotatef(*places):
    """Rotate places."""
    return None  # Simplified


def psetq(*pairs):
    """Parallel setq."""
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


def loop_fn(*args):
    """Loop macro."""
    # For now, return None - proper implementation later
    return None


def load_fn(filename, **kwargs):
    """Load file."""
    # For now, return None - proper implementation later
    return None


def function_fn(name):
    """Function special operator."""
    return name  # In real Lisp, this would return the function object


def quote_fn(expression):
    """Quote special operator."""
    return expression
