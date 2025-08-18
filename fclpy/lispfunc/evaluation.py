"""Lisp evaluation system - eval, special forms, and control structures."""

import fclpy.state as state
import fclpy.lisptype as lisptype
import fclpy.lispreader as lispreader
from .core import car, cdr, cons, _consp_internal, _atom_internal

# Register special operator handlers into the builtin registry
from . import registry as _registry

def eval(form, env=None):
    """Evaluate a Lisp form."""
    if env is None:
        # Always use the live environment stored in shared state so tests
        # that reset state.current_environment get the correct object.
        env = state.current_environment
    
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
    if _consp_internal(form):
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
            elif operator.name == 'DEFMACRO':
                return eval_defmacro(form, env)
        
        # Macro handling: if operator names a macro function, expand first
        if isinstance(operator, lisptype.LispSymbol):
            func_binding = env.find_func(operator)
            if callable(func_binding) and getattr(func_binding, '__is_macro__', False):
                # Gather raw args (without evaluating)
                raw_args = []
                current = args
                while _consp_internal(current):
                    raw_args.append(car(current))
                    current = cdr(current)
                expanded = func_binding(*raw_args)
                # If macro returns a tuple/list of forms, wrap as progn
                return eval(expanded, env)

        # Regular function call
        func = eval(operator, env)
        if callable(func):
            eval_args = []
            current = args
            while _consp_internal(current):
                eval_args.append(eval(car(current), env))
                current = cdr(current)
            return func(*eval_args)
        else:
            raise lisptype.LispNotImplementedError(f"Not a function: {operator}")
    
    return form


@_registry.cl_special('IF')
def eval_if(form, env):
    """Evaluate IF special form."""
    args = cdr(form)
    if not _consp_internal(args):
        raise lisptype.LispNotImplementedError("IF requires at least 2 arguments")
    
    test_form = car(args)
    then_form = car(cdr(args))
    else_form = car(cdr(cdr(args))) if _consp_internal(cdr(cdr(args))) else None
    
    test_result = eval(test_form, env)
    if test_result is not None and test_result != lisptype.NIL:
        return eval(then_form, env)
    elif else_form is not None:
        return eval(else_form, env)
    else:
        return None


@_registry.cl_special('SETQ')
def eval_setq(form, env):
    """Evaluate SETQ special form."""
    args = cdr(form)
    result = None

    while _consp_internal(args) and _consp_internal(cdr(args)):
        var = car(args)
        value_form = car(cdr(args))

        if not isinstance(var, lisptype.LispSymbol):
            raise lisptype.LispNotImplementedError("SETQ: variable must be a symbol")

        result = eval(value_form, env)
        env.set_variable(var, result)

        args = cdr(cdr(args))

    return result


@_registry.cl_special('DEFUN')
def eval_defun(form, env):
    """Evaluate DEFUN special form."""
    args = cdr(form)
    if not _consp_internal(args) or not _consp_internal(cdr(args)):
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
            if _consp_internal(params):
                param = car(params)
                if isinstance(param, lisptype.LispSymbol):
                    func_env.add_variable(param, arg)
                params = cdr(params)
        
        # Execute body
        result = None
        current_body = body
        while _consp_internal(current_body):
            result = eval(car(current_body), func_env)
            current_body = cdr(current_body)
        
        return result
    
    # Add function to environment
    env.add_function(func_name, user_function)
    return func_name


def eval_defmacro(form, env):
    """Evaluate DEFMACRO special form: register a macro in the environment.

    This creates a Python callable that performs simple textual substitution of
    the lambda-list parameters into the macro body forms and marks it as a macro
    so that the evaluator will call it with raw (unevaluated) arguments.
    """
    args = cdr(form)
    if not _consp_internal(args) or not _consp_internal(cdr(args)):
        raise lisptype.LispNotImplementedError("DEFMACRO requires a name, lambda-list and body")

    macro_name = car(args)
    lambda_list = car(cdr(args))
    body = cdr(cdr(args))

    if not isinstance(macro_name, lisptype.LispSymbol):
        raise lisptype.LispNotImplementedError("DEFMACRO: macro name must be a symbol")

    # Build parameter name list
    params = []
    cur = lambda_list
    while _consp_internal(cur):
        p = car(cur)
        if isinstance(p, lisptype.LispSymbol):
            params.append(p.name)
        cur = cdr(cur)

    # Helper to substitute params in a form
    def substitute(form, mapping):
        # Symbols: replace if in mapping
        if isinstance(form, lisptype.LispSymbol):
            if form.name in mapping:
                return mapping[form.name]
            return form
        # Cons cell: recurse
        if _consp_internal(form):
            new_car = substitute(car(form), mapping)
            new_cdr = substitute(cdr(form), mapping) if _consp_internal(cdr(form)) or isinstance(cdr(form), lisptype.LispSymbol) else cdr(form)
            return lisptype.lispCons(new_car, new_cdr)
        # Other atoms: return as-is
        return form

    # Create the macro callable
    def macro_callable(*call_args):
        # Map parameter names to raw argument forms
        mapping = {}
        for i, name in enumerate(params):
            if i < len(call_args):
                mapping[name] = call_args[i]
            else:
                mapping[name] = None

        # If multiple body forms, wrap in PROGN
        if not _consp_internal(body):
            return None

        # Build substituted body forms
        substituted_forms = []
        cur_body = body
        while _consp_internal(cur_body):
            substituted_forms.append(substitute(car(cur_body), mapping))
            cur_body = cdr(cur_body)

        if len(substituted_forms) == 1:
            return substituted_forms[0]
        else:
            # Build (PROGN <forms...>)
            progn_sym = lisptype.LispSymbol('PROGN')
            forms_list = lisptype.NIL
            for f in reversed(substituted_forms):
                forms_list = lisptype.lispCons(f, forms_list)
            return lisptype.lispCons(progn_sym, forms_list)

    # Mark as macro and register in environment
    setattr(macro_callable, '__is_macro__', True)
    env.add_function(macro_name, macro_callable)
    return macro_name


def eval_lambda(form, env):
    """Evaluate LAMBDA special form."""
    args = cdr(form)
    if not _consp_internal(args):
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
            if _consp_internal(params):
                param = car(params)
                if isinstance(param, lisptype.LispSymbol):
                    func_env.add_variable(param, arg)
                params = cdr(params)
        
        # Execute body
        result = None
        current_body = body
        while _consp_internal(current_body):
            result = eval(car(current_body), func_env)
            current_body = cdr(current_body)
        
        return result
    
    return lambda_function


def eval_when(form, env):
    """Evaluate WHEN special form."""
    args = cdr(form)
    if not _consp_internal(args):
        return None
    
    test_form = car(args)
    body = cdr(args)
    
    test_result = eval(test_form, env)
    if test_result is not None and test_result != lisptype.NIL:
        result = None
        current_body = body
        while _consp_internal(current_body):
            result = eval(car(current_body), env)
            current_body = cdr(current_body)
        return result
    else:
        return None


def eval_unless(form, env):
    """Evaluate UNLESS special form."""
    args = cdr(form)
    if not _consp_internal(args):
        return None
    
    test_form = car(args)
    body = cdr(args)
    
    test_result = eval(test_form, env)
    if test_result is None or test_result == lisptype.NIL:
        result = None
        current_body = body
        while _consp_internal(current_body):
            result = eval(car(current_body), env)
            current_body = cdr(current_body)
        return result
    else:
        return None


def eval_cond(form, env):
    """Evaluate COND special form."""
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


def eval_and(form, env):
    """Evaluate AND special form."""
    args = cdr(form)
    result = True  # AND with no arguments is T
    
    while _consp_internal(args):
        result = eval(car(args), env)
        if result is None or result == lisptype.NIL:
            return None
        args = cdr(args)
    
    return result


def eval_or(form, env):
    """Evaluate OR special form."""
    args = cdr(form)
    
    while _consp_internal(args):
        result = eval(car(args), env)
        if result is not None and result != lisptype.NIL:
            return result
        args = cdr(args)
    
    return None


def eval_progn(form, env):
    """Evaluate PROGN special form."""
    args = cdr(form)
    result = None
    
    while _consp_internal(args):
        result = eval(car(args), env)
        args = cdr(args)
    
    return result


def eval_prog1(form, env):
    """Evaluate PROG1 special form."""
    args = cdr(form)
    if not _consp_internal(args):
        return None
    
    result = eval(car(args), env)
    args = cdr(args)
    
    # Evaluate remaining forms for side effects
    while _consp_internal(args):
        eval(car(args), env)
        args = cdr(args)
    
    return result


def eval_prog2(form, env):
    """Evaluate PROG2 special form."""
    args = cdr(form)
    if not _consp_internal(args) or not _consp_internal(cdr(args)):
        return None
    
    # Evaluate first form for side effects
    eval(car(args), env)
    
    # Return value of second form
    result = eval(car(cdr(args)), env)
    args = cdr(cdr(args))
    
    # Evaluate remaining forms for side effects
    while _consp_internal(args):
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
    from .core import _consp_internal, car
    return car(lst) if _consp_internal(lst) else None


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
                    loop_env = lisptype.Environment(current_environment)
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
                res = eval_body_list(body, current_environment)
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
                res = eval_body_list(body, current_environment)
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
                res = eval_body_list(body, current_environment)
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


def function_fn(name):
    """Function special operator."""
    return name  # In real Lisp, this would return the function object


def quote_fn(expression):
    """Quote special operator."""
    return expression
