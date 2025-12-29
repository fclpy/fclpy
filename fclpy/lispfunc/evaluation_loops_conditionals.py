"""Loops and conditionals: WHEN, COND, AND, OR, PROGN, LET, quasiquote."""

import fclpy.lisptype as lisptype
from .core import car, cdr, _consp_internal, cons
from . import registry as _registry


def eval_when(form, env):
    """Evaluate WHEN special form."""
    from .evaluation_core import eval
    
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
    from .evaluation_core import eval
    
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


def eval_and(form, env):
    """Evaluate AND special form."""
    from .evaluation_core import eval
    
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
    """Evaluate PROGN special form."""
    from .evaluation_core import eval
    
    args = cdr(form)
    result = None
    
    while _consp_internal(args):
        result = eval(car(args), env)
        args = cdr(args)
    
    return result


def eval_locally(form, env):
    """Evaluate LOCALLY special form.
    
    (LOCALLY declaration* form*)
    
    LOCALLY is like PROGN but can include declarations at the start.
    The declarations affect only the body forms within the LOCALLY.
    For now, we skip declarations and just evaluate the body forms.
    """
    from .evaluation_core import eval
    
    args = cdr(form)
    result = lisptype.NIL
    
    # Skip DECLARE forms at the start
    while _consp_internal(args):
        first = car(args)
        # Check if this is a DECLARE form
        if (_consp_internal(first) and 
            isinstance(car(first), lisptype.LispSymbol) and 
            car(first).name == 'DECLARE'):
            # Skip this declaration
            args = cdr(args)
        else:
            # Not a declaration, start evaluating body
            break
    
    # Evaluate remaining body forms
    while _consp_internal(args):
        result = eval(car(args), env)
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
        if _consp_internal(binding):
            var = car(binding)
            init_form = car(cdr(binding))
            # Evaluate init in OUTER environment
            value = eval(init_form, env)
            bindings_list.append((var, value))
        current = cdr(current)
    
    # Now bind all variables in new environment
    # Track special variables that need special handling
    import fclpy.state as state
    old_package = None
    has_package_binding = False
    
    for var, value in bindings_list:
        if isinstance(var, lisptype.LispSymbol):
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


def eval_quasiquote(form, env):
    """Evaluate a QUASIQUOTE form by processing UNQUOTE and UNQUOTE-SPLICING.

    This is a simplified quasiquote evaluator that handles common patterns:
    - (QUASIQUOTE x) where x is a list will return a new list where elements
      of the form (UNQUOTE e) are replaced with the evaluated value of e,
      and elements of the form (UNQUOTE-SPLICING e) are spliced into the list.
    - Nested quasiquotes are not fully supported beyond a single level.
    """
    from .evaluation_core import eval
    
    expr = car(cdr(form))

    def _quasi(obj):
        # If an explicit (UNQUOTE e) form, evaluate and return its value
        if _consp_internal(obj) and isinstance(car(obj), lisptype.LispSymbol) and car(obj).name == 'UNQUOTE':
            return eval(car(cdr(obj)), env)

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
                elif name == 'UNQUOTE':
                    val = eval(car(cdr(item)), env)
                    parts.append(val)
                    cur = cdr(cur)
                    continue

            # Otherwise, recursively quasiquote the item
            if _consp_internal(item):
                parts.append(_quasi(item))
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
    
    result = eval(car(args), env)
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
    result = eval(car(cdr(args)), env)
    args = cdr(cdr(args))
    
    # Evaluate remaining forms for side effects
    while _consp_internal(args):
        eval(car(args), env)
        args = cdr(args)
    
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

    # Parse loop clauses into structured form
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
            return eval(forms[i+1], env)
            
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
    
    def should_execute_body(loop_env):
        """Check conditionals."""
        for cond_type, cond_form in conditionals:
            cond_result = eval(cond_form, loop_env)
            if cond_type == 'when' and not lisptype.is_truthy(cond_result):
                return False
            if cond_type == 'unless' and lisptype.is_truthy(cond_result):
                return False
        return True
    
    def execute_iteration_body(loop_env):
        """Execute one iteration of the loop body."""
        nonlocal result, accumulated, sum_result, count_result
        
        if not should_execute_body(loop_env):
            return
        
        # Execute body forms
        for f in body_forms:
            result = eval(f, loop_env)
        
        # Handle accumulation
        if accumulation:
            acc_type, acc_form = accumulation
            acc_value = eval(acc_form, loop_env)
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
        while lisptype.is_truthy(eval(iteration_test, env)):
            execute_iteration_body(env)
            
    elif iteration_type == 'until':
        while True:
            execute_iteration_body(env)
            if lisptype.is_truthy(eval(iteration_test, env)):
                break
                
    elif iteration_type == 'repeat':
        count = eval(repeat_count, env)
        for _ in range(count):
            execute_iteration_body(env)
            
    elif iteration_type == 'for-range':
        start = eval(iteration_start, env) if not isinstance(iteration_start, int) else iteration_start
        end = eval(iteration_end, env)
        step = eval(iteration_step, env) if not isinstance(iteration_step, int) else iteration_step
        if step == 0:
            raise lisptype.LispNotImplementedError('LOOP BY step cannot be 0')
        
        loop_env = lisptype.Environment(env)
        cur = start
        compare = (lambda a, b: a <= b) if step > 0 else (lambda a, b: a >= b)
        while compare(cur, end):
            loop_env.set_variable(iteration_var, cur)
            execute_iteration_body(loop_env)
            cur = cur + step
            
    elif iteration_type == 'for-below':
        start = eval(iteration_start, env) if not isinstance(iteration_start, int) else iteration_start
        end = eval(iteration_end, env)
        step = eval(iteration_step, env) if not isinstance(iteration_step, int) else iteration_step
        if step == 0:
            raise lisptype.LispNotImplementedError('LOOP BY step cannot be 0')
        
        loop_env = lisptype.Environment(env)
        cur = start
        compare = (lambda a, b: a < b) if step > 0 else (lambda a, b: a > b)
        while compare(cur, end):
            loop_env.set_variable(iteration_var, cur)
            execute_iteration_body(loop_env)
            cur = cur + step
            
    elif iteration_type == 'for-in':
        lst = eval(iteration_list, env)
        loop_env = lisptype.Environment(env)
        cur = lst
        while _consp_internal(cur):
            loop_env.set_variable(iteration_var, car(cur))
            execute_iteration_body(loop_env)
            cur = cdr(cur)
            
    elif iteration_type == 'for-on':
        lst = eval(iteration_list, env)
        loop_env = lisptype.Environment(env)
        cur = lst
        while _consp_internal(cur):
            loop_env.set_variable(iteration_var, cur)
            execute_iteration_body(loop_env)
            cur = cdr(cur)
            
    elif iteration_type is None:
        # No iteration - simple loop body, execute once
        # Or infinite loop if there are body forms
        if body_forms or accumulation:
            execute_iteration_body(env)
    
    # Execute FINALLY forms
    for f in finally_forms:
        result = eval(f, env)
    
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
    
    # Main loop
    while True:
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
        current = body
        while _consp_internal(current):
            eval(car(current), loop_env)
            current = cdr(current)
        
        # Update variables (evaluate all step forms first, then update)
        new_values = []
        for var, _, step_form in var_specs:
            if step_form is not None:
                new_values.append((var, eval(step_form, loop_env)))
        
        for var, value in new_values:
            loop_env.set_variable(var, value)


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
    
    # Main loop
    while True:
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
        current = body
        while _consp_internal(current):
            eval(car(current), loop_env)
            current = cdr(current)
        
        # Update variables sequentially
        for var, step_form in var_specs:
            if step_form is not None:
                new_value = eval(step_form, loop_env)
                loop_env.set_variable(var, new_value)


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
    
    # Iterate over list
    current_list = lst
    while _consp_internal(current_list):
        loop_env.set_variable(var, car(current_list))
        
        # Execute body
        current = body
        while _consp_internal(current):
            eval(car(current), loop_env)
            current = cdr(current)
        
        current_list = cdr(current_list)
    
    # Set var to NIL for result form
    loop_env.set_variable(var, lisptype.NIL)
    
    # Evaluate and return result form
    return eval(result_form, loop_env)


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
    
    # Iterate count times
    for i in range(count):
        loop_env.set_variable(var, i)
        
        # Execute body
        current = body
        while _consp_internal(current):
            eval(car(current), loop_env)
            current = cdr(current)
    
    # Set var to count for result form
    loop_env.set_variable(var, count)
    
    # Evaluate and return result form
    return eval(result_form, loop_env)


__all__ = [
    'eval_when',
    'eval_unless',
    'eval_cond',
    'eval_and',
    'eval_or',
    'eval_progn',
    'eval_locally',
    'eval_let',
    'eval_letstar',
    'eval_quasiquote',
    'eval_prog1',
    'eval_prog2',
    'eval_loop',
    'eval_do',
    'eval_do_star',
    'eval_dolist',
    'eval_dotimes',
]
