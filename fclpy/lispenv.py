
import fclpy.lisptype


current_environment = fclpy.lisptype.Environment()
_functions_loaded = False

def setup_standard_environment():
    """Set up the standard environment with proper Lisp function name mappings."""
    global _functions_loaded
    if _functions_loaded:
        return current_environment
        
    # Import here to avoid circular imports
    import fclpy.lispfunc as lispfunc
    
    # Function name mappings from Lisp names to Python function names
    function_mappings = {
        # Core list operations
        'CAR': lispfunc.car,
        'CDR': lispfunc.cdr,
        'CONS': lispfunc.cons,
        'LIST': lispfunc.list,
        'APPEND': lispfunc.append,
        'REVERSE': lispfunc.reverse,
        'LENGTH': lispfunc.length,
        'LAST': lispfunc.last,
        'FIRST': lispfunc.first,
        'SECOND': lispfunc.second,
        'THIRD': lispfunc.third,
        'FOURTH': lispfunc.fourth,
        'REST': lispfunc.rest,
        
        # List accessors
        'CADR': lispfunc.cadr,
        'CADDR': lispfunc.caddr,
        'CADDDR': lispfunc.cadddr,
        'CDDR': lispfunc.cddr,
        'CDDDR': lispfunc.cdddr,
        
        # Predicates
        'ATOM': lispfunc.atom,
        'CONSP': lispfunc.consp,
        'LISTP': lispfunc.listp,
        'NULL': lispfunc.null,
        'SYMBOLP': lispfunc.symbolp,
        'STRINGP': lispfunc.stringp,
        'NUMBERP': lispfunc.numberp,
        
        # Equality and comparison
        'EQ': lispfunc.eq,
        'EQL': lispfunc.eql,
        'EQUAL': lispfunc.equal,
        'NOT': lispfunc.not_fn,
        
        # Arithmetic operations
        '+': lispfunc._s_plus_,
        '-': lispfunc._s_minus_,
        '*': lispfunc._s_star_,
        '/': lispfunc._s_slash_,
        '=': lispfunc._s_eq_,
        '<': lispfunc._s_lt_,
        '>': lispfunc._s_gt_,
        '<=': lispfunc._s_le_,
        '>=': lispfunc._s_ge_,
        
        # Mathematical functions
        'FLOOR': lispfunc.floor,
        'CEILING': lispfunc.ceiling,
        'ROUND': lispfunc.round,
        'TRUNCATE': lispfunc.truncate,
        'EXP': lispfunc.exp,
        'EXPT': lispfunc.expt,
        'GCD': lispfunc.gcd,
        'LCM': lispfunc.lcm,
        'MAX': lispfunc.max_fn,
        'MIN': lispfunc.min_fn,
        'PLUSP': lispfunc.plusp,
        'MINUSP': lispfunc.minusp,
        'EVENP': lispfunc.evenp,
        'ODDP': lispfunc.oddp,
        
        # I/O and utilities
        'PRINT': lispfunc._s_print_,
        'READ': lispfunc.read,
        'ERROR': lispfunc.error,
        'GENSYM': lispfunc.gensym,
        
        # Special forms are handled by eval, but we can include them for completeness
        'EVAL': lispfunc.eval,
        'APPLY': lispfunc.apply,
        
        # Special forms (these are handled specially in eval)
        'IF': lambda *args: "SPECIAL_FORM_IF",
        'COND': lambda *args: "SPECIAL_FORM_COND", 
        'LAMBDA': lambda *args: "SPECIAL_FORM_LAMBDA",
        'DEFUN': lambda *args: "SPECIAL_FORM_DEFUN",
        'SETQ': lambda *args: "SPECIAL_FORM_SETQ",
    }
    
    # Add all the mapped functions to the environment
    for lisp_name, python_func in function_mappings.items():
        symbol = fclpy.lisptype.LispSymbol(lisp_name)
        current_environment.add_function(symbol, python_func)
    
    _functions_loaded = True
    return current_environment