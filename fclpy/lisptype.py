

class LispNotImplementedError(Exception):
    """Custom exception for ANSI Common Lisp functions that are not yet implemented."""
    def __init__(self, function_name=None, message="Not implemented"):
        if function_name:
            super().__init__(f"{function_name}: {message}")
        else:
            super().__init__(message)
        self.function_name = function_name


class LispTypeError(Exception):
    """Exception for Common Lisp type errors."""
    def __init__(self, message, expected_type=None, actual_value=None):
        super().__init__(message)
        self.expected_type = expected_type
        self.actual_value = actual_value


class LispError(Exception):
    """Base class for Common Lisp runtime errors."""
    def __init__(self, message):
        super().__init__(message)


class LispEndOfFileError(LispError):
    """Exception for Common Lisp END-OF-FILE condition."""
    def __init__(self, stream=None, message="End of file"):
        super().__init__(message)
        self.stream = stream


class LispEnvironmentError(LispError):
    """Raised when an operation requiring an active Lisp environment is invoked without one.

    This typically indicates that the standard environment has not yet been initialized
    (e.g. lispenv.setup_standard_environment() was not called) and neither an explicit
    environment argument nor state.current_environment is available.
    """
    pass


class Binding:
    def __init__(self,symbol,value,next,env=None):
        self.symbol = symbol
        self.value = value
        self.next = next
        self.env = env
    def __repr__(self):
        return repr(self.symbol)

py_str_map = [
    ["_S_STAR_","*"],
    ["_S_AMP_","&"],
    ["_S_LT_","<"],
    ["_S_GT_",">"],
    ["_S_EQ_",">"],
    ["_S_PLUS_","+"],
    ["_S_MINUS_","-"],
    ["_S_PRINT_","PRINT"],
]


class SpecialForm:
    pass

class FunctionBinding:
    def __init__(self,symbol,value,next):
        self.symbol = symbol
        self.value = value
        self.next = next
    def __repr__(self):
        o = self
        s= []        
        while o != None:
            s.append(repr(o.symbol))
            o = o.next
        return ",".join(s)

class Environment:
    def __init__(self, parent=None):
        self.parent = parent
        if parent == None:
            self.function_bindings = None
            self.variable_bindings = None
            self.tag_bindings = None
        else:
            self.function_bindings = parent.function_bindings
            self.variable_bindings = parent.variable_bindings
            self.tag_bindings = parent.tag_bindings
    def add_function(self, symbol, value):
        self.function_bindings = FunctionBinding(symbol,value, self.function_bindings)
    
    def find_func(self,sym):
        b = self.function_bindings
        while b != None:
            if b.symbol.name == sym.name:
                return b.value
            b = b.next
        return None
    
    def add_variable(self, symbol, value):
        self.variable_bindings = Binding(symbol, value, self.variable_bindings, self)
    
    def find_variable(self, sym):
        b = self.variable_bindings
        while b != None:
            if b.symbol.name == sym.name:
                return b.value
            b = b.next
        # If not found in current environment, check parent
        if self.parent:
            return self.parent.find_variable(sym)
        return None
    
    def set_variable(self, sym, value):
        b = self.variable_bindings
        while b != None:
            if b.symbol.name == sym.name:
                b.value = value
                return value
            b = b.next
        # If not found in current environment, check parent
        if self.parent:
            return self.parent.set_variable(sym, value)
        # If not found anywhere, create new binding
        self.add_variable(sym, value)
        return value

    def read_module(self, mod):
        for k,v in mod.__dict__.items():
            if callable(v) and not k.startswith("__"):
                self.add_function(py_str_to_sym(k),v)
    def __repr__(self):
        return "Environment(function_bindings="+repr(self.function_bindings)+", variable_bindings="+repr(self.variable_bindings)+")"


class lispT:
    pass

class lispSequence(lispT):
    pass

class lispList(lispSequence):
    pass

class lispNull(lispList):
    def __str__(self):
        return "NIL"
    def __repr__(self):
        return "NIL"

NIL = lispNull()

class LispSymbol(lispT):
    def __init__(self, name, package=None):
        self.name = name
        self.package = package
    def __repr__(self):
        return self.name

# Global T symbol for consistent boolean returns
T = LispSymbol('T')

def lisp_bool(value):
    """Convert a Python truthiness value to Lisp T or NIL."""
    if value is None or value is False or value == NIL:
        return NIL
    else:
        return T

def is_truthy(value):
    """Test if a value is truthy in Lisp (anything except NIL and None)."""
    return value is not NIL and value is not None

class lispKeyword(LispSymbol):
    pass


class Package(lispT):
    """Common Lisp package object."""
    
    def __init__(self, name, nicknames=None, use_list=None):
        self.name = name.upper()
        self.nicknames = [nick.upper() for nick in (nicknames or [])]
        self.use_list = use_list or []
        self.symbols = {}  # symbol name -> symbol mapping
        self.external_symbols = set()  # names of exported symbols
        self.shadowing_symbols = set()  # names of shadowing symbols
        
    def __str__(self):
        return f"#<PACKAGE {self.name}>"
        
    def __repr__(self):
        return f"Package(name='{self.name}', nicknames={self.nicknames})"
        
    def intern_symbol(self, name, external=False):
        """Intern a symbol in this package."""
        name = name.upper()
        if name not in self.symbols:
            symbol = LispSymbol(name)
            symbol.package = self
            self.symbols[name] = symbol
        if external:
            self.external_symbols.add(name)
        return self.symbols[name]
        
    def find_symbol(self, name):
        """Find symbol in package, return (symbol, status) tuple."""
        name = name.upper()
        if name in self.symbols:
            if name in self.external_symbols:
                return self.symbols[name], ':EXTERNAL'
            else:
                return self.symbols[name], ':INTERNAL'
        return None, None
        
    def export_symbol(self, name):
        """Export a symbol from this package."""
        name = name.upper()
        if name in self.symbols:
            self.external_symbols.add(name)
            
    def unexport_symbol(self, name):
        """Unexport a symbol from this package.""" 
        name = name.upper()
        self.external_symbols.discard(name)


import fclpy.state as state


def make_package(name, nicknames=None, use_list=None):
    """Create a new package."""
    name = name.upper()
    if name in state.packages:
        return state.packages[name]
    package = Package(name, nicknames, use_list)
    state.packages[name] = package
    # Also register by nicknames
    for nick in package.nicknames:
        state.packages[nick] = package
    return package


def find_package(name):
    """Find package by name or nickname."""
    if isinstance(name, Package):
        return name
    name = name.upper()
    return state.packages.get(name)


# Create standard packages (populate the central state registry)
KEYWORD_PACKAGE = make_package("KEYWORD")
COMMON_LISP_PACKAGE = make_package("COMMON-LISP", ["CL"])
COMMON_LISP_USER_PACKAGE = make_package("COMMON-LISP-USER", ["CL-USER"], [COMMON_LISP_PACKAGE])

class lispConsIterator:    
    def __init__(self, cons):
        self.cons = cons
    def __iter__(self):
        return self
    def __next__(self):
        if self.cons == None or type(self.cons) is lispNull:
            raise StopIteration()
        value = self.cons.car
        self.cons = self.cons.cdr
        return value
    def next(self):
        return self.__next__()

class lispCons(lispList):
    def __init__(self,car,cdr=NIL):
        self.car = car
        if cdr == None or type(cdr) is lispNull:
            self.cdr = NIL
        elif type(cdr) is tuple:
            cdrlen = len(cdr)
            if cdrlen == 0:
                self.cdr = NIL
            elif cdrlen == 1:
                self.cdr = lispCons(cdr[0])
            else:
                self.cdr = lispCons(cdr[0],cdr[1:])
        else:
            self.cdr = cdr
    def __str__(self):
        values = []
        values.append("(")
        values.append("NIL" if self.car == None else str(self.car))
        cdr = self.cdr
        while cdr != None:
            values.append(" ")
            if type(cdr) is lispCons:
                values.append(str(cdr.car))
                cdr = cdr.cdr if type(cdr.cdr) is not lispNull else None

            else:
                values.append(". ")
                values.append(str(cdr))
                cdr = None            
        values.append(")")
        return ''.join(values)
    
    def __repr__(self):
        values = []
        values.append("(")
        values.append("NIL" if self.car == None else repr(self.car))
        cdr = self.cdr if type(self.cdr) is not lispNull else None

        while cdr != None:
            values.append(" ")
            if type(cdr) is lispCons:
                values.append(repr(cdr.car))
                cdr = cdr.cdr if type(cdr.cdr) is not lispNull else None
            else:
                values.append(". ")
                values.append(repr(cdr))
                cdr = None
        values.append(")")
        return ''.join(values)
    
    def __iter__(self):
        return lispConsIterator(self)

def py_str_to_sym(s):
  s = s.upper()
  for p in py_str_map:
      s = s.replace(*p)
  return LispSymbol(s)


# --- Environment resolution helper ---------------------------------------------------------
def resolve_environment(env=None):
    """Return a usable environment or raise LispEnvironmentError.

    If an explicit env is provided it is returned as-is. Otherwise the global
    state.current_environment is used. If that is still None, a LispEnvironmentError
    is raised to surface a clearer error than AttributeError / None dereference.
    """
    if env is not None:
        return env
    from . import state  # local import to avoid cycles at module import time
    if state.current_environment is None:
        raise LispEnvironmentError(
            "No active environment (call lispenv.setup_standard_environment() first or pass env explicitly)."
        )
    return state.current_environment

            

