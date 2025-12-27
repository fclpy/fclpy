

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
        self.value = None        # Symbol's value (for SETQ)
        self.function = None     # Symbol's function definition (for DEFUN)
        self.plist = {}          # Property list (for PUTPROP/GETPROP)
    def __repr__(self):
        return self.name

# Global T symbol for consistent boolean returns
T = LispSymbol('T')

def symbol_value(symbol):
    """Get the value of a symbol."""
    if not isinstance(symbol, LispSymbol):
        raise TypeError(f"symbol-value: {symbol} is not a symbol")
    return symbol.value

def set_symbol_value(symbol, value):
    """Set the value of a symbol."""
    if not isinstance(symbol, LispSymbol):
        raise TypeError(f"set symbol-value: {symbol} is not a symbol")
    symbol.value = value
    return value

def symbol_function(symbol):
    """Get the function definition of a symbol."""
    if not isinstance(symbol, LispSymbol):
        raise TypeError(f"symbol-function: {symbol} is not a symbol")
    return symbol.function

def set_symbol_function(symbol, func):
    """Set the function definition of a symbol."""
    if not isinstance(symbol, LispSymbol):
        raise TypeError(f"set symbol-function: {symbol} is not a symbol")
    symbol.function = func
    return func

def symbol_plist(symbol):
    """Get the property list of a symbol."""
    if not isinstance(symbol, LispSymbol):
        raise TypeError(f"symbol-plist: {symbol} is not a symbol")
    return symbol.plist

def set_symbol_plist(symbol, plist):
    """Set the property list of a symbol."""
    if not isinstance(symbol, LispSymbol):
        raise TypeError(f"set symbol-plist: {symbol} is not a symbol")
    symbol.plist = plist
    return plist

def lisp_bool(value):
    """Convert a Python truthiness value to Lisp T or NIL."""
    if value is None or value is False or value == NIL:
        return NIL
    else:
        return T

def is_truthy(value):
    """Test if a value is truthy in Lisp (anything except NIL and None)."""
    return value is not NIL and value is not None


def lisp_str(value):
    """Return a Lisp-style string representation for printing.

    This ensures Lisp booleans and symbols print as `T`/`NIL` and that
    cons lists and keywords use their own string representations.
    """
    # Native Python booleans -> Lisp booleans
    if value is True:
        return repr(T)
    if value is False:
        return repr(NIL)

    # If the value is a native Python list/tuple (leftover from some helpers)
    # present it as a Lisp list for readability, e.g. [T, NIL] -> (T NIL)
    if isinstance(value, (list, tuple)):
        inner = ' '.join(lisp_str(v) for v in value)
        return f"({inner})"

    # Lisp-specific types already implement __str__/__repr__ as needed
    try:
        # For NIL (lispNull) __str__ already returns 'NIL'
        return str(value)
    except Exception:
        return repr(value)


def lisp_repr(value):
    """Return a Lisp-style readable representation (like prin1).

    Prefer __repr__ for Lisp objects so structures are readable.
    """
    # For native Python lists/tuples, render as Lisp-style lists
    if isinstance(value, (list, tuple)):
        inner = ' '.join(lisp_repr(v) for v in value)
        return f"({inner})"
    try:
        return repr(value)
    except Exception:
        return str(value)

class lispKeyword(LispSymbol):
    def __repr__(self):
        # Represent keywords with a leading colon, e.g. :FOO
        return f":{self.name}"

    def __str__(self):
        # Ensure printing (str) also shows the leading colon
        return f":{self.name}"


class Character(lispT):
    """Common Lisp CHARACTER type.
    
    Represents a single character with a code attribute.
    """
    
    # Standard named character mappings
    NAMED_CHARACTERS = {
        'SPACE': ' ',
        'NEWLINE': '\n',
        'TAB': '\t',
        'RETURN': '\r',
        'BACKSPACE': '\b',
        'FORM-FEED': '\f',
        'RUBOUT': '\x7f',  # DELETE character
    }
    
    # Reverse mapping for printing
    CODE_TO_NAME = {v: k for k, v in NAMED_CHARACTERS.items()}
    CODE_TO_NAME[' '] = 'Space'  # Preferred form
    CODE_TO_NAME['\n'] = 'Newline'
    CODE_TO_NAME['\t'] = 'Tab'
    CODE_TO_NAME['\r'] = 'Return'
    
    def __init__(self, char: str):
        """Initialize a Character.
        
        Args:
            char: A single character string
        """
        if not isinstance(char, str) or len(char) != 1:
            raise TypeError(f"Character must be a single character, got {char!r}")
        self.char = char
        self.code = ord(char)
    
    def __repr__(self):
        """Return character representation for reading: #\\A or #\\Space."""
        # Check if it's a named character
        if self.char in self.CODE_TO_NAME:
            name = self.CODE_TO_NAME[self.char]
            return f"#\\{name}"
        elif self.char.isalnum() or self.char in "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~":
            # ASCII printable character
            return f"#\\{self.char}"
        else:
            # Unprintable character, use code form
            return f"#\\U{self.code:04X}"
    
    def __str__(self):
        """Return string representation."""
        if self.char in self.CODE_TO_NAME:
            name = self.CODE_TO_NAME[self.char]
            return f"#\\{name}"
        elif self.char.isprintable():
            return f"#\\{self.char}"
        else:
            return f"#\\U{self.code:04X}"
    
    def __eq__(self, other):
        """Compare characters.
        
        Characters are only equal to other Character objects with the same character value.
        """
        if isinstance(other, Character):
            return self.char == other.char
        return False
    
    def __hash__(self):
        """Allow characters to be used in sets/dicts."""
        return hash(self.char)
    
    @classmethod
    def from_code(cls, code: int) -> 'Character':
        """Create a Character from a Unicode code point."""
        return cls(chr(code))
    
    @classmethod
    def from_name(cls, name: str) -> 'Character':
        """Create a Character from a named character.
        
        Args:
            name: Named character like "Space", "Newline", etc.
            
        Returns:
            Character instance
        """
        name_upper = name.upper()
        if name_upper in cls.NAMED_CHARACTERS:
            return cls(cls.NAMED_CHARACTERS[name_upper])
        # Try exact case match
        for named_char, char_value in cls.NAMED_CHARACTERS.items():
            if named_char.lower() == name_upper.lower():
                return cls(char_value)
        raise ValueError(f"Unknown named character: {name}")


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

def intern_symbol(name, package=None):
    """Intern a symbol in the given package (or CL-USER if not specified).
    
    This should be used instead of directly calling LispSymbol() constructor
    to ensure proper package association.
    """
    if isinstance(name, LispSymbol):
        return name
    if package is None:
        package = COMMON_LISP_USER_PACKAGE
    if isinstance(package, str):
        package = find_package(package) or make_package(package)
    if not isinstance(package, Package):
        raise TypeError(f"intern_symbol: {package} is not a package")
    return package.intern_symbol(str(name))

def intern_keyword(name):
    """Intern a keyword symbol in the KEYWORD package.
    
    Keywords are self-evaluating and created from names with leading colons stripped.
    """
    if isinstance(name, str):
        # Remove leading colon if present
        if name.startswith(':'):
            name = name[1:]
        name = name.upper()
        # Check if already interned
        if name in KEYWORD_PACKAGE.symbols:
            symbol = KEYWORD_PACKAGE.symbols[name]
            if isinstance(symbol, lispKeyword):
                return symbol
        # Create new keyword keyword
        keyword = lispKeyword(name, KEYWORD_PACKAGE)
        KEYWORD_PACKAGE.symbols[name] = keyword
        KEYWORD_PACKAGE.external_symbols.add(name)
        return keyword
    elif isinstance(name, lispKeyword):
        return name
    else:
        raise TypeError(f"intern_keyword: {name} is not a string or keyword")

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
  return intern_symbol(s, COMMON_LISP_USER_PACKAGE)


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

            

