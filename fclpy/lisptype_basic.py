"""
lisptype_basic - Core Lisp type system definitions.

Provides fundamental Lisp types, symbols, characters, and basic utilities.
"""


class LispError(Exception):
    """Base class for Common Lisp runtime errors."""
    def __init__(self, message):
        super().__init__(message)


class LispNotImplementedError(LispError):
    """Custom exception for ANSI Common Lisp functions that are not yet implemented."""
    def __init__(self, function_name=None, message="Not implemented"):
        if function_name:
            super().__init__(f"{function_name}: {message}")
        else:
            super().__init__(message)
        self.function_name = function_name


class LispTypeError(LispError):
    """Exception for Common Lisp type errors."""
    def __init__(self, message, expected_type=None, actual_value=None):
        super().__init__(message)
        self.expected_type = expected_type
        self.actual_value = actual_value


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


class MultipleValues(lispT):
    """Represents multiple return values in Common Lisp.
    
    In Common Lisp, functions can return multiple values using (VALUES a b c ...).
    This class wraps those values and can be unpacked as needed.
    
    When used in single-value context, the first value is used (default behavior).
    When used in multiple-value context, all values are available.
    """
    
    def __init__(self, *values):
        """Initialize with a sequence of values.
        
        Args:
            *values: Variable number of Lisp values
        """
        # Store values as a tuple for immutability
        if len(values) == 1 and isinstance(values[0], (list, tuple)):
            # Allow passing a list/tuple as a single argument
            self.values = tuple(values[0])
        else:
            self.values = values
    
    def __repr__(self):
        """Return representation as (VALUES ...)."""
        if not self.values:
            return "(VALUES)"
        val_strs = [repr(v) for v in self.values]
        return f"(VALUES {' '.join(val_strs)})"
    
    def __str__(self):
        """Return string representation."""
        if not self.values:
            return "NIL"  # (VALUES) returns NIL
        return str(self.values[0])  # In single-value context, use first
    
    def get_primary(self):
        """Get the primary (first) value.
        
        When multiple values are used in a single-value context,
        this is what is returned.
        """
        if self.values:
            return self.values[0]
        else:
            return NIL
    
    def get_all(self):
        """Get all values as a tuple."""
        return self.values
    
    def __len__(self):
        """Return number of values."""
        return len(self.values)
    
    def __getitem__(self, index):
        """Get value by index."""
        return self.values[index]
    
    def to_list(self):
        """Convert multiple values to a Lisp list.
        
        Useful for MULTIPLE-VALUE-LIST.
        """
        if not self.values:
            return NIL
        result = NIL
        for val in reversed(self.values):
            result = lispCons(val, result)
        return result
    
    @staticmethod
    def from_list(lst):
        """Create MultipleValues from a Lisp list.
        
        Useful for VALUES-LIST.
        """
        values = []
        current = lst
        while isinstance(current, lispCons):
            values.append(current.car)
            current = current.cdr
        
        if not values:
            return NIL
        elif len(values) == 1:
            return values[0]
        else:
            return MultipleValues(*values)


__all__ = [
    # Exceptions
    'LispNotImplementedError', 'LispTypeError', 'LispError',
    'LispEndOfFileError', 'LispEnvironmentError',
    # Core Types
    'lispT', 'lispSequence', 'lispList', 'lispNull', 'LispSymbol',
    'lispKeyword', 'Character', 'lispCons', 'lispConsIterator',
    # Constants
    'NIL', 'T',
    # Symbol Operations
    'symbol_value', 'set_symbol_value', 'symbol_function',
    'set_symbol_function', 'symbol_plist', 'set_symbol_plist',
    # Utilities
    'lisp_bool', 'is_truthy', 'lisp_str', 'lisp_repr',
    'MultipleValues', 'py_str_map',
    # Binding helpers (internal but useful)
    'Binding', 'FunctionBinding', 'SpecialForm'
]
