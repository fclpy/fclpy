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


class LispStreamError(LispError):
    """Exception for Common Lisp STREAM-ERROR conditions (CLHS 21.1) --
    e.g. reading from a stream not open for input, or one that is closed."""
    def __init__(self, stream=None, message="Stream error"):
        super().__init__(message)
        self.stream = stream


class LispEnvironmentError(LispError):
    """Raised when an operation requiring an active Lisp environment is invoked without one.

    This typically indicates that the standard environment has not yet been initialized
    (e.g. lispenv.setup_standard_environment() was not called) and neither an explicit
    environment argument nor state.current_environment is available.
    """
    pass


class LispProgramError(LispError):
    """Exception for Common Lisp PROGRAM-ERROR condition.
    
    This is signaled when a program violates language rules that should be
    detected at run time, such as wrong number of arguments to a function.
    """
    def __init__(self, message="Program error"):
        super().__init__(message)


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
    def __iter__(self):
        # NIL should act as an empty sequence for iteration contexts
        return iter(())

    def __len__(self):
        return 0

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

def symbol_function(*args):
    """Get the function definition of a symbol.

    Accepts variable arguments so callers that omit the required symbol
    will receive a LispProgramError rather than a Python TypeError.
    """
    if len(args) != 1:
        raise LispProgramError(f"symbol-function: wrong number of arguments (got {len(args)}, expected 1)")
    symbol = args[0]
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
    """Test if a value is truthy in Lisp (anything except NIL and None).

    CLHS 5.1: a boolean-test position (IF/COND/AND/OR/WHEN/UNLESS/a loop
    end-test, ...) is a single-value context, so a `MultipleValues` result
    must be reduced to its primary value before this check. Without it,
    `(if (subtypep 'integer 'character) ...)` was unconditionally true,
    because the not-yet-unwrapped `MultipleValues` *wrapper object* is
    itself neither `NIL` nor `None` regardless of what SUBTYPEP decided --
    every two-values-returning predicate (SUBTYPEP, GETHASH, FIND-SYMBOL,
    ...) has the same defect in a test position. An ordinary function call
    does not hit this: evaluating an argument already reduces it to its
    primary value on the way into the call, which is why `(not (subtypep
    ...))` was fine but `(if (subtypep ...) ...)` was not.
    """
    if isinstance(value, MultipleValues):
        value = value.get_primary()
    return value is not NIL and value is not None


class _Omitted:
    """Marks an argument that was not supplied at all.

    A `=None` default cannot express this wherever NIL is itself a meaningful
    value -- and in Common Lisp it usually is. `(load f :if-does-not-exist
    nil)` must return NIL while `(load f)` must signal, `(copy-readtable nil)`
    asks for the *standard* readtable rather than the current one, and
    `(load f :verbose nil)` overrides `*LOAD-VERBOSE*` where an omitted
    `:verbose` defers to it. A builtin that defaults such a parameter to
    `None` cannot tell those apart and has to pick one, silently.
    """

    def __bool__(self):
        return False

    def __repr__(self):
        return '<omitted>'


#: The one "argument not supplied" sentinel. `readtable.py` had its own.
OMITTED = _Omitted()


def supplied(value):
    """True when `value` is a real argument rather than the OMITTED marker."""
    return not isinstance(value, _Omitted)


def is_symbol(value):
    """Test whether `value` is a Lisp SYMBOL (CLHS 4.2, Figure 4-2).

    The one predicate for "is this a symbol", because the answer is not a
    single `isinstance`: a symbol here is a `LispSymbol`, a `lispKeyword`
    (KEYWORD is a *subtype* of SYMBOL), or NIL -- which reaches Python as the
    `lispNull` singleton, as Python `None`, or as a `LispSymbol` named "NIL"
    interned in some other package.

    `SYMBOLP` used to spell this `type(obj) is LispSymbol`, an *exact* type
    test, so `(symbolp :foo)` and `(symbolp nil)` were both NIL while
    `(typep :foo 'symbol)` and `(typep nil 'symbol)` were both T -- two
    disagreeing interpretations of the same lattice question. Anything that
    dispatched on SYMBOLP (`(every #'symbolp *features*)`, the SETF/place
    machinery, LOOP's var-spec parsing) therefore saw keywords as non-symbols.
    """
    return (isinstance(value, LispSymbol)
            or isinstance(value, lispNull)
            or value is None)


def is_keyword(value):
    """Test whether `value` is a KEYWORD (CLHS 11.1.2.3.1): a symbol whose
    home package is KEYWORD. NIL is *not* a keyword, so this is not simply
    `is_symbol` narrowed."""
    return isinstance(value, lispKeyword)


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


class LispString(lispSequence):
    """Common Lisp STRING type - a mutable sequence of characters.
    
    Unlike Python strings, Common Lisp strings are mutable.
    This class stores characters in a list for mutability while
    providing string-like behavior.
    """
    
    def __init__(self, content='', element_type=None, fill_pointer=None, adjustable=False):
        """Initialize a LispString.
        
        Args:
            content: Initial string content (str or sequence of characters)
            element_type: Element type (CHARACTER or BASE-CHAR)
            fill_pointer: Fill pointer for variable-length strings
            adjustable: Whether the string is adjustable
        """
        if isinstance(content, str):
            self._data = list(content)
        elif isinstance(content, LispString):
            self._data = list(content._data)
        elif hasattr(content, '__iter__'):
            # Convert sequence of characters
            self._data = [c.char if isinstance(c, Character) else str(c) for c in content]
        else:
            self._data = list(str(content))
        
        self.element_type = element_type
        self.fill_pointer = fill_pointer
        self.adjustable = adjustable
    
    def _active(self):
        """The characters this string actually *has* -- those below the fill
        pointer, or all of them when it has none (CLHS 3.2.1, "active
        elements").

        `__len__` and `__iter__` already answered by this rule; `__str__` and
        `__repr__` answered by the whole backing store, so one object reported
        two different contents: for a fill-pointered "FOO" whose backing store
        is "FOOZZZZ", `len(s)` was 3 while `str(s)` was "FOOZZZZ". Every Python
        reader that goes through `str()` -- the string-designator resolvers,
        the printer, FORMAT -- therefore saw the inactive characters, which is
        what made `(provide s)` record the module under the wrong name.
        """
        if self.fill_pointer is not None:
            return ''.join(self._data[:self.fill_pointer])
        return ''.join(self._data)

    def __repr__(self):
        """Return string representation for reading."""
        # Escape special characters for Lisp reader
        escaped = self._active().replace('\\', '\\\\').replace('"', '\\"')
        return f'"{escaped}"'

    def __str__(self):
        """Return Python string representation."""
        return self._active()
    
    def __len__(self):
        """Return string length (respecting fill-pointer if set)."""
        if self.fill_pointer is not None:
            return self.fill_pointer
        return len(self._data)
    
    def __getitem__(self, index):
        """Get character at index."""
        if isinstance(index, slice):
            return LispString(''.join(self._data[index]))
        return self._data[index]
    
    def __setitem__(self, index, value):
        """Set character at index (mutable!)."""
        if isinstance(value, Character):
            value = value.char
        elif isinstance(value, str) and len(value) == 1:
            pass  # Already a character
        else:
            raise TypeError(f"String element must be a character, got {type(value)}")
        
        if isinstance(index, slice):
            # Handle slice assignment
            if isinstance(value, str):
                self._data[index] = list(value)
            else:
                self._data[index] = value
        else:
            self._data[index] = value
    
    def __iter__(self):
        """Iterate over characters."""
        limit = self.fill_pointer if self.fill_pointer is not None else len(self._data)
        return iter(self._data[:limit])
    
    def __eq__(self, other):
        """Compare strings."""
        if isinstance(other, LispString):
            return str(self) == str(other)
        elif isinstance(other, str):
            return str(self) == other
        return False
    
    def __hash__(self):
        """Allow strings to be used in sets/dicts (hash immutable view)."""
        return hash(str(self))
    
    def __add__(self, other):
        """Concatenate strings."""
        if isinstance(other, LispString):
            return LispString(str(self) + str(other))
        elif isinstance(other, str):
            return LispString(str(self) + other)
        raise TypeError(f"Cannot concatenate LispString with {type(other)}")
    
    def __contains__(self, item):
        """Check if character is in string."""
        if isinstance(item, Character):
            item = item.char
        return item in self._data
    
    @property
    def actual_length(self):
        """Return actual allocated length (ignoring fill-pointer)."""
        return len(self._data)
    
    def copy(self):
        """Return a mutable copy of this string."""
        result = LispString(self._data[:])
        result.element_type = self.element_type
        result.fill_pointer = self.fill_pointer
        result.adjustable = self.adjustable
        return result
    
    def resize(self, new_size, fill_char=' '):
        """Resize the string (for ADJUST-ARRAY)."""
        if new_size > len(self._data):
            self._data.extend([fill_char] * (new_size - len(self._data)))
        elif new_size < len(self._data):
            self._data = self._data[:new_size]
        if self.fill_pointer is not None and self.fill_pointer > new_size:
            self.fill_pointer = new_size
    
    # Common Python string methods for compatibility
    def upper(self):
        """Return uppercase copy."""
        return LispString(str(self).upper())
    
    def lower(self):
        """Return lowercase copy."""
        return LispString(str(self).lower())
    
    def capitalize(self):
        """Return capitalized copy."""
        return LispString(str(self).capitalize())
    
    def strip(self, chars=None):
        """Return stripped copy."""
        return LispString(str(self).strip(chars))
    
    def lstrip(self, chars=None):
        """Return left-stripped copy."""
        return LispString(str(self).lstrip(chars))
    
    def rstrip(self, chars=None):
        """Return right-stripped copy."""
        return LispString(str(self).rstrip(chars))
    
    def find(self, sub, start=0, end=None):
        """Find substring."""
        return str(self).find(str(sub) if isinstance(sub, LispString) else sub, start, end)
    
    def rfind(self, sub, start=0, end=None):
        """Find substring from right."""
        return str(self).rfind(str(sub) if isinstance(sub, LispString) else sub, start, end)
    
    def replace(self, old, new, count=-1):
        """Return copy with replacements."""
        old_str = str(old) if isinstance(old, LispString) else old
        new_str = str(new) if isinstance(new, LispString) else new
        return LispString(str(self).replace(old_str, new_str, count))
    
    def startswith(self, prefix, start=0, end=None):
        """Check if string starts with prefix."""
        prefix_str = str(prefix) if isinstance(prefix, LispString) else prefix
        return str(self).startswith(prefix_str, start, end)
    
    def endswith(self, suffix, start=0, end=None):
        """Check if string ends with suffix."""
        suffix_str = str(suffix) if isinstance(suffix, LispString) else suffix
        return str(self).endswith(suffix_str, start, end)
    
    def isalpha(self):
        """Check if all characters are alphabetic."""
        return str(self).isalpha()
    
    def isdigit(self):
        """Check if all characters are digits."""
        return str(self).isdigit()
    
    def isalnum(self):
        """Check if all characters are alphanumeric."""
        return str(self).isalnum()
    
    def isspace(self):
        """Check if all characters are whitespace."""
        return str(self).isspace()
    
    def split(self, sep=None, maxsplit=-1):
        """Split string."""
        sep_str = str(sep) if isinstance(sep, LispString) else sep
        return [LispString(s) for s in str(self).split(sep_str, maxsplit)]
    
    def join(self, iterable):
        """Join strings."""
        return LispString(str(self).join(str(s) if isinstance(s, LispString) else s for s in iterable))


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

    def __len__(self):
        count = 0
        current = self
        while isinstance(current, lispCons):
            count += 1
            current = current.cdr
        return count

    def _to_sequence(self):
        seq = []
        current = self
        while isinstance(current, lispCons):
            seq.append(current.car)
            current = current.cdr
        return seq

    def __getitem__(self, index):
        # Support slicing and integer indexing to behave like a sequence
        if isinstance(index, slice):
            seq = self._to_sequence()
            return tuple(seq[index])
        if isinstance(index, int):
            if index < 0:
                seq = self._to_sequence()
                return seq[index]
            current = self
            i = index
            while i > 0 and isinstance(current, lispCons):
                current = current.cdr
                i -= 1
            if not isinstance(current, lispCons):
                raise IndexError('list index out of range')
            return current.car
        raise TypeError('list indices must be integers or slices')


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


def primary_value(value):
    """Reduce a result to its primary value, for a single-value context.

    CLHS 2.4.1 / 5.1.3: everywhere but an explicitly multiple-value context,
    a form returning several values yields only the first, and one returning
    *zero* values yields NIL. That rule has one home here rather than an
    `isinstance(x, MultipleValues)` test at each site, because the sites that
    forgot it let a `MultipleValues` **object** escape as a Lisp value
    (standing rule 2): a `:key` function ending in `(floor (/ i 2))` handed
    the comparison a `#<MULTIPLEVALUES>` instead of a number, so
    `(subsetp '(1) '(0 2 3 4) :key ...)` compared two distinct wrapper objects
    and answered NIL.
    """
    if isinstance(value, MultipleValues):
        return value.get_primary()
    return value


__all__ = [
    # Exceptions
    'LispNotImplementedError', 'LispTypeError', 'LispError',
    'LispEndOfFileError', 'LispStreamError', 'LispEnvironmentError', 'LispProgramError',
    # Core Types
    'lispT', 'lispSequence', 'lispList', 'lispNull', 'LispSymbol',
    'lispKeyword', 'Character', 'LispString', 'lispCons', 'lispConsIterator',
    # Constants
    'NIL', 'T',
    # Symbol Operations
    'symbol_value', 'set_symbol_value', 'symbol_function',
    'set_symbol_function', 'symbol_plist', 'set_symbol_plist',
    # Utilities
    'lisp_bool', 'is_truthy', 'is_symbol', 'is_keyword', 'lisp_str', 'lisp_repr',
    'OMITTED', 'supplied',
    'MultipleValues', 'primary_value', 'py_str_map',
    # Binding helpers (internal but useful)
    'Binding', 'FunctionBinding', 'SpecialForm'
]
