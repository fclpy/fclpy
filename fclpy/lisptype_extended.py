"""
lisptype_extended - Extended Lisp type system with environments, packages, and conditions.

Provides the Environment class, package management, ANSI condition system,
and restart support for advanced error handling.
"""

from . import lisptype_basic
from .lisptype_basic import (
    LispSymbol, lispT, NIL, T, LispError, LispEnvironmentError,
    lispCons, Binding, FunctionBinding, SpecialForm,
    is_truthy, lisp_bool, lispKeyword
)


class Environment(lispT):
    """An execution environment for symbol bindings.
    
    Common Lisp environments contain bindings for variables and functions.
    This implementation supports lexical variable bindings and function definitions.
    """
    
    def __init__(self, parent=None):
        """Initialize an Environment.
        
        Args:
            parent: Optional parent environment for lexical scoping.
                    When a symbol is not found locally, the search continues in parent.
        """
        self.parent = parent
        self.bindings = None  # Singly-linked list of Binding objects
        self.function_bindings = None  # Singly-linked list of FunctionBinding objects
        # Legacy attributes for old API compatibility
        if parent is None:
            self.variable_bindings = None
            self.tag_bindings = None
        else:
            self.variable_bindings = parent.variable_bindings
            self.tag_bindings = parent.tag_bindings
    
    def bind(self, symbol, value):
        """Bind a symbol to a value in this environment.
        
        Creates a new local binding. If the symbol already has a binding in this
        environment, it creates a new binding that shadows the old one.
        
        Args:
            symbol: LispSymbol to bind
            value: Value to bind to the symbol
            
        Returns:
            The bound value
        """
        if not isinstance(symbol, LispSymbol):
            raise TypeError(f"bind: {symbol} is not a symbol")
        
        # Create new binding that shadows previous bindings
        self.bindings = Binding(symbol, value, self.bindings, env=self)
        return value
    
    def lookup(self, symbol):
        """Look up a symbol's value in this environment and parent environments.
        
        Args:
            symbol: LispSymbol to look up
            
        Returns:
            The value bound to the symbol, or None if not found
        """
        if not isinstance(symbol, LispSymbol):
            raise TypeError(f"lookup: {symbol} is not a symbol")
        
        # Check local bindings
        current_binding = self.bindings
        while current_binding is not None:
            if current_binding.symbol == symbol:
                return current_binding.value
            current_binding = current_binding.next
        
        # Check parent environment
        if self.parent is not None:
            return self.parent.lookup(symbol)
        
        return None
    
    def bind_function(self, symbol, func):
        """Bind a symbol to a function definition.
        
        Args:
            symbol: LispSymbol to bind
            func: Function to bind
            
        Returns:
            The bound function
        """
        if not isinstance(symbol, LispSymbol):
            raise TypeError(f"bind_function: {symbol} is not a symbol")
        
        self.function_bindings = FunctionBinding(symbol, func, self.function_bindings)
        return func
    
    def lookup_function(self, symbol):
        """Look up a symbol's function in this environment and parent environments.
        
        Args:
            symbol: LispSymbol to look up
            
        Returns:
            The function bound to the symbol, or None if not found
        """
        if not isinstance(symbol, LispSymbol):
            raise TypeError(f"lookup_function: {symbol} is not a symbol")
        
        # Check local function bindings
        current_binding = self.function_bindings
        while current_binding is not None:
            if current_binding.symbol == symbol:
                return current_binding.value
            current_binding = current_binding.next
        
        # Check parent environment
        if self.parent is not None:
            return self.parent.lookup_function(symbol)
        
        return None
    
    # ===== Legacy API for backward compatibility =====
    
    def add_function(self, symbol, value):
        """Legacy: add a function binding (use bind_function)."""
        self.bind_function(symbol, value)
    
    def find_func(self, sym):
        """Legacy: find a function by symbol name."""
        b = self.function_bindings
        while b is not None:
            if b.symbol.name == sym.name:
                return b.value
            b = b.next
        # Check parent
        if self.parent:
            return self.parent.find_func(sym)
        return None
    
    def add_variable(self, symbol, value):
        """Legacy: add a variable binding (use bind)."""
        self.variable_bindings = Binding(symbol, value, self.variable_bindings, self)
    
    def find_variable(self, sym):
        """Legacy: find a variable by symbol name."""
        b = self.variable_bindings
        while b is not None:
            if b.symbol.name == sym.name:
                return b.value
            b = b.next
        # Check parent
        if self.parent:
            return self.parent.find_variable(sym)
        return None
    
    def set_variable(self, sym, value):
        """Legacy: set a variable value."""
        b = self.variable_bindings
        while b is not None:
            if b.symbol.name == sym.name:
                b.value = value
                return value
            b = b.next
        # Check parent
        if self.parent:
            return self.parent.set_variable(sym, value)
        # Create new binding if not found
        self.add_variable(sym, value)
        return value
    
    def read_module(self, mod):
        """Legacy: read functions from a module."""
        for k, v in mod.__dict__.items():
            if callable(v) and not k.startswith("__"):
                self.add_function(py_str_to_sym(k), v)
    
    def __repr__(self):
        return f"<Environment {hex(id(self))}>"


class Package(lispT):
    """A Common Lisp package for namespace management.
    
    Packages allow grouping of symbols and controlling symbol visibility
    between different modules/namespaces. Each symbol in a package has
    a unique name within that package.
    """
    
    def __init__(self, name, use_packages=None, nick_names=None):
        """Initialize a Package.
        
        Args:
            name: String name of the package
            use_packages: List of package names to inherit symbols from
            nick_names: List of alternative names for the package
        """
        self.name = name
        self.nick_names = nick_names or []
        self.use_packages = use_packages or []
        self.symbols = {}  # Map from symbol name to LispSymbol
        self.external_symbols = set()  # Set of exported symbol names
    
    def intern(self, name, external=False):
        """Intern a symbol in this package.

        Returns an existing symbol with the given name, or creates a new one.
        Symbol names are uppercased per Common Lisp standard.
        
        Args:
            name: Symbol name (string)
            external: Whether to export the symbol
            
        Returns:
            LispSymbol object
        """
        # Uppercase the name per Common Lisp standard
        name = name.upper()
        
        if name not in self.symbols:
            symbol = LispSymbol(name, package=self)
            self.symbols[name] = symbol
        else:
            symbol = self.symbols[name]
        
        if external:
            self.external_symbols.add(name)
        
        return symbol
    
    def intern_symbol(self, name, external=False):
        """Alias for intern() - interned a symbol in this package.

        Returns an existing symbol with the given name, or creates a new one.
        Symbol names are uppercased per Common Lisp standard.
        
        Args:
            name: Symbol name (string)
            external: Whether to export the symbol
            
        Returns:
            LispSymbol object
        """
        return self.intern(name, external)
    
    def find_symbol(self, name):
        """Find a symbol in this package.
        
        Returns a tuple of (symbol, status) where status is one of:
        - ':INTERNAL' if symbol exists but not exported
        - ':EXTERNAL' if symbol exists and is exported
        - (None, None) if symbol not found
        
        Args:
            name: Symbol name to search for
            
        Returns:
            Tuple of (LispSymbol or None, status string or None)
        """
        symbol = self.symbols.get(name, None)
        if symbol is None:
            return (None, None)
        
        status = ':EXTERNAL' if name in self.external_symbols else ':INTERNAL'
        return (symbol, status)
    
    def export_symbol(self, name):
        """Export a symbol from this package.
        
        Args:
            name: Symbol name (string) or LispSymbol
        """
        if isinstance(name, LispSymbol):
            name = name.name
        if name in self.symbols:
            self.external_symbols.add(name)
    
    def import_symbol(self, symbol):
        """Import a symbol into this package.
        
        Args:
            symbol: LispSymbol to import
        """
        if not isinstance(symbol, LispSymbol):
            raise TypeError("import_symbol expects a LispSymbol")
        # Add without making it external by default
        self.symbols[symbol.name] = symbol
    
    def __repr__(self):
        return f"#<PACKAGE {self.name}>"


# Global packages
KEYWORD_PACKAGE = Package("KEYWORD")
COMMON_LISP_PACKAGE = Package("COMMON-LISP")
COMMON_LISP_USER_PACKAGE = Package("COMMON-LISP-USER", use_packages=["COMMON-LISP"])


def make_package(name, use_packages=None, nick_names=None):
    """Create a new package.
    
    Args:
        name: Package name
        use_packages: List of package names to inherit symbols from
        nick_names: List of package nicknames
        
    Returns:
        Package object
    """
    return Package(name, use_packages=use_packages, nick_names=nick_names)


def find_package(name):
    """Find a package by name or nickname.
    
    Args:
        name: Package name or nickname
        
    Returns:
        Package object or None if not found
    """
    # Check built-in packages
    if name == "KEYWORD":
        return KEYWORD_PACKAGE
    if name == "COMMON-LISP" or name == "CL":
        return COMMON_LISP_PACKAGE
    if name == "COMMON-LISP-USER" or name == "CL-USER":
        return COMMON_LISP_USER_PACKAGE
    
    return None


def intern_symbol(name, package=None):
    """Intern a symbol in a package.
    
    If the symbol already exists, returns the existing symbol.
    Otherwise creates and returns a new symbol.
    Symbol names are case-normalized (converted to uppercase) for 
    case-insensitive comparison per ANSI Common Lisp standard.
    
    Args:
        name: Symbol name (string or LispSymbol)
        package: Package object (default: COMMON-LISP-USER)
        
    Returns:
        LispSymbol
    """
    if isinstance(name, LispSymbol):
        return name
    
    # Normalize name to uppercase for case-insensitive interning
    if isinstance(name, str):
        name = name.upper()
    else:
        name = str(name).upper()
    
    if package is None:
        package = COMMON_LISP_USER_PACKAGE
    elif isinstance(package, str):
        package = find_package(package) or make_package(package)
    
    if not isinstance(package, Package):
        raise TypeError(f"intern_symbol: {package} is not a package")
    
    return package.intern_symbol(name, external=False)


def intern_keyword(name):
    """Intern a keyword (interned in KEYWORD package and auto-exported).
    
    Args:
        name: Keyword name (without leading colon) or lispKeyword object
        
    Returns:
        lispKeyword in keyword package
    """
    # If it's already a lispKeyword, return it
    if isinstance(name, lispKeyword):
        return name
    
    # Convert to string and strip leading colon if present
    name = str(name)
    if name.startswith(':'):
        name = name[1:]
    
    # Normalize to uppercase (Common Lisp keyword convention)
    name = name.upper()
    
    # Check if already interned
    if name in KEYWORD_PACKAGE.symbols:
        return KEYWORD_PACKAGE.symbols[name]
    
    # Create new lispKeyword
    keyword = lispKeyword(name, package=KEYWORD_PACKAGE)
    KEYWORD_PACKAGE.symbols[name] = keyword
    KEYWORD_PACKAGE.external_symbols.add(name)
    
    return keyword


class Condition(lispT):
    """Base class for ANSI Common Lisp conditions.
    
    The condition system is used for error handling and recovery. Conditions
    are objects that encapsulate abnormal situations and can be handled
    with handlers and restarts.
    """
    
    def __init__(self, message="", **kwargs):
        """Initialize a Condition.
        
        Args:
            message: Condition message
            **kwargs: Additional condition attributes (stored as slots)
        """
        self._slots = {'message': message}
        self._slots.update(kwargs)
        self.format_args = []
    
    @property
    def message(self):
        """Get the message slot."""
        return self._slots.get('message', '')
    
    @message.setter
    def message(self, value):
        """Set the message slot."""
        self._slots['message'] = value
    
    def get_slot(self, name):
        """Get the value of a named slot.
        
        Args:
            name: Slot name (string, with or without hyphens)
            
        Returns:
            Slot value or None if not found
        """
        # Try exact match first
        if name in self._slots:
            return self._slots[name]
        # Try with hyphen-to-underscore conversion
        underscore_name = name.replace('-', '_')
        if underscore_name in self._slots:
            return self._slots[underscore_name]
        # Try with underscore-to-hyphen conversion
        hyphen_name = name.replace('_', '-')
        if hyphen_name in self._slots:
            return self._slots[hyphen_name]
        return None
    
    def set_slot(self, name, value):
        """Set the value of a named slot.
        
        Args:
            name: Slot name (string)
            value: Value to set
        """
        self._slots[name] = value
    
    def __str__(self):
        """Return string representation of the condition."""
        return self.message
    
    def __repr__(self):
        """Return detailed representation with uppercase class name."""
        return f"<{self.__class__.__name__.upper()}: {self.message}>"


class SimpleCondition(Condition):
    """A simple condition with just a message."""
    pass


class Warning(Condition):
    """Base class for warning conditions."""
    pass


class Error(Condition):
    """Base class for error conditions."""
    pass


class TypeError(Error):
    """Condition raised when an argument has an unexpected type."""
    def __init__(self, datum=None, expected_type=None, message="", **kwargs):
        if not message and datum is not None:
            message = f"Type error: expected {expected_type}, got {datum}"
        super().__init__(message, **kwargs)
        self._slots['datum'] = datum
        self._slots['expected-type'] = expected_type


class ProgramError(Error):
    """Condition for program errors (control flow issues)."""
    pass


class ControlError(Error):
    """Condition for control flow errors."""
    pass


class FileError(Error):
    """Condition for file operation errors."""
    pass


class StreamError(Error):
    """Condition for stream operation errors."""
    pass


class EndOfFile(StreamError):
    """Condition raised when EOF is reached unexpectedly."""
    def __init__(self, stream=None, message="End of file", **kwargs):
        super().__init__(message, **kwargs)
        if stream is not None:
            self._slots['stream'] = stream


class ArithmeticError(Error):
    """Condition for arithmetic errors."""
    def __init__(self, operation=None, operands=None, message="", **kwargs):
        if not message and operation is not None:
            message = f"Arithmetic error in operation {operation}"
        super().__init__(message, **kwargs)
        if operation is not None:
            self._slots['operation'] = operation
        if operands is not None:
            self._slots['operands'] = operands


class DivisionByZero(ArithmeticError):
    """Condition raised for division by zero."""
    def __init__(self, operation=None, operands=None, message="Division by zero", **kwargs):
        super().__init__(operation=operation, operands=operands, message=message, **kwargs)


class FloatingPointInvalidOperation(ArithmeticError):
    """Condition for invalid floating point operations."""
    pass


class FloatingPointOverflow(ArithmeticError):
    """Condition for floating point overflow."""
    pass


class FloatingPointUnderflow(ArithmeticError):
    """Condition for floating point underflow."""
    pass


def resolve_environment(env=None):
    """Resolve an environment argument, using current environment if needed.
    
    Args:
        env: Environment object, or None to use current environment
        
    Returns:
        Environment object
        
    Raises:
        LispEnvironmentError: If env is None and no current environment available
    """
    import fclpy.state as state
    
    if env is not None:
        return env
    
    if state.current_environment is not None:
        return state.current_environment
    
    raise LispEnvironmentError(
        "No active environment. Call setup_standard_environment() first."
    )


def py_str_to_sym(s):
    """Convert a Python string to a Lisp symbol, handling special character mapping.
    
    This converts underscores and special markers to Lisp-style names.
    For example: _S_STAR_ -> *, _S_PLUS_ -> +, etc.
    
    Args:
        s: String to convert
        
    Returns:
        LispSymbol in COMMON-LISP-USER package
    """
    from .lisptype_basic import py_str_map
    
    s = s.upper()
    for pattern, replacement in py_str_map:
        s = s.replace(pattern, replacement)
    return intern_symbol(s, COMMON_LISP_USER_PACKAGE)


class Restart(lispT):
    """A restart point for error recovery.
    
    Restarts provide named recovery points that error handlers can use
    to continue execution from a known state with corrected values.
    """
    
    def __init__(self, name, handler, report=None, test=None):
        """Initialize a Restart.
        
        Args:
            name: Restart name (string or symbol)
            handler: Callable that performs the restart
            report: Optional function that reports restart to user
            test: Optional predicate to test if restart is applicable
        """
        if isinstance(name, str):
            self.name = LispSymbol(name)
        else:
            self.name = name
        self.handler = handler
        self.report = report
        self.test = test
    
    def test_applicability(self, condition=None):
        """Test if this restart is applicable to a condition.
        
        Args:
            condition: Condition object to test against
            
        Returns:
            True if restart is applicable, False otherwise
        """
        if self.test is None:
            return True
        return self.test(condition)
    
    def get_report(self):
        """Get the restart's report message.
        
        Returns:
            String describing the restart
        """
        if self.report is None:
            return f"Restart {self.name.name}"
        if callable(self.report):
            return self.report()
        return str(self.report)
    
    def __repr__(self):
        return f"<Restart {self.name.name}>"


class RestartException(Exception):
    """Internal exception used to invoke a restart.
    
    When a restart is invoked, this exception is raised to unwind the stack
    to the restart point, carrying the restart's values with it.
    """
    
    def __init__(self, restart, values=None):
        """Initialize a RestartException.
        
        Args:
            restart: Restart object or restart name (string/symbol) being invoked
            values: Optional values to return from the restart
        """
        self.restart = restart
        self.values = values or []
        # Handle both Restart objects and string names
        if isinstance(restart, str):
            self.restart_name = restart
            super().__init__(f"Restart: {restart}")
        elif isinstance(restart, Restart):
            self.restart_name = restart.name.name if hasattr(restart.name, 'name') else str(restart.name)
            super().__init__(f"Restart: {self.restart_name}")
        else:
            # Assume it has a name attribute
            self.restart_name = str(restart)
            super().__init__(f"Restart: {self.restart_name}")


__all__ = [
    # Environment
    'Environment',
    # Package system
    'Package', 'KEYWORD_PACKAGE', 'COMMON_LISP_PACKAGE', 'COMMON_LISP_USER_PACKAGE',
    'make_package', 'find_package', 'intern_symbol', 'intern_keyword',
    # Conditions (ANSI condition system)
    'Condition', 'SimpleCondition', 'Warning', 'Error',
    'TypeError', 'ProgramError', 'ControlError', 'FileError', 'StreamError',
    'EndOfFile', 'ArithmeticError', 'DivisionByZero',
    'FloatingPointInvalidOperation', 'FloatingPointOverflow', 'FloatingPointUnderflow',
    # Restarts
    'Restart', 'RestartException',
    # Utilities
    'resolve_environment',
    'py_str_to_sym'
]
