"""
lisptype_extended - Extended Lisp type system with environments, packages, and conditions.

Provides the Environment class, package management, ANSI condition system,
and restart support for advanced error handling.
"""

from fclpy.lisptype_basic import (
    LispSymbol, lispT, NIL, T, LispError, LispEnvironmentError,
    lispCons, Binding, FunctionBinding, SpecialForm,
    is_truthy, lisp_bool, lispKeyword, lispCons, py_str_map, LispString
)


def _same_variable(a, b):
    """Whether `a` and `b` denote the same variable (CLHS 3.1.2.1.1).

    Variable binding and lookup are by symbol *identity*: an uninterned
    ``#:x`` beside an interned ``x`` is a different variable, and two
    same-named symbols from different packages are two variables (let.5).

    One documented fallback mirrors `evaluation_control_flow._tags_match`'s:
    when *both* symbols are uninterned (no package) and share a name, they
    denote the same variable. That is the case of a macro expander that
    built its expansion's binding and references as separate fresh
    `LispSymbol` objects -- standard_macros' ``_sym('C')`` in the
    IGNORE-ERRORS expansion binds one and references another -- where the
    expander's own spellings of its one variable must unify, while the
    reader-uninterned/interned pair stays distinct.
    """
    if a is b:
        return True
    return (isinstance(a, LispSymbol) and isinstance(b, LispSymbol)
            and getattr(a, 'package', None) is None
            and getattr(b, 'package', None) is None
            and a.name == b.name)


class Environment(lispT):
    """An execution environment for symbol bindings.

    Common Lisp environments contain bindings for variables and functions.
    This implementation supports lexical variable bindings and function definitions.

    **The global environment holds no lexical variable bindings.** CLHS 3.1.1.1:
    the global environment's variable bindings are the dynamic ones, and Common
    Lisp has no such thing as a global lexical variable. So for the parentless
    environment at the root of every chain, a variable's one and only home is
    the symbol's value cell -- the same cell ``SYMBOL-VALUE``, ``BOUNDP``,
    ``SET``, ``MAKUNBOUND``, ``PROGV`` and every dynamic binding read and write.

    It used to have its own binding list as well, and that second home is what
    plan.md's 2026-08-15 changelog entry describes as "a special variable has
    two homes": ``DEFVAR``/``DEFPARAMETER`` and the bootstrap wrote the global
    *lexical* binding, `SETQ` maintained it, and every dynamic-binding operator
    wrote the value cell -- so ``(defvar *x* 1)`` left ``(boundp '*x*)`` NIL and
    ``(let ((*x* 2)) *x*)`` read 1, because the global lexical binding shadowed
    the dynamic binding the binding form had correctly established. Deleting
    the home Common Lisp does not have is what reconciles them: a dynamic
    binding now writes the only cell a reference can reach.
    """

    @property
    def is_global(self):
        """True for the environment at the root of the chain.

        Its variables live in their symbols' value cells rather than in a
        binding list of its own -- see the class docstring.
        """
        return self.parent is None

    def __init__(self, parent=None):
        """Initialize an Environment.
        
        Args:
            parent: Optional parent environment for lexical scoping.
                    When a symbol is not found locally, the search continues in parent.
        """
        self.parent = parent
        self.bindings = None  # Singly-linked list of Binding objects
        self.function_bindings = None  # Singly-linked list of FunctionBinding objects
        self.symbol_macros = {}  # Dict of symbol-macro bindings: symbol.name -> expansion

        # Fast caches to speed up legacy APIs (find_func/find_variable).
        # The *function* caches are name-keyed: `find_func`'s documented
        # contract is name-based lookup, and `_function_map_by_symbol` below
        # is the identity overlay that keeps shadowed symbols distinct. The
        # *variable* cache is keyed by the symbol object itself: CLHS variable
        # binding and lookup are by symbol identity, so two same-named
        # symbols -- an uninterned `#:x` beside an interned `x` (let.5), or
        # the same name from two different packages -- are two variables.
        self._function_map = {}
        self._variable_map = {}
        # Identity-keyed overlay `find_func` checks *first* -- see its
        # docstring for why the name-only cache above cannot be replaced
        # outright: `standard_macros.py`'s expanders build fresh, uninterned
        # operator symbols (`LispSymbol('DOLIST')`, etc.) purely to *name* an
        # existing binding in a generated form, and those must keep resolving
        # by name. This dict is what makes a real, interned, *shadowed*
        # symbol (e.g. a package that `(shadow '(handler-bind))`s and
        # `DEFMACRO`s its own) resolve to its *own* binding instead of
        # colliding, by bare name, with `COMMON-LISP:HANDLER-BIND`'s.
        self._function_map_by_symbol = {}
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

        if self.is_global:
            # The global environment has no lexical variables; its one home
            # for a variable is the symbol's value cell.
            symbol.value = value
            return value

        # Create new binding that shadows previous bindings
        self.bindings = Binding(symbol, value, self.bindings, env=self)
        # Keep the identity-keyed variable cache in step.
        try:
            self._variable_map[symbol] = value
        except Exception:
            pass
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

        if self.is_global:
            return symbol.value

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
        # Keep legacy name-based function lookup fast.
        try:
            self._function_map[symbol.name] = func
        except Exception:
            pass
        # Identity-keyed overlay -- see the __init__ comment on
        # `_function_map_by_symbol`.
        try:
            self._function_map_by_symbol[symbol] = func
        except Exception:
            pass
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

    def unbind_function(self, symbol):
        """Remove `symbol`'s function binding *in this environment*.

        The one place a function binding is removed, because a function
        definition is recorded in two structures here -- the
        `function_bindings` linked list and the `_function_map` name cache
        `find_func` consults first -- and a removal that forgets the cache
        does not remove anything observable. That was FMAKUNBOUND: it unlinked
        the list node and left the cache, so `(fboundp g)` stayed T for ever
        afterwards. The ANSI suite's `compile-file-test` and `load-file-test`
        both open with `(fmakunbound funname)` and then assert the function is
        *not* defined, so this one stale cache entry failed sixteen
        system-construction tests that had nothing to do with function cells.

        Returns True if a binding was removed.
        """
        if not isinstance(symbol, LispSymbol):
            raise TypeError(f"unbind_function: {symbol} is not a symbol")

        removed = self._function_map.pop(symbol.name, None) is not None
        if self._function_map_by_symbol.pop(symbol, None) is not None:
            removed = True

        previous = None
        node = self.function_bindings
        while node is not None:
            if node.symbol.name == symbol.name:
                if previous is None:
                    self.function_bindings = node.next
                else:
                    previous.next = node.next
                removed = True
                break
            previous = node
            node = node.next
        return removed
    
    def find_func(self, sym):
        """Legacy: find a function by symbol name.

        Checks the identity-keyed overlay first (`_function_map_by_symbol`):
        an exact match there means `sym` is a real, interned symbol with its
        *own* binding, which must win over a same-named binding installed
        under a different (e.g. shadowed-in-another-package) symbol object.
        Only when there is no exact match does this fall back to the
        name-only cache, which is what lets generated code reference an
        existing binding through a fresh, uninterned symbol built purely to
        carry a name (`standard_macros.py`'s `LispSymbol('DOLIST')`, etc.).
        """
        try:
            if sym in self._function_map_by_symbol:
                return self._function_map_by_symbol[sym]
        except Exception:
            pass
        try:
            if sym.name in self._function_map:
                return self._function_map[sym.name]
        except Exception:
            pass
        b = self.function_bindings
        while b is not None:
            if b.symbol.name == sym.name:
                try:
                    self._function_map[sym.name] = b.value
                except Exception:
                    pass
                return b.value
            b = b.next
        # Check parent
        if self.parent:
            return self.parent.find_func(sym)
        return None
    
    def add_variable(self, symbol, value):
        """Legacy: add a variable binding (use bind)."""
        # Accept either a direct LispSymbol or a cons of the form
        # (:KEYWORD var) where the variable is the second element.
        # This handles lambda-list key argument syntaxes that present
        # parameter names as a cons pairing a keyword and a symbol.

        if isinstance(symbol, lispCons):
            # Attempt to extract the actual variable from the cdr
            try:
                # Handle common forms:
                # - (:keyword var)  -> symbol.cdr is a cons whose car is the var
                # - (head . tail)   -> dotted pair where cdr is the tail symbol
                cdr = symbol.cdr
                actual = None
                if isinstance(cdr, LispSymbol):
                    # Dotted-pair style: (head . tail) -> tail is the variable
                    actual = cdr
                elif isinstance(cdr, lispCons):
                    # Normal list: (:keyword var) -> cdr.car is the var
                    actual = cdr.car if isinstance(cdr.car, LispSymbol) else None
            except Exception:
                actual = None

            if isinstance(actual, LispSymbol):
                symbol = actual
            else:
                raise TypeError(f"add_variable: {symbol} is not a symbol, has type {type(symbol)}")

        if not isinstance(symbol, LispSymbol):
            raise TypeError(f"add_variable: {symbol} is not a symbol, has type {type(symbol)}")

        if self.is_global:
            # No lexical binding list here, and no `_variable_map` entry
            # either -- a cached copy of a value that lives in the symbol
            # would be the two-homes defect over again, one indirection down.
            symbol.value = value
            return

        self.variable_bindings = Binding(symbol, value, self.variable_bindings, self)
        self._variable_map[symbol] = value


    
    def has_variable(self, sym: LispSymbol):
        """Check if a variable binding exists (distinguishes unbound from bound-to-None).

        Binding and lookup are by symbol *identity* (CLHS 3.1.2.1.1): a
        binding made for one symbol object is invisible to another with the
        same name -- let.5's `(let ((x 0)) (let ((#:x 1)) x))` must see the
        outer binding, and two same-named symbols from different packages are
        two variables.
        """
        if self.is_global:
            # Python None is the "unbound" marker in a value cell; NIL is a
            # distinct object, so a variable bound to NIL reads as bound.
            return getattr(sym, 'value', None) is not None

        # Fast-path: identity-keyed map populated by bind/add_variable
        try:
            if sym in self._variable_map:
                return True
        except Exception:
            pass

        # Check modern lexical bindings list (self.bindings)
        b = self.bindings
        while b is not None:
            if _same_variable(b.symbol, sym):
                return True
            b = b.next

        # Legacy variable_bindings (kept for backward compatibility)
        b = self.variable_bindings
        while b is not None:
            if _same_variable(b.symbol, sym):
                return True
            b = b.next

        # Check parent environment
        if self.parent:
            return self.parent.has_variable(sym)
        return False
    
    def find_variable(self, sym):
        """Legacy: find a variable by symbol identity.

        The variable cache is identity-keyed and the binding lists compare
        the symbol object, so a binding of one symbol is never found through
        another of the same name (see `has_variable`).
        """
        if self.is_global:
            return getattr(sym, 'value', None)

        # Prefer walking the variable_bindings linked list to find the
        # most-recent binding value. Only use the identity-keyed cache as a
        # fallback to avoid returning stale cached values from a different
        # environment frame.
        b = self.variable_bindings
        while b is not None:
            if _same_variable(b.symbol, sym):
                try:
                    self._variable_map[sym] = b.value
                except Exception:
                    pass
                return b.value
            b = b.next

        try:
            if sym in self._variable_map:
                return self._variable_map[sym]
        except Exception:
            pass

        # Check parent
        if self.parent:
            return self.parent.find_variable(sym)
        return None
    
    def set_variable(self, sym, value):
        """Legacy: set a variable value."""
        if self.is_global:
            # SETQ of a name with no lexical binding assigns the value cell,
            # which is where the global environment keeps its variables. This
            # is also the end of the chain, so it is where every unshadowed
            # assignment lands.
            sym.value = value
            return value

        b = self.variable_bindings
        while b is not None:
            if _same_variable(b.symbol, sym):
                b.value = value
                try:
                    self._variable_map[sym] = value
                except Exception:
                    pass
                return value
            b = b.next
        # Check parent
        if self.parent:
            return self.parent.set_variable(sym, value)
        # Create new binding if not found
        self.add_variable(sym, value)
        
        return value
    
    def add_symbol_macro(self, symbol, expansion):
        """Add a symbol-macro binding (for SYMBOL-MACROLET).
        
        Symbol macros are replaced with their expansion whenever the symbol
        is evaluated or used in a form (except in QUOTE contexts).
        
        Args:
            symbol: LispSymbol to bind as a symbol-macro
            expansion: The expansion form (unevaluated)
        """
        if isinstance(symbol, LispSymbol):
            self.symbol_macros[symbol.name] = expansion
    
    def get_symbol_macro(self, symbol):
        """Get a symbol-macro expansion if it exists.
        
        Looks up the symbol-macro in this environment and parent environments.
        
        Args:
            symbol: LispSymbol to look up
            
        Returns:
            The expansion form if a symbol-macro binding exists, else None
        """
        if not isinstance(symbol, LispSymbol):
            return None
        name = symbol.name
        env = self
        while env is not None:
            expansion = env.symbol_macros.get(name)
            if expansion is not None:
                return expansion
            # CLHS 3.1.2.1.1: a *variable* binding shadows a symbol macro of
            # the same name established further out. Variables and symbol
            # macros live in separate structures, and a child shares its
            # parent's `variable_bindings` list, so neither can out-rank the
            # other by position on its own -- this walk compares them, using
            # `_variable_map`, which holds only *this* environment's own
            # bindings and is keyed by the symbol itself. Without it, `(let
            # ((x :a)) (let ((x :b)) (declare (special x)) ...))` -- where
            # the inner binding installs a `%SPECIAL-REF` redirection --
            # would leave that redirection visible to any *enclosing* lexical
            # X, and a plain `(symbol-macrolet ((x 1)) (let ((x 2)) x))`
            # answered 1.
            if symbol in env._variable_map:
                return None
            env = env.parent
        return None
    
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
        self.shadowing_symbols = set()  # Set of shadowing symbol names (CLHS 11.1.2.3)
        # CLHS 25.1.3: `(documentation package t)` / `(setf (documentation
        # package t) doc)`. DEFPACKAGE's :documentation option and the SETF
        # place both live here.
        self.documentation = None
    
    @property
    def use_list(self):
        """Alias for use_packages for compatibility with package functions."""
        return self.use_packages
    
    def intern(self, name, external=False, exact_case=False):
        """Intern a symbol in this package.

        Returns an existing symbol with the given name, or creates a new one.
        Symbol names are uppercased per Common Lisp standard -- that
        normalization is really the *reader*'s job (CLHS 23.1.2's
        readtable-case, applied only to characters that were not escaped
        with `\\`/`|...|`), not this function's, but every existing caller
        already hands in a name it wants upcased wholesale. `exact_case=True`
        opts out for a caller (the reader) that has already resolved the
        correct per-character case itself and must not have it clobbered --
        without this, a pipe-escaped `|abc|` and plain `ABC` always interned
        as the same symbol, because this method force-upcased both.

        If the symbol is inherited from a used package, returns the inherited
        symbol (Common Lisp semantics).

        Args:
            name: Symbol name (string)
            external: Whether to export the symbol
            exact_case: use `name` exactly as given, skipping the upcase

        Returns:
            LispSymbol object
        """
        if not exact_case:
            name = name.upper()
        
        # DEBUG: Trace T intern attempts
        # First check if symbol already exists in this package
        if name in self.symbols:
            symbol = self.symbols[name]
            if external:
                self.external_symbols.add(name)
            return symbol
        
        # Check if symbol is inherited from a used package
        for used_pkg in getattr(self, 'use_packages', []):
            # Handle both Package objects and package names
            if isinstance(used_pkg, str):
                used_pkg = find_package(used_pkg)
            if used_pkg is not None and hasattr(used_pkg, 'external_symbols'):
                # Only look at external symbols of used packages
                if name in used_pkg.external_symbols:
                    sym = used_pkg.symbols.get(name)
                    if sym is not None:
                        if external:
                            # CLHS 11.1.2.1.2: exporting a name this package
                            # only inherits makes that symbol directly
                            # *present* here too, not merely inherited -- the
                            # same symbol object, not a copy, so its home
                            # package is unchanged. Skipping this left
                            # `(export ...)`/DEFPACKAGE's :EXPORT unable to
                            # promote an inherited symbol: FIND-SYMBOL kept
                            # answering :INHERITED because the name was never
                            # added to `self.symbols`.
                            self.symbols[name] = sym
                            self.external_symbols.add(name)
                        # Return the inherited symbol (don't create a new one)
                        return sym
        
        # Symbol not found anywhere - create new one in this package.
        #
        # This used to special-case name == 'T' to unconditionally return
        # the canonical COMMON-LISP:T object here, regardless of `self` --
        # so `(intern "T" "KEYWORD")` answered plain T instead of :T
        # (KEYWORD-P NIL), and any package not using COMMON-LISP got the
        # wrong symbol identity for "T" instead of one of its own. The
        # bootstrap already registers the canonical T as
        # COMMON_LISP_PACKAGE.symbols['T'], and every package that :uses
        # COMMON-LISP reaches it through the inherited-symbol loop above, so
        # the special case was redundant for the normal case and wrong for
        # every package that does not use COMMON-LISP (KEYWORD, or a
        # `(defpackage ... (:use))` with none) -- structures/structures-02
        # .lsp's STRUCT-TEST-65 interns a slot literally named T into
        # KEYWORD and needs the real :T back.
        if self.name.upper() == "KEYWORD":
            symbol = lispKeyword(name, package=self)  # special self-evaluating symbol
        else:
            symbol = LispSymbol(name, package=self)
        self.symbols[name] = symbol
        
        if external:
            self.external_symbols.add(name)
        
        return symbol
    
    def intern_symbol(self, name, external=False, exact_case=False):
        """Alias for intern() - interned a symbol in this package.

        Returns an existing symbol with the given name, or creates a new one.
        Symbol names are uppercased per Common Lisp standard, unless
        `exact_case` says the caller already resolved the case itself.

        Args:
            name: Symbol name (string)
            external: Whether to export the symbol
            exact_case: use `name` exactly as given, skipping the upcase

        Returns:
            LispSymbol object
        """
        return self.intern(name, external, exact_case=exact_case)
    
    def find_symbol(self, name):
        """Find a symbol in this package.

        Returns a tuple of (symbol, status) where status is one of:
        - ':INTERNAL' if symbol exists in this package but not exported
        - ':EXTERNAL' if symbol exists in this package and is exported
        - ':INHERITED' if symbol is inherited from a used package
        - (None, None) if symbol not found

        `name` is a *string designator* (CLHS 11.2): a string, a symbol,
        a character, or a specialized character array. The
        `find-symbol.17`-.21` family pass displaced, fill-pointered, and
        adjustable arrays whose `.upper()`-equivalent is the same as
        the simple "FOO" string the symbol is stored under; before
        normalizing, `self.symbols.get(displaced_array)` answered NIL
        because the key is a plain `str` and the designator is not.
        """
        from .lispfunc.comparison import _string_characters
        normalized = _string_characters(name)
        if normalized is not None:
            name = normalized
        elif isinstance(name, (lisptype.lispKeyword, lisptype.LispSymbol)):
            name = name.name
        elif isinstance(name, lisptype.Character):
            name = name.char
        # First check this package's own symbols
        symbol = self.symbols.get(name, None)
        if symbol is not None:
            status = ':EXTERNAL' if name in self.external_symbols else ':INTERNAL'
            return (symbol, status)

        # Check inherited symbols from used packages
        for used_pkg in getattr(self, 'use_packages', []):
            # Handle both Package objects and package names
            if isinstance(used_pkg, str):
                used_pkg = find_package(used_pkg)
            if used_pkg is not None and hasattr(used_pkg, 'external_symbols'):
                # Only look at external symbols of used packages
                if name in used_pkg.external_symbols:
                    sym = used_pkg.symbols.get(name)
                    if sym is not None:
                        return (sym, ':INHERITED')

        return (None, None)
    
    def export_symbol(self, name):
        """Export a symbol from this package (CLHS 11.2).

        If a symbol is inherited from a used package, exporting it promotes
        it to be directly present in this package (same symbol object,
        unchanged home package). If a symbol is internal to this package,
        it becomes external.

        Args:
            name: Symbol name (string) or LispSymbol
        """
        # Normalize input to symbol object and name string
        symbol_obj = None
        if isinstance(name, LispSymbol):
            symbol_obj = name
            name_str = name.name
        else:
            name_str = str(name).upper()
            # If the symbol exists in this package, use it
            symbol_obj = self.symbols.get(name_str)

        # If symbol not found locally, check inherited symbols (CLHS 11.1.2.1.2)
        if symbol_obj is None:
            for used_pkg in getattr(self, 'use_packages', []):
                # Handle both Package objects and package names
                if isinstance(used_pkg, str):
                    used_pkg = find_package(used_pkg)
                if used_pkg is not None and hasattr(used_pkg, 'external_symbols'):
                    # Only look at external symbols of used packages
                    if name_str in used_pkg.external_symbols:
                        sym = used_pkg.symbols.get(name_str)
                        if sym is not None:
                            # Promote the inherited symbol to be directly present
                            symbol_obj = sym
                            self.symbols[name_str] = sym
                            break

        # If still no symbol found, create a new one
        if symbol_obj is None:
            symbol_obj = LispSymbol(name_str, package=self)
            self.symbols[name_str] = symbol_obj

        # Export the symbol (add to external_symbols set)
        self.external_symbols.add(name_str)
    
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
COMMON_LISP_PACKAGE = Package("COMMON-LISP", nick_names=['CL'])

# CRITICAL: Inject the canonical T and NIL symbols into COMMON-LISP package
# T is created without a package in lisptype_basic.py
# We must register it in the COMMON-LISP package so that intern_symbol finds it
COMMON_LISP_PACKAGE.symbols['T'] = T
COMMON_LISP_PACKAGE.external_symbols.add('T')
T.package = COMMON_LISP_PACKAGE

# NIL needs exactly the same seeding: it is the `lispNull` singleton, not a
# LispSymbol at all, but `(intern "NIL" :cl)` and `(find-symbol "NIL" :cl)`
# must answer *the* symbol NIL -- eql to the object the reader returns for
# the token `nil` (lispreader.py special-cases the two tokens identically).
# Without the seeding, the bootstrap's interning of the 978 ANSI names grew
# COMMON-LISP a second, *distinct* LispSymbol named "NIL", so
# `(eq (intern "NIL" :cl) nil)` was false and every EQL-based list operation
# failed to see the two as one symbol -- ansi-test's `boundp.5` collects
# NIL out of `*cl-non-variable-constant-symbols*` exactly because that
# set-difference compares with EQL and *cl-symbols* interns its names while
# *cl-constant-symbols* quotes them.
COMMON_LISP_PACKAGE.symbols['NIL'] = NIL
COMMON_LISP_PACKAGE.external_symbols.add('NIL')

COMMON_LISP_USER_PACKAGE = Package("COMMON-LISP-USER", use_packages=["COMMON-LISP"], nick_names=['CL-USER'])

# Home for registry entries that are not among the 978 ANSI CL symbols
# (plan.md Finding A / M1 step 1): implementation-internal helpers must not be
# exported from - or even interned into - COMMON-LISP, or they pollute the
# namespace every real CL library relies on being clean.
FCLPY_INTERNAL_PACKAGE = Package("FCLPY-INTERNAL", use_packages=["COMMON-LISP"])

def _register_bootstrap_packages():
    import fclpy.state as _state
    if not hasattr(_state, 'packages') or _state.packages is None:
        _state.packages = {}
    _state.packages.setdefault("FCLPY-INTERNAL", FCLPY_INTERNAL_PACKAGE)

_register_bootstrap_packages()




def make_package(name, use_packages=None, nick_names=None):
    """Create a new package.
    
    Args:
        name: Package name
        use_packages: List of package names to inherit symbols from
        nick_names: List of package nicknames
        
    Returns:
        Package object
    """
    import fclpy.state as state
    
    # Normalize name to uppercase and strip leading colon (for keywords)
    name_upper = name.upper() if isinstance(name, str) else str(name).upper()
    if name_upper.startswith(':'):
        name_upper = name_upper[1:]
    
    # Check if package already exists
    existing = find_package(name_upper)
    if existing is not None:
        return existing
    
    # Create new package
    pkg = Package(name_upper, use_packages=use_packages, nick_names=nick_names)
    
    # Register in state.packages
    if not hasattr(state, 'packages'):
        state.packages = {}
    state.packages[name_upper] = pkg
    
    return pkg


def find_package(name):
    """Find a package by name or nickname.
    
    Args:
        name: Package name or nickname
        
    Returns:
        Package object or None if not found
    """
    import fclpy.state as state
    
    # Normalize to uppercase and strip leading colon (for keywords)
    name_upper = name.upper() if isinstance(name, str) else str(name).upper()
    if name_upper.startswith(':'):
        name_upper = name_upper[1:]
    
    # Check built-in packages first
    if name_upper == "KEYWORD":
        return KEYWORD_PACKAGE
    if name_upper == "COMMON-LISP" or name_upper == "CL":
        return COMMON_LISP_PACKAGE
    if name_upper == "COMMON-LISP-USER" or name_upper == "CL-USER":
        return COMMON_LISP_USER_PACKAGE
    
    # Check dynamically created packages
    if hasattr(state, 'packages') and state.packages:
        # Check by exact name
        if name_upper in state.packages:
            return state.packages[name_upper]
        # Check by nickname
        for pkg_name, pkg in state.packages.items():
            if hasattr(pkg, 'nick_names') and name_upper in [n.upper() if isinstance(n, str) else str(n).upper() for n in pkg.nick_names]:
                return pkg
    
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


def intern_keyword(name, exact_case=False):
    """Intern a keyword (interned in KEYWORD package and auto-exported).

    Args:
        name: Keyword name (without leading colon) or lispKeyword object
        exact_case: use `name` exactly as given (a reader that already
            resolved per-character case for a `\\`/`|...|`-escaped keyword
            name, e.g. `:|foo|`, must not have it re-upcased here)

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

    # Normalize to uppercase (Common Lisp keyword convention), unless the
    # caller already resolved the correct case itself.
    if not exact_case:
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
    """CLHS SIMPLE-CONDITION: the condition type that owns the FORMAT-CONTROL
    and FORMAT-ARGUMENTS slots, and the type SIGNAL builds by default from a
    format-control datum.

    SIMPLE-ERROR and SIMPLE-WARNING get these slots by inheriting from this
    class (CLHS Figure 9-1 lists both as (<parent> SIMPLE-CONDITION)), which is
    why the initializer lives here rather than being repeated in each of them
    as it previously was.
    """

    def __init__(self, format_control="", format_arguments=None, message="", **kwargs):
        # format_control may be a function (CLHS format-control designator,
        # e.g. FORMATTER's result) rather than a string; __str__ must always
        # return a plain str, so only borrow it as the message when it is
        # string-like, and coerce a LispString (a distinct class with no str
        # base -- see plan.md Finding I) to str -- storing the LispString
        # object itself as `message` would make str(condition) raise
        # "__str__ returned non-string (type LispString)" the moment
        # anything printed or matched the condition. A callable format
        # control leaves message empty rather than crashing the same way.
        if not message and format_control and isinstance(format_control, (str, LispString)):
            message = str(format_control)
        super().__init__(message, **kwargs)
        self._slots['format-control'] = format_control
        self._slots['format-arguments'] = format_arguments or []


class SeriousCondition(Condition):
    """CLHS Figure 9-1: SERIOUS-CONDITION, a direct subtype of CONDITION and
    the supertype of both ERROR and STORAGE-CONDITION.

    "Serious" is the property SIGNAL keys off: a serious condition that no
    handler handles enters the debugger, while a non-serious one makes SIGNAL
    simply return NIL. It also exists so (typep c 'serious-condition) and a
    (SERIOUS-CONDITION (C) ...) handler clause work at all -- before this class
    existed the name resolved to no Python class and every such test silently
    took the "not a subtype" branch.
    """
    pass


class StorageCondition(SeriousCondition):
    """CLHS STORAGE-CONDITION: a serious condition, but deliberately *not* an
    ERROR, so an (ERROR (C) ...) handler must not catch it."""
    pass


class Warning(Condition):
    """Base class for warning conditions."""
    pass


class StyleWarning(Warning):
    """CLHS STYLE-WARNING: a WARNING subtype for stylistic problems.

    The ANSI suite's own RT harness binds a STYLE-WARNING handler around every
    test it runs (`rt.lsp`'s do-entry), so this type name is consulted 22036
    times per full suite run.
    """
    pass


class SimpleWarning(Warning, SimpleCondition):
    """Simple warning condition with format control and arguments.

    This is used for warnings created with the SIMPLE-WARNING type specifier,
    and by WARN when its datum is a format-control string.

    CLHS Figure 9-1 defines simple-warning's superclass list as (WARNING
    SIMPLE-CONDITION) -- true multiple inheritance, not just WARNING. Every
    accessor and TYPEP/HANDLER-CASE clause keyed on SIMPLE-CONDITION (the ANSI
    suite's own FROB-SIMPLE-CONDITION helper, for one) depends on that second
    parent; without it (typep <simple-warning> 'simple-condition) is NIL.
    The FORMAT-CONTROL/FORMAT-ARGUMENTS initializer comes from SimpleCondition.
    """
    pass


class Error(SeriousCondition, BaseException):
    """Base class for error conditions.

    CLHS Figure 9-1 makes ERROR a subtype of SERIOUS-CONDITION, not a direct
    subtype of CONDITION; inheriting through SeriousCondition is what makes
    (typep <any-error> 'serious-condition) true, as ANSI requires.
    """
    pass


class TypeError(Error):
    """Condition raised when an argument has an unexpected type."""
    def __init__(self, datum=None, expected_type=None, message="", **kwargs):
        if not message and datum is not None:
            message = f"Type error: expected {expected_type}, got {datum}"
        super().__init__(message, **kwargs)
        self._slots['datum'] = datum
        self._slots['expected-type'] = expected_type


class SimpleTypeError(TypeError, SimpleCondition):
    """CLHS Figure 9-1: SIMPLE-TYPE-ERROR is (TYPE-ERROR SIMPLE-CONDITION), so
    it carries both the DATUM/EXPECTED-TYPE slots and the FORMAT-CONTROL/
    FORMAT-ARGUMENTS pair."""
    def __init__(self, datum=None, expected_type=None, format_control="",
                 format_arguments=None, message="", **kwargs):
        if not message and format_control and isinstance(format_control, (str, LispString)):
            message = str(format_control)
        super().__init__(datum=datum, expected_type=expected_type, message=message, **kwargs)
        self._slots['format-control'] = format_control
        self._slots['format-arguments'] = format_arguments or []


class CellError(Error):
    """CLHS CELL-ERROR: the supertype of UNBOUND-VARIABLE, UNDEFINED-FUNCTION
    and UNBOUND-SLOT, carrying the NAME slot they all share (CELL-ERROR-NAME).
    """
    def __init__(self, name=None, message="", **kwargs):
        if not message and name is not None:
            message = f"Cell error: {name}"
        super().__init__(message, **kwargs)
        if name is not None:
            self._slots['name'] = name


class PackageError(Error):
    """CLHS PACKAGE-ERROR, with the PACKAGE-ERROR-PACKAGE slot."""
    def __init__(self, package=None, message="", **kwargs):
        if not message and package is not None:
            message = f"Package error: {package}"
        super().__init__(message, **kwargs)
        if package is not None:
            self._slots['package'] = package


class ParseError(Error):
    """CLHS PARSE-ERROR: a parsing failure; supertype of READER-ERROR."""
    pass


class PrintNotReadable(Error):
    """CLHS PRINT-NOT-READABLE, with the PRINT-NOT-READABLE-OBJECT slot."""
    def __init__(self, object=None, message="", **kwargs):
        if not message:
            message = "Object cannot be printed readably"
        super().__init__(message, **kwargs)
        self._slots['object'] = object


class ProgramError(Error):
    """Condition for program errors (control flow issues)."""
    pass


class ControlError(Error):
    """Condition for control flow errors."""
    pass


class FileError(Error):
    """CLHS FILE-ERROR: an error involving a file, carrying the PATHNAME slot
    that FILE-ERROR-PATHNAME reads.

    The slot is part of the type, not an optional extra: CLHS says the
    pathname "is initialized by the :PATHNAME initialization argument", so
    every operator that signals a FILE-ERROR must say *which* file it was
    about. Storing it here (rather than letting each raise site invent an
    attribute) is what makes `signal_file_error` in evaluation_conditions.py
    the one place a file operation reports failure.
    """

    def __init__(self, pathname=None, message="", **kwargs):
        if not message:
            message = ("File error" if pathname is None
                       else f"File error on {pathname}")
        super().__init__(message, **kwargs)
        self._slots['pathname'] = pathname


class StreamError(Error):
    """Condition for stream operation errors."""
    pass


class EndOfFile(StreamError):
    """Condition raised when EOF is reached unexpectedly."""
    def __init__(self, stream=None, message="End of file", **kwargs):
        super().__init__(message, **kwargs)
        if stream is not None:
            self._slots['stream'] = stream


class ReaderError(ParseError, StreamError):
    """CLHS Figure 9-1: READER-ERROR is (PARSE-ERROR STREAM-ERROR) -- true
    multiple inheritance, so an (ERROR (C) ...), a (PARSE-ERROR (C) ...) and a
    (STREAM-ERROR (C) ...) clause must all match it."""
    def __init__(self, stream=None, message="Reader error", **kwargs):
        super().__init__(message, **kwargs)
        if stream is not None:
            self._slots['stream'] = stream


class UndefinedFunction(CellError):
    """Condition for undefined-function errors.

    CLHS Figure 9-1: a subtype of CELL-ERROR (which is where the NAME slot and
    CELL-ERROR-NAME come from), not a direct subtype of ERROR.
    """
    def __init__(self, name=None, message=None, **kwargs):
        if message is None:
            message = f"Undefined function: {name}" if name is not None else "Undefined function"
        super().__init__(name=name, message=message, **kwargs)


class UnboundVariable(CellError):
    """Condition for unbound-variable errors (CLHS: a CELL-ERROR subtype)."""
    def __init__(self, name=None, message=None, **kwargs):
        if message is None:
            message = f"Unbound variable: {name}" if name is not None else "Unbound variable"
        super().__init__(name=name, message=message, **kwargs)


class UnboundSlot(CellError):
    """CLHS UNBOUND-SLOT: a CELL-ERROR subtype with the additional
    UNBOUND-SLOT-INSTANCE slot naming the object whose slot was unbound."""
    def __init__(self, name=None, instance=None, message=None, **kwargs):
        if message is None:
            message = f"Unbound slot: {name}" if name is not None else "Unbound slot"
        super().__init__(name=name, message=message, **kwargs)
        self._slots['instance'] = instance


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


class FloatingPointInexact(ArithmeticError):
    """CLHS FLOATING-POINT-INEXACT, completing Figure 9-1's four
    FLOATING-POINT-* ARITHMETIC-ERROR subtypes."""
    pass


class SimpleError(Error, SimpleCondition):
    """Simple error condition with format control and arguments.

    This is used for errors created with SIMPLE-ERROR type specifier.

    CLHS Figure 9-1 defines simple-error's superclass list as (ERROR
    SIMPLE-CONDITION) -- true multiple inheritance, not just ERROR. Every
    accessor and TYPEP/HANDLER-CASE clause keyed on SIMPLE-CONDITION (the ANSI
    suite's own FROB-SIMPLE-CONDITION helper, for one) depends on that second
    parent; without it (typep <simple-error> 'simple-condition) is NIL.
    The FORMAT-CONTROL/FORMAT-ARGUMENTS initializer comes from SimpleCondition.
    """
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
    
    s = s.upper()
    for pattern, replacement in py_str_map:
        s = s.replace(pattern, replacement)
    return intern_symbol(s, COMMON_LISP_USER_PACKAGE)


class Restart(lispT):
    """A CLHS 9.1 restart: a named dynamic recovery point.

    `function` is what INVOKE-RESTART funcalls -- for a RESTART-BIND restart
    it is exactly the user's function (so calling it performs the recovery
    directly, with no unwinding of its own); for a RESTART-CASE restart it is
    a closure that performs RESTART-CASE's implicit non-local exit back to
    the establishing form. INVOKE-RESTART treats both the same way, through
    `evaluation_core.funcall`, which is what makes a wrong argument count
    signal PROGRAM-ERROR (RESTART-BIND.ERROR.*) for either kind rather than
    needing two invocation paths.

    `associated_conditions` implements CLHS 9.1's condition-restart
    association: empty means "visible regardless of which condition, if any,
    is asked about"; once WITH-CONDITION-RESTARTS (used automatically by
    RESTART-CASE when its protected form is literally a call to SIGNAL/
    ERROR/CERROR/WARN) adds a condition to this list, the restart becomes
    invisible to COMPUTE-RESTARTS/FIND-RESTART queries naming any *other*
    condition. `test_function` is the orthogonal, user-supplied `:test`/
    `:test-function` filter, consulted independently of association.
    """

    def __init__(self, name, function, report_function=None,
                 interactive_function=None, test_function=None):
        """Initialize a Restart.

        Args:
            name: restart name -- a LispSymbol, or NIL for an anonymous restart
            function: callable invoked (via funcall) by INVOKE-RESTART
            report_function: optional callable of one argument (a stream)
            interactive_function: optional callable of no arguments, called
                by INVOKE-RESTART-INTERACTIVELY to produce the argument list
            test_function: optional callable of one argument (a condition,
                or NIL) deciding whether this restart applies; defaults to
                "always applies"
        """
        if isinstance(name, str):
            self.name = LispSymbol(name)
        else:
            self.name = name
        self.function = function
        self.report_function = report_function
        self.interactive_function = interactive_function
        self.test_function = test_function
        self.associated_conditions = []

    def name_matches(self, identifier):
        """CLHS 9.1's restart-name designator match: a symbol matches by
        `string=`-equivalent name (case already normalized by the reader);
        this restart is never matched by name if it is anonymous (NIL)."""
        if self.name is NIL or not isinstance(self.name, LispSymbol):
            return False
        target = identifier.name if isinstance(identifier, LispSymbol) else str(identifier)
        return self.name.name == target

    def applies_to(self, condition):
        """Whether this restart is a candidate for a COMPUTE-RESTARTS/
        FIND-RESTART/INVOKE-RESTART query naming `condition` (NIL/None for
        "no condition given" -- CLHS: no filtering at all in that case)."""
        real_condition = None if condition in (None, NIL) else condition
        if real_condition is not None and self.associated_conditions:
            if not any(c is real_condition for c in self.associated_conditions):
                return False
        if self.test_function is not None:
            from fclpy.lispfunc.evaluation_core import funcall as _funcall
            result = _funcall(self.test_function, real_condition if real_condition is not None else NIL)
            if result is NIL or result is None or result is False:
                return False
        return True

    def __repr__(self):
        name = self.name.name if isinstance(self.name, LispSymbol) else 'NIL'
        return f"#<RESTART {name}>"


__all__ = [
    # Environment
    'Environment',
    # Package system
    'Package', 'KEYWORD_PACKAGE', 'COMMON_LISP_PACKAGE', 'COMMON_LISP_USER_PACKAGE',
    'FCLPY_INTERNAL_PACKAGE',
    'make_package', 'find_package', 'intern_symbol', 'intern_keyword',
    # Conditions (ANSI condition system -- CLHS Figure 9-1)
    'Condition', 'SimpleCondition', 'SeriousCondition', 'StorageCondition',
    'SimpleError', 'Warning', 'StyleWarning', 'SimpleWarning', 'Error',
    'TypeError', 'SimpleTypeError', 'ProgramError', 'ControlError', 'FileError',
    'StreamError', 'ReaderError', 'ParseError', 'PrintNotReadable',
    'CellError', 'PackageError',
    'EndOfFile', 'ArithmeticError', 'DivisionByZero',
    'UndefinedFunction', 'UnboundVariable', 'UnboundSlot',
    'FloatingPointInvalidOperation', 'FloatingPointOverflow', 'FloatingPointUnderflow',
    'FloatingPointInexact',
    # Restarts
    'Restart',
    # Utilities
    'resolve_environment',
    'py_str_to_sym'
]
