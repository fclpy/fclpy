"""Symbol and package management operations."""

import time
import fclpy.lisptype as lisptype
import fclpy.state as state
from fclpy.lispfunc import registry as _registry


# --- Symbol operations ---
@_registry.cl_function('SYMBOL-NAME')
def symbol_name(symbol):
    """Get the name of a symbol."""
    if hasattr(symbol, 'name'):
        return symbol.name
    return str(symbol)


@_registry.cl_function('SYMBOL-PACKAGE')
def symbol_package(symbol):
    """Get the package a symbol belongs to."""
    return getattr(symbol, 'package', None)


@_registry.cl_function('SYMBOL-VALUE')
def symbol_value(symbol):
    """Get the value bound to a symbol."""
    return getattr(symbol, 'value', None)


@_registry.cl_function('MAKE-SYMBOL')
def make_symbol(name):
    """Create a new uninterned symbol."""
    return lisptype.LispSymbol(str(name))


@_registry.cl_function('COPY-SYMBOL')
def copy_symbol(symbol, copy_props=None):
    """Copy a symbol, optionally copying properties."""
    return make_symbol(symbol_name(symbol))


# --- Gensym (unique symbol generation) ---
_gensym_counter = 0

def gensym(prefix="G"):
    """Generate unique symbol with prefix."""
    global _gensym_counter
    _gensym_counter += 1
    return lisptype.LispSymbol(f"{prefix}{_gensym_counter}")


# --- Package operations ---
@_registry.cl_function('*PACKAGE*')
def get_current_package():
    """Get the value of *PACKAGE* (current package)."""
    return getattr(state, 'current_package', None) or lisptype.COMMON_LISP_USER_PACKAGE


@_registry.cl_function('IN-PACKAGE')
def in_package(name):
    """Set current package and return it.
    
    This updates both state.current_package and the *PACKAGE* variable
    in the current environment for proper dynamic binding behavior.
    """
    if isinstance(name, lisptype.Package):
        pkg = name
    else:
        # Handle keywords  
        if isinstance(name, lisptype.lispKeyword):
            pkg_name = name.name
        elif isinstance(name, lisptype.LispSymbol):
            pkg_name = name.name
            if pkg_name.startswith(':'):
                pkg_name = pkg_name[1:]
        else:
            pkg_name = str(name)
            if pkg_name.startswith(':'):
                pkg_name = pkg_name[1:]
        
        pkg = lisptype.find_package(pkg_name)
        if pkg is None:
            # Create package - by default new packages USE COMMON-LISP
            pkg = lisptype.make_package(pkg_name)
            # Add COMMON-LISP to use list by default
            cl_pkg = lisptype.COMMON_LISP_PACKAGE
            if cl_pkg and cl_pkg not in pkg.use_packages:
                pkg.use_packages.append(cl_pkg)
    
    # Update state.current_package
    state.current_package = pkg
    
    # Also update *PACKAGE* variable in the current environment if it exists
    env = getattr(state, 'current_environment', None)
    if env is not None:
        package_sym = lisptype.COMMON_LISP_PACKAGE.intern_symbol('*PACKAGE*')
        # Set the variable directly
        env.set_variable(package_sym, pkg)
    
    return pkg


@_registry.cl_function('IMPORT')
def import_symbol(symbols, package=None):
    """Import symbols into a package."""
    import fclpy.state as state  # Re-import to ensure we have the same state module
    
    # Convert symbols to a list for iteration
    if isinstance(symbols, lisptype.lispCons):
        # Convert lispCons to Python list
        sym_list = []
        cur = symbols
        while isinstance(cur, lisptype.lispCons):
            sym_list.append(cur.car)
            cur = cur.cdr
        symbols = sym_list
    elif not isinstance(symbols, (list, tuple)):
        symbols = [symbols]
    
    # Determine the target package
    if package is None:
        pkg = lisptype.COMMON_LISP_USER_PACKAGE
    elif isinstance(package, lisptype.Package):
        pkg = package
    else:
        # Handle keywords and symbols - extract the name
        if isinstance(package, lisptype.lispKeyword):
            pkg_name = package.name  # Keyword names don't include the colon
        elif isinstance(package, lisptype.LispSymbol):
            pkg_name = package.name
            if pkg_name.startswith(':'):
                pkg_name = pkg_name[1:]  # Remove leading colon if present
        else:
            pkg_name = str(package)
            if pkg_name.startswith(':'):
                pkg_name = pkg_name[1:]  # Remove leading colon
        pkg = lisptype.find_package(pkg_name)
    
    if pkg is None:
        raise lisptype.LispNotImplementedError(f"IMPORT: unknown package '{pkg_name}'")
    for s in symbols:
        name = s.name if hasattr(s, 'name') else str(s)
        pkg.intern_symbol(name, external=True)
    return lisptype.T


@_registry.cl_function('INTERN')
def intern(name, package=None):
    """Intern a symbol in a package or create new interned symbol."""
    if not isinstance(name, str):
        name = str(name)
    if package is None:
        package = getattr(state, 'current_package', None) or lisptype.COMMON_LISP_USER_PACKAGE
    if isinstance(package, lisptype.Package):
        return package.intern_symbol(name)
    pkg = lisptype.find_package(str(package))
    if pkg is None:
        pkg = lisptype.make_package(str(package))
    return pkg.intern_symbol(name)


@_registry.cl_function('FIND-SYMBOL')
def find_symbol(name, package=None):
    """Find a symbol in a package.
    
    Returns two values:
    1. The symbol (or NIL if not found)
    2. Status: :INTERNAL, :EXTERNAL, :INHERITED, or NIL if not found
    """
    pkg = package if isinstance(package, lisptype.Package) else lisptype.find_package(str(package)) if package else getattr(state, 'current_package', None)
    if pkg is None:
        return lisptype.MultipleValues(lisptype.NIL, lisptype.NIL)
    
    symbol, status = pkg.find_symbol(name)
    if symbol is None:
        return lisptype.MultipleValues(lisptype.NIL, lisptype.NIL)
    
    # Convert status string to keyword (use intern_keyword for proper interning)
    if status:
        status_keyword = lisptype.intern_keyword(status[1:])  # Remove leading ':'
    else:
        status_keyword = lisptype.NIL
    
    return lisptype.MultipleValues(symbol, status_keyword)


@_registry.cl_function('FIND-PACKAGE')
def find_package(name):
    """Find a package by name."""
    return lisptype.find_package(str(name))


@_registry.cl_function('FIND-ALL-SYMBOLS')
def find_all_symbols(name):
    """Find all symbols with given name across all packages."""
    results = []
    for pkg in list({id(p): p for p in state.packages.values()}.values()):
        sym, status = pkg.find_symbol(name)
        if sym is not None:
            results.append(sym)
    return results


@_registry.cl_function('EXPORT')
def export(symbols, package=None):
    """Export symbols from a package.
    
    Makes symbols accessible to other packages that USE this package.
    """
    # Handle lispCons (Lisp list) by converting to Python list
    if isinstance(symbols, lisptype.lispCons):
        symbols = list(symbols)  # lispCons is iterable
    elif not isinstance(symbols, (list, tuple)):
        symbols = [symbols]
    pkg = package if isinstance(package, lisptype.Package) else lisptype.find_package(str(package)) if package else getattr(state, 'current_package', None)
    if pkg is None:
        pkg = lisptype.COMMON_LISP_USER_PACKAGE
    for s in symbols:
        sym_name = s.name if hasattr(s, 'name') else str(s)
        # Intern the symbol if not already present, then export it
        sym = pkg.intern_symbol(sym_name)
        # Add to package's external_symbols set
        pkg.export_symbol(sym_name)
    return lisptype.T


@_registry.cl_function('GENTEMP')
def gentemp(prefix='T', package=None):
    """Generate temporary interned symbol."""
    return intern(f"{prefix}{int(time.time()*1000)}", package)


@_registry.cl_function('APROPOS')
def apropos(string, package=None):
    """Find symbols matching string."""
    raise lisptype.LispNotImplementedError("APROPOS")


@_registry.cl_function('APROPOS-LIST')
def apropos_list(string, package=None):
    """List symbols matching string."""
    raise lisptype.LispNotImplementedError("APROPOS-LIST")


__all__ = [
    'symbol_name',
    'symbol_package',
    'symbol_value',
    'make_symbol',
    'copy_symbol',
    'gensym',
    'in_package',
    'import_symbol',
    'intern',
    'find_symbol',
    'find_package',
    'find_all_symbols',
    'export',
    'gentemp',
    'apropos',
    'apropos_list',
]
