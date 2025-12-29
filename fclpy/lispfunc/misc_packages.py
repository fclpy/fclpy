"""Package operations and macro expansion."""

import fclpy.lisptype as lisptype
import fclpy.state as state
from fclpy.lispfunc import registry as _registry


def _parse_keyword_args(args):
    """Parse Lisp-style keyword arguments into Python dict.
    
    Converts (value1 :key1 val1 :key2 val2 ...) into
    {'key1': val1, 'key2': val2, ...}
    """
    result = {}
    positional = []
    i = 0
    while i < len(args):
        arg = args[i]
        # Check if this is a keyword argument
        is_keyword = False
        key = None
        if isinstance(arg, lisptype.lispKeyword):
            is_keyword = True
            key = arg.name.lower()
        elif isinstance(arg, lisptype.LispSymbol) and arg.name.startswith(':'):
            is_keyword = True
            key = arg.name[1:].lower()
        
        if is_keyword and key and i + 1 < len(args):
            result[key] = args[i + 1]
            i += 2
        else:
            positional.append(arg)
            i += 1
    
    return positional, result


# --- Package operations (advanced) ---
@_registry.cl_function('MAKE-PACKAGE')
def make_package(*args):
    """Create a new package.
    
    Handles both:
    - (make-package name)
    - (make-package name :nicknames list :use list)
    """
    if not args:
        raise ValueError("MAKE-PACKAGE requires a name")
    
    # First argument is always the package name (positional)
    name_arg = args[0]
    
    # Remaining arguments are keyword args
    remaining_args = args[1:] if len(args) > 1 else []
    _, kwargs = _parse_keyword_args(remaining_args)
    if isinstance(name_arg, lisptype.lispKeyword):
        name = name_arg.name  # Keywords store name without colon
    elif isinstance(name_arg, lisptype.LispSymbol):
        name = name_arg.name
        if name.startswith(':'):
            name = name[1:]  # Remove leading colon
    else:
        name = str(name_arg)
        if name.startswith(':'):
            name = name[1:]  # Remove leading colon
    
    nicknames = kwargs.get('nicknames', None)
    use = kwargs.get('use', None)
    
    # Convert nicknames to Python list if it's a Lisp list
    if nicknames is not None and nicknames != lisptype.NIL:
        if isinstance(nicknames, lisptype.lispCons):
            nick_list = []
            cur = nicknames
            while cur is not None and cur != lisptype.NIL and isinstance(cur, lisptype.lispCons):
                item = cur.car
                if isinstance(item, lisptype.lispKeyword):
                    nick_list.append(item.name)
                elif isinstance(item, lisptype.LispSymbol):
                    n = item.name
                    if n.startswith(':'):
                        n = n[1:]
                    nick_list.append(n)
                else:
                    nick_list.append(str(item) if item else None)
                cur = cur.cdr
            nicknames = [n for n in nick_list if n]
        elif isinstance(nicknames, (list, tuple)):
            nicknames = [str(n) for n in nicknames if n]
    
    # Convert use to Python list if it's a Lisp list
    use_list = []
    if use is not None and use != lisptype.NIL:
        if isinstance(use, lisptype.lispCons):
            cur = use
            while cur is not None and cur != lisptype.NIL and isinstance(cur, lisptype.lispCons):
                item = cur.car
                if isinstance(item, lisptype.lispKeyword):
                    use_list.append(item.name)
                elif isinstance(item, lisptype.LispSymbol):
                    n = item.name
                    if n.startswith(':'):
                        n = n[1:]
                    use_list.append(n)
                elif isinstance(item, lisptype.Package):
                    use_list.append(item.name)
                else:
                    use_list.append(str(item) if item else None)
                cur = cur.cdr
            use_list = [n for n in use_list if n]
        elif isinstance(use, (list, tuple)):
            use_list = [str(n) for n in use if n]
    
    # Create the package
    pkg = lisptype.make_package(name)
    
    # Store nicknames if provided
    if nicknames:
        pkg.nick_names = nicknames  # Use nick_names to match Package class
    
    # Add USE'd packages
    for use_pkg_name in use_list:
        use_pkg = lisptype.find_package(use_pkg_name)
        if use_pkg and use_pkg not in pkg.use_packages:
            pkg.use_packages.append(use_pkg)
    
    return pkg


@_registry.cl_function('PACKAGE-NAME')
def package_name(package):
    """Get package name."""
    return package.name if isinstance(package, lisptype.Package) else None


@_registry.cl_function('PACKAGE-NICKNAMES')
def package_nicknames(package):
    """Get package nicknames."""
    return getattr(package, 'nicknames', [])


@_registry.cl_function('RENAME-PACKAGE')
def rename_package(package, new_name, new_nicknames=None):
    """Rename a package."""
    if isinstance(package, lisptype.Package):
        package.name = str(new_name)
    return package


@_registry.cl_function('PACKAGE-USE-LIST')
def package_use_list(package):
    """Get packages this package uses."""
    pkg = package if isinstance(package, lisptype.Package) else lisptype.find_package(str(package)) if package else getattr(state, 'current_package', None)
    if pkg is None:
        return []
    return list(pkg.use_list)


@_registry.cl_function('PACKAGE-USED-BY-LIST')
def package_used_by_list(package):
    """Get packages that use this package."""
    pkg = package if isinstance(package, lisptype.Package) else lisptype.find_package(str(package)) if package else getattr(state, 'current_package', None)
    if pkg is None:
        return []
    used_by = []
    for p in list({id(p): p for p in state.packages.values()}.values()):
        if pkg in getattr(p, 'use_list', []):
            used_by.append(p)
    return used_by


@_registry.cl_function('PACKAGE-SHADOWING-SYMBOLS')
def package_shadowing_symbols(package):
    """Get shadowing symbols in package."""
    pkg = package if isinstance(package, lisptype.Package) else lisptype.find_package(str(package)) if package else getattr(state, 'current_package', None)
    if pkg is None:
        return []
    syms = []
    for name in getattr(pkg, 'shadowing_symbols', set()):
        s = pkg.symbols.get(name)
        if s is not None:
            syms.append(s)
    return syms


@_registry.cl_function('PACKAGE-EXTERNAL-SYMBOLS')
def package_external_symbols(package):
    """Get external symbols in package.
    
    Returns a list of symbols that are exported from the package.
    """
    pkg = package if isinstance(package, lisptype.Package) else lisptype.find_package(str(package)) if package else getattr(state, 'current_package', None)
    if pkg is None:
        return []
    syms = []
    external_names = getattr(pkg, 'external_symbols', set())
    for name in external_names:
        s = pkg.symbols.get(name)
        if s is not None:
            syms.append(s)
    return syms


@_registry.cl_function('PACKAGE-INTERNAL-SYMBOLS')
def package_internal_symbols(package):
    """Get internal (non-exported) symbols in package.
    
    Returns a list of symbols that are in the package but not exported.
    """
    pkg = package if isinstance(package, lisptype.Package) else lisptype.find_package(str(package)) if package else getattr(state, 'current_package', None)
    if pkg is None:
        return []
    syms = []
    external_names = getattr(pkg, 'external_symbols', set())
    for name, sym in pkg.symbols.items():
        if name not in external_names:
            syms.append(sym)
    return syms


@_registry.cl_function('LIST-ALL-PACKAGES')
def list_all_packages():
    """List all known packages."""
    unique = {id(p): p for p in state.packages.values()}
    return list(unique.values())


@_registry.cl_function('UNINTERN')
def unintern(symbol, package=None):
    """Remove symbol from package."""
    if not isinstance(symbol, str) and hasattr(symbol, 'name'):
        name = symbol.name
    else:
        name = str(symbol)
    pkg = package if isinstance(package, lisptype.Package) else lisptype.find_package(str(package)) if package else getattr(state, 'current_package', None)
    if pkg is None:
        return lisptype.NIL
    name = name.upper()
    if name in pkg.symbols:
        del pkg.symbols[name]
        pkg.external_symbols.discard(name)
        pkg.shadowing_symbols.discard(name)
        return lisptype.T
    return lisptype.NIL


@_registry.cl_function('UNEXPORT')
def unexport(symbols, package=None):
    """Unexport symbols from package."""
    if not isinstance(symbols, (list, tuple)):
        symbols = [symbols]
    pkg = package if isinstance(package, lisptype.Package) else lisptype.find_package(str(package)) if package else getattr(state, 'current_package', None)
    if pkg is None:
        return lisptype.NIL
    for s in symbols:
        name = s.name if hasattr(s, 'name') else str(s)
        pkg.external_symbols.discard(name.upper())
    return lisptype.T


@_registry.cl_function('SHADOWING-IMPORT')
def shadowing_import(symbols, package=None):
    """Shadowing import symbols."""
    return lisptype.T


@_registry.cl_function('SHADOW')
def shadow(symbols, package=None):
    """Create shadowing symbols in package."""
    return lisptype.T


@_registry.cl_function('USE-PACKAGE')
def use_package(packages, package=None):
    """Install packages into use-list."""
    # Handle lispCons (Lisp list)
    if isinstance(packages, lisptype.lispCons):
        packages = list(packages)
    elif not isinstance(packages, (list, tuple)):
        packages = [packages]
    target = package if isinstance(package, lisptype.Package) else lisptype.find_package(str(package)) if package else getattr(state, 'current_package', None)
    if target is None:
        return lisptype.NIL
    for p in packages:
        pkgobj = p if isinstance(p, lisptype.Package) else lisptype.find_package(str(p))
        if pkgobj is None:
            pkgobj = lisptype.make_package(str(p))
        if pkgobj not in target.use_packages:
            target.use_packages.append(pkgobj)
    return lisptype.T


@_registry.cl_function('UNUSE-PACKAGE')
def unuse_package(packages, package=None):
    """Remove packages from use-list."""
    # Handle lispCons (Lisp list)
    if isinstance(packages, lisptype.lispCons):
        packages = list(packages)
    elif not isinstance(packages, (list, tuple)):
        packages = [packages]
    target = package if isinstance(package, lisptype.Package) else lisptype.find_package(str(package)) if package else getattr(state, 'current_package', None)
    if target is None:
        return lisptype.NIL
    for p in packages:
        pkgobj = p if isinstance(p, lisptype.Package) else lisptype.find_package(str(p))
        if pkgobj in target.use_packages:
            target.use_packages.remove(pkgobj)
    return lisptype.T


# --- Macro expansion ---
@_registry.cl_function('MACROEXPAND')
def macroexpand(form, environment=None):
    """Expand macros fully."""
    return form, lisptype.NIL


@_registry.cl_function('MACROEXPAND-1')
def macroexpand_1(form, environment=None):
    """Expand macros once."""
    return form, lisptype.NIL


__all__ = [
    'make_package',
    'package_name',
    'package_nicknames',
    'rename_package',
    'package_use_list',
    'package_used_by_list',
    'package_shadowing_symbols',
    'list_all_packages',
    'unintern',
    'unexport',
    'shadowing_import',
    'shadow',
    'use_package',
    'unuse_package',
    'macroexpand',
    'macroexpand_1',
]
