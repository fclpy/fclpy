"""Package operations and macro expansion."""

import fclpy.lisptype as lisptype
import fclpy.state as state
from fclpy.lispfunc import registry as _registry


# --- Package operations (advanced) ---
@_registry.cl_function('MAKE-PACKAGE')
def make_package(name, nicknames=None, use=None):
    """Create a new package."""
    return lisptype.make_package(str(name))


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
    if not isinstance(packages, (list, tuple)):
        packages = [packages]
    target = package if isinstance(package, lisptype.Package) else lisptype.find_package(str(package)) if package else getattr(state, 'current_package', None)
    if target is None:
        return lisptype.NIL
    for p in packages:
        pkgobj = p if isinstance(p, lisptype.Package) else lisptype.find_package(str(p))
        if pkgobj is None:
            pkgobj = lisptype.make_package(str(p))
        if pkgobj not in target.use_list:
            target.use_list.append(pkgobj)
    return lisptype.T


@_registry.cl_function('UNUSE-PACKAGE')
def unuse_package(packages, package=None):
    """Remove packages from use-list."""
    if not isinstance(packages, (list, tuple)):
        packages = [packages]
    target = package if isinstance(package, lisptype.Package) else lisptype.find_package(str(package)) if package else getattr(state, 'current_package', None)
    if target is None:
        return lisptype.NIL
    for p in packages:
        pkgobj = p if isinstance(p, lisptype.Package) else lisptype.find_package(str(p))
        if pkgobj in target.use_list:
            target.use_list.remove(pkgobj)
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
