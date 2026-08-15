"""Package operations and macro expansion."""

import fclpy.lisptype as lisptype
import fclpy.state as state
from fclpy.lispfunc import registry as _registry
import logging

logger = logging.getLogger(__name__)

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


def coerce_to_package(designator, default=None):
    """Resolve a package designator to a Package (CLHS 11.1.1.1).

    A designator is a package, a string, a symbol, or a character; NIL (or an
    omitted argument) means `default`, which itself defaults to the value of
    `*PACKAGE*`. An unknown name is a PACKAGE-ERROR, not None: returning None
    made every caller invent its own "and if it wasn't found" branch, and the
    ones that swallowed it (`except Exception: pkg = None`) turned a misspelled
    package name into an empty iteration instead of an error.
    """
    if isinstance(designator, lisptype.Package):
        return designator
    if designator is None or designator is lisptype.NIL:
        if default is not None:
            return coerce_to_package(default)
        current = getattr(state, 'current_package', None)
        return current if current is not None else lisptype.COMMON_LISP_USER_PACKAGE
    if isinstance(designator, lisptype.LispSymbol):
        name = designator.name
    elif isinstance(designator, str):
        name = str(designator)
    else:
        name = str(designator)
    pkg = lisptype.find_package(name)
    if pkg is None:
        raise lisptype.LispError(f'Package not found: {name}')
    return pkg


def package_symbols(package, kind):
    """The symbols of `package` that `kind` names, as a Python list.

    `kind` is one of 'symbols' (every symbol *accessible* in the package:
    its own, plus the external symbols of the packages it uses),
    'present-symbols' (its own, whether exported or not) or
    'external-symbols' (the ones it exports) -- the three sets CLHS 6.1.2.1.7
    gives LOOP's for-as-package clause and DO-SYMBOLS / DO-EXTERNAL-SYMBOLS.

    One enumerator for all of them, because the interesting part is a detail
    each open-coded copy got differently: `use_packages` holds package *names*
    as well as `Package` objects (see `Package.intern`), so a copy that reads
    `used.external_symbols` off a string gets the empty set and silently drops
    every inherited symbol. `external_symbols` is likewise a set of names in
    some packages and of symbol objects in others.
    """
    pkg = coerce_to_package(package)

    def externals_of(p):
        result = []
        for item in list(getattr(p, 'external_symbols', ()) or ()):
            sym = item if isinstance(item, lisptype.LispSymbol) else p.symbols.get(item)
            if sym is not None:
                result.append(sym)
        return result

    if kind == 'external-symbols':
        return externals_of(pkg)

    present = list(pkg.symbols.values())
    if kind == 'present-symbols':
        return present
    if kind != 'symbols':
        raise ValueError(f'unknown package symbol set: {kind!r}')

    # Accessible = present + inherited externals, without duplicates. Identity
    # is the right key: an inherited symbol *is* the used package's symbol.
    seen = {id(s) for s in present}
    accessible = list(present)
    for used in list(getattr(pkg, 'use_list', ()) or ()):
        used_pkg = lisptype.find_package(used) if isinstance(used, str) else used
        if used_pkg is None:
            continue
        for sym in externals_of(used_pkg):
            if id(sym) not in seen:
                seen.add(id(sym))
                accessible.append(sym)
    return accessible


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
    # Package class uses `nick_names`; accept either for compatibility
    return getattr(package, 'nick_names', getattr(package, 'nicknames', []))


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
        return lisptype.NIL
    result = list(pkg.use_list)
    return result if result else lisptype.NIL


@_registry.cl_function('PACKAGE-USED-BY-LIST')
def package_used_by_list(package):
    """Get packages that use this package."""
    pkg = package if isinstance(package, lisptype.Package) else lisptype.find_package(str(package)) if package else getattr(state, 'current_package', None)
    if pkg is None:
        return lisptype.NIL
    used_by = []
    for p in list({id(p): p for p in state.packages.values()}.values()):
        if pkg in getattr(p, 'use_list', []):
            used_by.append(p)
    return used_by if used_by else lisptype.NIL


@_registry.cl_function('PACKAGE-SHADOWING-SYMBOLS')
def package_shadowing_symbols(package):
    """Get shadowing symbols in package."""
    pkg = package if isinstance(package, lisptype.Package) else lisptype.find_package(str(package)) if package else getattr(state, 'current_package', None)
    if pkg is None:
        return lisptype.NIL
    syms = []
    for name in getattr(pkg, 'shadowing_symbols', set()):
        s = pkg.symbols.get(name)
        if s is not None:
            syms.append(s)
    return syms if syms else lisptype.NIL


@_registry.cl_function('PACKAGE-EXTERNAL-SYMBOLS')
def package_external_symbols(package):
    """Get external symbols in package.
    
    Returns a list of symbols that are exported from the package.
    """
    pkg = package if isinstance(package, lisptype.Package) else lisptype.find_package(str(package)) if package else getattr(state, 'current_package', None)
    if pkg is None:
        return lisptype.NIL
    syms = []
    external_names = getattr(pkg, 'external_symbols', set())
    for name in external_names:
        s = pkg.symbols.get(name)
        if s is not None:
            syms.append(s)
    return syms if syms else lisptype.NIL


@_registry.cl_function('PACKAGE-INTERNAL-SYMBOLS')
def package_internal_symbols(package):
    """Get internal (non-exported) symbols in package.
    
    Returns a list of symbols that are in the package but not exported.
    """
    pkg = package if isinstance(package, lisptype.Package) else lisptype.find_package(str(package)) if package else getattr(state, 'current_package', None)
    if pkg is None:
        return lisptype.NIL
    syms = []
    external_names = getattr(pkg, 'external_symbols', set())
    for name, sym in pkg.symbols.items():
        if name not in external_names:
            syms.append(sym)
    return syms if syms else lisptype.NIL


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
    # Handle lispCons (Lisp list)
    if isinstance(symbols, lisptype.lispCons):
        symbols = list(symbols)
    elif not isinstance(symbols, (list, tuple)):
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


@_registry.cl_function('DELETE-PACKAGE')
def delete_package(package):
    """Delete a package by instance or name.

    Removes the package from the global package registry and
    from other packages' use-lists. Returns T if deleted, NIL otherwise.
    """
    pkg = package if isinstance(package, lisptype.Package) else lisptype.find_package(str(package))
    if pkg is None:
        return lisptype.NIL

    # Remove any entries in state.packages that point to this package
    keys_to_remove = [k for k, v in list(state.packages.items()) if v is pkg]
    for k in keys_to_remove:
        try:
            del state.packages[k]
        except Exception:
            pass

    # Remove from other packages' use lists (supporting different attribute names)
    for p in list(state.packages.values()):
        try:
            if hasattr(p, 'use_packages') and pkg in getattr(p, 'use_packages'):
                getattr(p, 'use_packages').remove(pkg)
        except Exception:
            pass
        try:
            if hasattr(p, 'use_list') and pkg in getattr(p, 'use_list'):
                getattr(p, 'use_list').remove(pkg)
        except Exception:
            pass

    return lisptype.T


# --- Macro expansion ---
def _direct_macroexpand_1(form, environment):
    """Direct, eval-free macro expansion of form in environment.

    Returns (expanded_form, did_expand).  Unlike eval_macroexpand_1 this
    function never wraps the form in a cons cell, so it is safe to call
    with forms that are themselves quoted lists such as (QUOTE FOO).
    """
    from .evaluation_core import _consp_internal
    from .core import car as _car, cdr as _cdr

    # Only cons cells can be macro call forms
    if not _consp_internal(form):
        return form, False

    operator = _car(form)
    if not isinstance(operator, lisptype.LispSymbol):
        return form, False

    # Need an environment to look up macros
    if environment is None:
        return form, False

    try:
        macro_func = environment.find_func(operator)
    except Exception:
        return form, False

    if not macro_func or not callable(macro_func):
        return form, False
    if not getattr(macro_func, '__is_macro__', False):
        return form, False

    # Collect raw (unevaluated) arguments
    args_list = []
    current = _cdr(form)
    while _consp_internal(current):
        args_list.append(_car(current))
        current = _cdr(current)

    try:
        expects_whole = getattr(macro_func, '__expects_whole__', False)
        expects_env = getattr(macro_func, '__expects_environment__', False)

        call_args = []
        if expects_whole:
            call_args.append(form)
        call_args.extend(args_list)
        if expects_env:
            call_args.append(environment)

        expanded = macro_func(*call_args)
        return expanded, True
    except Exception:
        logger.error(f"[_direct_macroexpand_1] error expanding {operator}", exc_info=True)
        return form, False


@_registry.cl_function('MACROEXPAND')
def macroexpand(form, environment=None):
    """Expand macros fully (multiple passes until stable)."""
    try:
        prev = form
        while True:
            expanded, did_expand = _direct_macroexpand_1(prev, environment)
            if not did_expand:
                return prev
            prev = expanded
    except Exception:
        logger.error(f"[macroexpand] error while expanding, returning original. env_id={id(environment)}\n", exc_info=True)
        return form


@_registry.cl_function('MACROEXPAND-1')
def macroexpand_1(form, environment=None):
    """Expand macros once."""
    try:
        expanded, _did = _direct_macroexpand_1(form, environment)
        return expanded
    except Exception:
        logger.error(f"[macroexpand-1] error while expanding form env_id={id(environment)}\n", exc_info=True)
        return form


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
    'delete_package',
    'shadowing_import',
    'shadow',
    'use_package',
    'unuse_package',
    'macroexpand',
    'macroexpand_1',
]
