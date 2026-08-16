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


def _as_list(x):
    """Normalize a Lisp designator-or-list argument into a Python list.

    Many package operators (`USE-PACKAGE`, `EXPORT`, `SHADOW`, ...) accept
    either a single designator or a list of them (CLHS "list of X or X").
    This was previously a 4-line `isinstance` block copy-pasted at every call
    site; a single shared helper means a shape one copy forgot (e.g. NIL
    itself, which is also a `lispCons`-less empty list) cannot silently
    diverge between operators.
    """
    if x is None or x is lisptype.NIL:
        return []
    if isinstance(x, lisptype.lispCons):
        return list(x)
    if isinstance(x, (list, tuple)):
        return list(x)
    return [x]


def _lisp_list(items):
    """Build a proper Lisp list (`lispCons` chain, NIL when empty).

    Every package accessor that answers "a list of ..." (`PACKAGE-NICKNAMES`,
    `PACKAGE-USE-LIST`, `PACKAGE-USED-BY-LIST`, `PACKAGE-SHADOWING-SYMBOLS`,
    `LIST-ALL-PACKAGES`, ...) used to return a bare Python `list`. A Python
    `list` is a *vector* in this implementation (plan.md Finding M), so
    `(equal (package-nicknames p) nil)` compared an empty vector to NIL and
    `(equal (package-use-list p) (list pkg))` compared a vector to a cons --
    both structurally false regardless of the packages under test, which is
    why nearly every assertion in make-package.lsp/defpackage.lsp failed.
    """
    result = lisptype.NIL
    for item in reversed(list(items)):
        result = lisptype.lispCons(item, result)
    return result


def _designator_to_string(x):
    """Resolve a string designator to plain text (CLHS "string designator").

    A string designator is a string, a symbol (its name), or a character
    (a length-1 string) -- and the ANSI package tests also exercise every
    specialized character-array shape (fill-pointered, adjustable, displaced)
    as a package/nickname/symbol name. This is the one place that decides,
    replacing the `isinstance(x, lispKeyword) ... elif LispSymbol ... else
    str(x)` block that was previously copy-pasted in `MAKE-PACKAGE` and again
    (differently) in `evaluation_core.py`'s `DEFPACKAGE` handling.
    """
    from .comparison import _string_characters
    s = _string_characters(x)
    if s is not None:
        return s
    if isinstance(x, (lisptype.lispKeyword, lisptype.LispSymbol)):
        n = x.name
        # A reader-produced |...|-escaped name keeps its pipes in `.name`
        # (SYMBOL-NAME strips them for the same reason); without this an
        # uninterned designator like `#:|TEST1|` produced "|TEST1|" instead
        # of "TEST1" and every comparison against the plain string failed.
        if isinstance(n, str) and n.startswith('|') and n.endswith('|') and len(n) >= 2:
            n = n[1:-1]
        return n
    if isinstance(x, lisptype.Character):
        return x.char
    from . import arrays as _arrays
    if _arrays.is_array(x) and _arrays.array_rank_of(x) == 1:
        chars = []
        for e in _arrays.array_elements(x):
            if isinstance(e, lisptype.Character):
                chars.append(e.char)
            elif isinstance(e, str) and len(e) == 1:
                chars.append(e)
            else:
                return str(x)
        return ''.join(chars)
    return str(x)


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
    name = _designator_to_string(designator)
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
_MAKE_PACKAGE_KEYS = {'nicknames', 'use'}


@_registry.cl_function('MAKE-PACKAGE')
def make_package(*args):
    """Create a new package (CLHS `MAKE-PACKAGE`).

    (make-package package-name &key nicknames use)
    """
    if not args:
        raise lisptype.LispProgramError(
            "MAKE-PACKAGE: wrong number of arguments (got 0, expected at least 1)")

    name_arg = args[0]
    remaining_args = args[1:] if len(args) > 1 else []
    positional, kwargs = _parse_keyword_args(remaining_args)
    if positional:
        raise lisptype.LispProgramError(
            f"MAKE-PACKAGE: malformed keyword arguments {positional!r}")
    allow_other_keys = lisptype.is_truthy(kwargs.get('allow-other-keys', lisptype.NIL))
    unknown = set(kwargs) - _MAKE_PACKAGE_KEYS - {'allow-other-keys'}
    if unknown and not allow_other_keys:
        raise lisptype.LispProgramError(
            f"MAKE-PACKAGE: unrecognized keyword argument(s) {sorted(unknown)!r}")

    name = _designator_to_string(name_arg)
    nicknames = [_designator_to_string(n) for n in _as_list(kwargs.get('nicknames'))]
    use_list = []
    for item in _as_list(kwargs.get('use')):
        use_list.append(item.name if isinstance(item, lisptype.Package) else _designator_to_string(item))

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
    pkg = coerce_to_package(package)
    return lisptype.LispString(pkg.name)


@_registry.cl_function('PACKAGE-NICKNAMES')
def package_nicknames(package):
    """Get package nicknames."""
    pkg = coerce_to_package(package)
    # Package class uses `nick_names`; accept either for compatibility
    names = getattr(pkg, 'nick_names', None) or getattr(pkg, 'nicknames', [])
    return _lisp_list(lisptype.LispString(n) for n in names)


@_registry.cl_function('RENAME-PACKAGE')
def rename_package(package, new_name, new_nicknames=None):
    """Rename a package."""
    if isinstance(package, lisptype.Package):
        package.name = str(new_name)
    return package


@_registry.cl_function('PACKAGE-USE-LIST')
def package_use_list(package):
    """Get packages this package uses."""
    pkg = coerce_to_package(package)
    return _lisp_list(pkg.use_list)


@_registry.cl_function('PACKAGE-USED-BY-LIST')
def package_used_by_list(package):
    """Get packages that use this package."""
    pkg = coerce_to_package(package)
    used_by = []
    for p in list({id(p): p for p in state.packages.values()}.values()):
        if pkg in getattr(p, 'use_list', []):
            used_by.append(p)
    return _lisp_list(used_by)


@_registry.cl_function('PACKAGE-SHADOWING-SYMBOLS')
def package_shadowing_symbols(package):
    """Get shadowing symbols in package."""
    pkg = coerce_to_package(package)
    syms = []
    for name in getattr(pkg, 'shadowing_symbols', set()):
        s = pkg.symbols.get(name)
        if s is not None:
            syms.append(s)
    return _lisp_list(syms)


@_registry.cl_function('PACKAGE-EXTERNAL-SYMBOLS')
def package_external_symbols(package):
    """Get external symbols in package.

    Returns a list of symbols that are exported from the package.
    """
    pkg = coerce_to_package(package)
    syms = []
    external_names = getattr(pkg, 'external_symbols', set())
    for name in external_names:
        s = pkg.symbols.get(name)
        if s is not None:
            syms.append(s)
    return _lisp_list(syms)


@_registry.cl_function('PACKAGE-INTERNAL-SYMBOLS')
def package_internal_symbols(package):
    """Get internal (non-exported) symbols in package.

    Returns a list of symbols that are in the package but not exported.
    """
    pkg = coerce_to_package(package)
    syms = []
    external_names = getattr(pkg, 'external_symbols', set())
    for name, sym in pkg.symbols.items():
        if name not in external_names:
            syms.append(sym)
    return _lisp_list(syms)


@_registry.cl_function('LIST-ALL-PACKAGES')
def list_all_packages():
    """List all known packages."""
    unique = {id(p): p for p in state.packages.values()}
    return _lisp_list(unique.values())


@_registry.cl_function('UNINTERN')
def unintern(symbol, package=None):
    """Remove symbol from package."""
    name = _designator_to_string(symbol)
    pkg = coerce_to_package(package)
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
    pkg = coerce_to_package(package)
    for s in _as_list(symbols):
        name = s.name if hasattr(s, 'name') else _designator_to_string(s)
        pkg.external_symbols.discard(name.upper())
    return lisptype.T


@_registry.cl_function('SHADOWING-IMPORT')
def shadowing_import(symbols, package=None):
    """Shadowing-import symbols into a package (CLHS 11.3).

    Unlike `SHADOW`, the arguments here are already the actual symbols to be
    made present in `package` (typically fetched from another package with
    `FIND-SYMBOL`) -- the given symbol object itself becomes accessible, it
    is not copied or re-interned under a fresh identity, and it is marked as
    a shadowing symbol so it takes precedence over anything `package` would
    otherwise inherit through `USE-PACKAGE`.
    """
    pkg = coerce_to_package(package)
    for sym in _as_list(symbols):
        name = sym.name if hasattr(sym, 'name') else _designator_to_string(sym)
        pkg.symbols[name] = sym
        pkg.shadowing_symbols.add(name)
    return lisptype.T


@_registry.cl_function('SHADOW')
def shadow(symbols, package=None):
    """Create shadowing symbols in a package (CLHS 11.3).

    Unlike `SHADOWING-IMPORT`, the arguments are string designators: if a
    symbol of that name is already present (not merely inherited) in
    `package`, it is simply marked as shadowing; otherwise a *new* symbol is
    interned directly into `package`, deliberately not the one `package`
    would inherit via its use-list, and then marked as shadowing.
    """
    pkg = coerce_to_package(package)
    for designator in _as_list(symbols):
        name = _designator_to_string(designator)
        existing = pkg.symbols.get(name)
        if existing is None:
            existing = lisptype.LispSymbol(name, package=pkg)
            pkg.symbols[name] = existing
        pkg.shadowing_symbols.add(name)
    return lisptype.T


@_registry.cl_function('USE-PACKAGE')
def use_package(packages, package=None):
    """Install packages into use-list."""
    target = coerce_to_package(package)
    for p in _as_list(packages):
        pkgobj = p if isinstance(p, lisptype.Package) else lisptype.find_package(_designator_to_string(p))
        if pkgobj is None:
            pkgobj = lisptype.make_package(_designator_to_string(p))
        if pkgobj not in target.use_packages:
            target.use_packages.append(pkgobj)
    return lisptype.T


@_registry.cl_function('UNUSE-PACKAGE')
def unuse_package(packages, package=None):
    """Remove packages from use-list."""
    target = coerce_to_package(package)
    for p in _as_list(packages):
        pkgobj = p if isinstance(p, lisptype.Package) else lisptype.find_package(_designator_to_string(p))
        if pkgobj in target.use_packages:
            target.use_packages.remove(pkgobj)
    return lisptype.T


@_registry.cl_function('DELETE-PACKAGE')
def delete_package(package):
    """Delete a package by instance or name.

    Removes the package from the global package registry and
    from other packages' use-lists. Returns T if deleted, NIL otherwise.
    """
    pkg = package if isinstance(package, lisptype.Package) else lisptype.find_package(_designator_to_string(package))
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
