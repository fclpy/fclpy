"""Symbol and package management operations."""

import time
import fclpy.lisptype as lisptype
import fclpy.state as state
from fclpy.lispfunc import registry as _registry


def _require_symbol(value, operator):
    """Signal a TYPE-ERROR unless `value` is a Lisp symbol (CLHS 4.2).

    A single helper, used by every symbol-taking builtin in this module
    (`SYMBOL-NAME`, `SYMBOL-VALUE`, `SYMBOL-PACKAGE`, `BOUNDP`-adjacent
    callers, `MAKE-SYMBOL`, `COPY-SYMBOL`, ...). Without it, a non-symbol
    argument used to fall through to `str(value)` or a dict access and
    answer a value (or raise the wrong condition), so `check-type-error`
    in the ansi-test suite collected every non-symbol element of the
    universe and asserted "TYPE-ERROR" against the form -- which
    `(symbol-name 0)`, `(symbol-package #\a)`, `(symbol-value '(a))`
    silently failed.
    """
    if not lisptype.is_symbol(value):
        raise lisptype.LispTypeError(
            f"{operator}: {value!r} is not a symbol",
            expected_type='SYMBOL', actual_value=value)
    return value


# --- Symbol operations ---
@_registry.cl_function('SYMBOL-NAME')
def symbol_name(*args):
    """Get the name of a symbol."""
    if len(args) != 1:
        raise lisptype.LispProgramError(
            f"SYMBOL-NAME: wrong number of arguments (got {len(args)}, expected 1)"
        )
    symbol = _require_symbol(args[0], 'SYMBOL-NAME')
    if hasattr(symbol, 'name'):
        # A symbol's name is returned exactly as it is. There used to be a
        # `raw.startswith('|') and raw.endswith('|')` branch stripping the
        # bars, which was compensation for a reader that stored `|abc|` as a
        # name *containing* the bars -- `lispreader` had no multiple-escape
        # handling, so the escape syntax leaked into the name. With the reader
        # reading `|abc|` as the name `abc` (CLHS 2.4.5), stripping here is
        # simply wrong: it renamed every symbol whose name genuinely begins
        # and ends with a vertical bar, and answered "" for the symbol named
        # "|" -- which is what `set-syntax-from-char.lsp` reads back after
        # making `|` a constituent.
        return lisptype.LispString(str(symbol.name))
    return lisptype.LispString(str(symbol))


@_registry.cl_function('SYMBOL-PACKAGE')
def symbol_package(*args):
    """Get the package a symbol belongs to."""
    if len(args) != 1:
        raise lisptype.LispProgramError(
            f"SYMBOL-PACKAGE: wrong number of arguments (got {len(args)}, expected 1)"
        )
    symbol = _require_symbol(args[0], 'SYMBOL-PACKAGE')
    return getattr(symbol, 'package', None)


@_registry.cl_function('SYMBOL-VALUE')
def symbol_value(*args):
    """Get the value bound to a symbol."""
    if len(args) != 1:
        raise lisptype.LispProgramError(
            f"SYMBOL-VALUE: wrong number of arguments (got {len(args)}, expected 1)"
        )
    symbol = _require_symbol(args[0], 'SYMBOL-VALUE')
    # T, NIL, and keywords are self-evaluating and therefore always bound.
    if symbol is lisptype.T or getattr(symbol, 'name', None) in ('T', 'NIL') or isinstance(symbol, lisptype.lispKeyword):
        return symbol
    value = getattr(symbol, 'value', None)
    if value is None:
        from fclpy.lispfunc.evaluation_core import ConditionException
        cond = lisptype.UnboundVariable(name=symbol)
        raise ConditionException(cond, recoverable=False)
    return value


@_registry.cl_function('MAKE-SYMBOL')
def make_symbol(*args):
    """Create a new uninterned symbol (CLHS 13.1.2.1).

    `(make-symbol name)` returns a freshly allocated, uninterned symbol
    whose name is the given string designator. The new symbol's package
    is NIL.

    `name` is a *string designator*: a string, a symbol, or a
    specialized character array. **A character is not a valid name**
    for `MAKE-SYMBOL` -- CLHS permits a character to *name* a string in
    general, but `make-symbol.error.1` collects every non-string in the
    ANSI mini-universe (including `#\Space`) and requires `MAKE-SYMBOL`
    to signal a TYPE-ERROR for each, which is what SBCL, CCL and ECL
    all do. Other ANSI implementations that treat characters as string
    designators would still pass the test if their `stringp` predicate
    is the one `check-type-error` uses, because `(stringp #\Space)` is
    NIL; here we enforce it directly by not falling through the
    character branch.

    `make-symbol.error.1` and `make-symbol.error.11` check the
    non-designator half: a number, a stream, a path, a list of
    characters, or any other non-designator must signal a TYPE-ERROR
    whose datum is the value passed in. The previous `LispSymbol(str(x))`
    silently turned every object into *some* Python string and answered
    an uninterned symbol whose name was the object's `__repr__`, so
    those tests collected every non-string element of `*mini-universe*`
    and asked "did this signal?" -- which it never did.
    """
    if len(args) != 1:
        raise lisptype.LispProgramError(
            f"MAKE-SYMBOL: wrong number of arguments (got {len(args)}, expected 1)"
        )
    arg = args[0]
    from . import arrays as _arrays
    if isinstance(arg, (str, lisptype.LispString)):
        from .misc_packages import _designator_to_string
        return lisptype.LispSymbol(_designator_to_string(arg))
    if isinstance(arg, (lisptype.lispKeyword, lisptype.LispSymbol)):
        n = arg.name
        if isinstance(n, str) and n.startswith('|') and n.endswith('|') and len(n) >= 2:
            n = n[1:-1]
        return lisptype.LispSymbol(n)
    if _arrays.is_array(arg) and _arrays.array_rank_of(arg) == 1:
        chars = []
        for e in _arrays.array_elements(arg):
            if isinstance(e, lisptype.Character):
                chars.append(e.char)
            elif isinstance(e, str) and len(e) == 1:
                chars.append(e)
            else:
                break
        else:
            return lisptype.LispSymbol(''.join(chars))
    raise lisptype.LispTypeError(
        f"MAKE-SYMBOL: {arg!r} is not a string designator",
        expected_type='STRING-DESIGNATOR', actual_value=arg)


@_registry.cl_function('COPY-SYMBOL')
def copy_symbol(*args):
    """Copy a symbol, optionally copying its plist/value/function (CLHS 13.1.2.1).

    `(copy-symbol sym)` returns an uninterned symbol with the same name and
    nothing else -- no plist, no value, no function. `(copy-symbol sym t)`
    additionally copies the plist, value, and function binding; without it,
    the new symbol is a fresh empty slot with only the name in common.

    The previous implementation was a one-liner that only copied the name,
    so `copy-symbol.2`/`copy-symbol.3` (which expect a matching plist and
    `boundp`/`fboundp` parity when the second arg is true) were
    permanently failing.
    """
    if len(args) < 1 or len(args) > 2:
        raise lisptype.LispProgramError(
            f"COPY-SYMBOL: wrong number of arguments (got {len(args)}, expected 1-2)"
        )
    symbol = _require_symbol(args[0], 'COPY-SYMBOL')
    copy_props = bool(lisptype.is_truthy(args[1])) if len(args) == 2 else False

    new_sym = make_symbol(symbol_name(symbol))

    if copy_props:
        # Plist -- copied as a fresh cons chain (EQUAL, not EQ, on the
        # elements, the way `setf` of plist expects).
        src_plist = getattr(symbol, 'plist', lisptype.NIL)
        if src_plist is None or src_plist is lisptype.NIL:
            new_sym.plist = lisptype.NIL
        elif isinstance(src_plist, dict):
            # CLHS 13.1.2.1 copies the plist as a list; what the test asks
            # for is `(equal (symbol-plist y) (symbol-plist x))`, so a dict
            # representation that round-trips through the same
            # `symbol-plist` accessor is what the property transfer looks
            # like from the outside.
            new_sym.plist = dict(src_plist)
        else:
            from .sequences_compose import copy_list
            new_sym.plist = copy_list(src_plist)
        # Value cell. NIL, T and keywords are self-evaluating, so copying
        # them means returning the same singleton; everything else
        # transfers the actual stored value, if any.
        if (symbol is lisptype.T or symbol is lisptype.NIL
                or isinstance(symbol, lisptype.lispKeyword)):
            new_sym.value = symbol
        else:
            src_value = getattr(symbol, 'value', None)
            if src_value is not None:
                new_sym.value = src_value
        # Function cell.
        src_func = getattr(symbol, 'function', None)
        if src_func is not None:
            new_sym.function = src_func
    return new_sym


# --- Gensym (unique symbol generation) ---
# Initialize *GENSYM-COUNTER* as a special variable
_gensym_counter_symbol = None

def _get_gensym_counter_symbol():
    """Get the *GENSYM-COUNTER* symbol, creating it if needed."""
    global _gensym_counter_symbol
    if _gensym_counter_symbol is None:
        _gensym_counter_symbol = lisptype.COMMON_LISP_PACKAGE.intern_symbol('*GENSYM-COUNTER*')
        # Set initial value to 0
        _gensym_counter_symbol.value = 0
    return _gensym_counter_symbol

def _get_gensym_counter(env=None):
    """Get current value of *GENSYM-COUNTER* from environment or symbol."""
    sym = _get_gensym_counter_symbol()
    # Try environment first (for LET bindings)
    if env is not None:
        try:
            val = env.find_variable(sym)
            if val is not None:
                return val
        except Exception:
            pass
    # Fall back to symbol value
    val = getattr(sym, 'value', None)
    if val is None:
        return 0
    return val

def _set_gensym_counter(value, env=None):
    """Set *GENSYM-COUNTER* in environment or symbol."""
    sym = _get_gensym_counter_symbol()
    # Try environment first (for LET bindings)
    if env is not None:
        try:
            env.set_variable(sym, value)
            return
        except Exception:
            pass
    # Fall back to symbol value
    sym.value = value

@_registry.cl_function('GENSYM')
def gensym(*args):
    """Generate unique symbol with prefix.
    
    (gensym) - uses "G" prefix and *gensym-counter*, increments counter
    (gensym string) - uses string prefix and *gensym-counter*, increments counter
    (gensym integer) - uses "G" prefix and integer, does NOT increment counter
    """
    if len(args) > 1:
        raise lisptype.LispProgramError(
            f"GENSYM: wrong number of arguments (got {len(args)}, expected 0-1)"
        )
    
    # Get current environment from state
    env = getattr(state, 'current_environment', None)
    
    prefix = "G"
    use_counter = True
    explicit_number = None
    
    if len(args) == 1:
        arg = args[0]
        if isinstance(arg, str) or isinstance(arg, lisptype.LispString):
            prefix = str(arg)
        elif isinstance(arg, int) and arg >= 0:
            # Integer argument: use it as the number, don't increment counter
            explicit_number = arg
            use_counter = False
        else:
            raise lisptype.LispTypeError(
                f"GENSYM: argument must be a string or non-negative integer, got {type(arg).__name__}",
                expected_type="(OR STRING UNSIGNED-BYTE)",
                actual_value=arg
            )
    
    if use_counter:
        counter = _get_gensym_counter(env)
        # Validate counter
        if not isinstance(counter, int) or counter < 0:
            raise lisptype.LispTypeError(
                f"*GENSYM-COUNTER* must be a non-negative integer, got {counter}",
                expected_type="(INTEGER 0 *)",
                actual_value=counter
            )
        number = counter
        # Increment counter after using it
        _set_gensym_counter(counter + 1, env)
    else:
        number = explicit_number
    
    # Create uninterned symbol
    sym = lisptype.LispSymbol(f"{prefix}{number}")
    sym.package = None  # Ensure it's uninterned
    return sym


# --- Package operations ---
def get_current_package():
    """Get the value of *PACKAGE* (current package)."""
    return state.current_package_value()


@_registry.cl_function('IN-PACKAGE')
def in_package(*args):
    """Set current package and return it (CLHS 11.2).

    `in-package` is a *binding form*: it is supposed to be wrapped in
    `LET`/`LET*` so the package change is local. As a top-level form
    fclpy additionally sets `*package*` globally so subsequent reads
    see the new value, the same way `(in-package :foo)` then `(read)`
    works in a conforming implementation.

    The previous version *created* the package when the name did not
    resolve -- `(in-package "H")` for a deleted package would silently
    bring "H" back. CLHS 11.2 says `in-package` is undefined for a
    package that does not exist; `in-package.5` requires a
    PACKAGE-ERROR in that case. The new version signals one.
    """
    if len(args) != 1:
        raise lisptype.LispProgramError(
            f"IN-PACKAGE: wrong number of arguments (got {len(args)}, expected 1)"
        )
    name = args[0]
    if isinstance(name, lisptype.Package):
        pkg = name
    else:
        from .misc_packages import _designator_to_string
        pkg_name = _designator_to_string(name)
        pkg = lisptype.find_package(pkg_name)
        if pkg is None:
            from .evaluation_conditions import signal_error_object
            condition = lisptype.PackageError(
                message=f"IN-PACKAGE: no package named {pkg_name!r} currently exists")
            condition.package = pkg_name
            signal_error_object(condition)
            return lisptype.NIL

    state.current_package = pkg
    env = getattr(state, 'current_environment', None)
    if env is not None:
        package_sym = lisptype.COMMON_LISP_PACKAGE.intern_symbol('*PACKAGE*')
        env.set_variable(package_sym, pkg)
    return pkg


@_registry.cl_function('IMPORT')
def import_symbol(symbols, package=None):
    """Import symbols into a package (CLHS 11.3).

    The given symbol objects themselves become accessible in `package` as
    *internal* symbols -- `IMPORT` does not export them, and it must not
    re-intern a fresh symbol under the same name, or `(eq sym (find-symbol
    (symbol-name sym) package))` becomes false for every imported symbol and
    `SYMBOL-PACKAGE` still points at the original home package.

    Three things this version handles that the previous copy skipped:

      * **The imported symbol's home package is `package`** when the symbol
        was uninterned or its home was the same package (CLHS 11.1.2.1).
        Without the reassignment, `import.5` (which imports an
        uninterned `make-symbol`'d symbol) reports the symbol as
        still uninterned.

      * **A name conflict is a correctable PACKAGE-ERROR** (CLHS 11.2's
        import error case). `import.error.3` interned the name first,
        then asked IMPORT to import the original; the previous code
        silently overwrote the existing entry, which is wrong.
        `import.error.4`/`.5` further require a non-`abort` restart to
        be available, which is the CERROR / `signal_cerror_object`
        contract (an implicit CONTINUE restart bound to the condition),
        not a bare `signal_error_object` (which only establishes the
        universal ABORT restart and leaves nothing for the
        `set-difference ... remove 'abort` check to find).
    """
    from .misc_packages import _as_list, coerce_to_package
    pkg = coerce_to_package(package)
    for s in _as_list(symbols):
        if not lisptype.is_symbol(s):
            raise lisptype.LispTypeError(
                f"IMPORT: {s!r} is not a symbol",
                expected_type='SYMBOL', actual_value=s)
        name = s.name if hasattr(s, 'name') else str(s)
        existing = pkg.symbols.get(name)
        if existing is not None and existing is not s:
            from .evaluation_conditions import _signal_cerror_object
            condition = lisptype.PackageError(
                package=pkg,
                message=(f"IMPORT: a symbol named {name} is already "
                         f"accessible in package {pkg.name}"))
            condition.symbol = s
            return _signal_cerror_object(
                condition, continue_format="Delete the existing symbol")
        pkg.symbols[name] = s
        # Adopt the symbol if it was uninterned or already home in `pkg`.
        # Per CLHS 11.1.2.1, IMPORT's effect on the symbol's home is:
        # if uninterned, its home becomes `package`; otherwise the home
        # is unchanged. A symbol with a *different* existing home keeps
        # it -- so `(import 'cl:car pkg)` does not make CL:CAR's home
        # into `pkg`.
        home = getattr(s, 'package', None)
        if home is None or home is pkg:
            s.package = pkg
    return lisptype.T


@_registry.cl_function('INTERN')
def intern(name, package=None):
    """Intern a symbol in a package (CLHS 11.2). Creates a new
    interned symbol if the name is not present.

    `name` is a *string designator* (CLHS 11.1.1.1): a string, a
    symbol, a character, or a specialized character array. The
    previous version did `name = str(name)` for non-string arguments,
    which discarded a character array's content and asked the package
    to intern the *Python repr* of the array -- so the displaced /
    fill-pointered / adjustable character arrays `intern.5`-.11`
    exercise as names produced a Python `repr` like
    `<lispfunc.arrays.LispArray object at 0x...>` instead of the actual
    text, and `(intern "XYZZY" p) == (intern <array with same chars>
    p)` was false.

    Empty arrays are also designators (`:notes (:nil-vectors-are-strings)`):
    `(intern "" p)` must equal `(intern (make-array 0 :element-type nil) p)`.
    A `NIL`-element-type array's contents are all `NIL`, the textual
    empty string; `intern.3` exercises this directly.

    The string is interned **as given**: INTERN does no case conversion
    (CLHS 11.2 -- case is a *reader* rule), so `(intern "12e5")` and the
    reader's `|12e5|` denote the same symbol. The previous default call
    upcased, and under `:preserve` (`read-symbol.25`) the interned symbol
    and the read one came out different.

    Interning into the KEYWORD package yields an **external** keyword
    (CLHS 11.1.2 -- every symbol in KEYWORD is external), which is what
    `keyword.2` requires of `(do-symbols ...)` over it.
    """
    from .misc_packages import _designator_to_string
    name = _designator_to_string(name)
    from .misc_packages import coerce_to_package
    pkg = coerce_to_package(package)
    if pkg is lisptype.KEYWORD_PACKAGE:
        return lisptype.intern_keyword(name, exact_case=True)
    return pkg.intern_symbol(name, exact_case=True)


@_registry.cl_function('FIND-SYMBOL')
def find_symbol(name, package=None):
    """Find a symbol in a package.

    Returns two values:
    1. The symbol (or NIL if not found)
    2. Status: :INTERNAL, :EXTERNAL, :INHERITED, or NIL if not found
    """
    from .misc_packages import coerce_to_package
    pkg = coerce_to_package(package)
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
def find_package(*args):
    """Find a package by name (CLHS 11.2).

    Accepts a package designator: a package, a string, a symbol, or a
    character. The previous version always called `_designator_to_string`
    on the argument, so a `Package` object as the designator was coerced
    to its `__repr__` (e.g. `"#<PACKAGE COMMON-LISP>"`) and the lookup
    answered NIL -- `find-package.11` passes the result of
    `(find-package "CL")` back through `find-package` and expects
    identity.
    """
    if len(args) != 1:
        raise lisptype.LispProgramError(
            f"FIND-PACKAGE: wrong number of arguments (got {len(args)}, expected 1)"
        )
    arg = args[0]
    if isinstance(arg, lisptype.Package):
        return arg
    from .misc_packages import _designator_to_string
    return lisptype.find_package(_designator_to_string(arg))


@_registry.cl_function('FIND-ALL-SYMBOLS')
def find_all_symbols(*args):
    """Find all symbols with given name across all packages (CLHS 11.2).

    Searches all packages, including built-in COMMON-LISP and KEYWORD packages,
    for symbols with the given name. Returns a list of all matching symbols.
    Special handling for NIL: also returns the NIL value itself as one of
    the symbols named "NIL".
    """
    if len(args) != 1:
        raise lisptype.LispProgramError(
            f"FIND-ALL-SYMBOLS: wrong number of arguments (got {len(args)}, expected 1)"
        )
    from . import misc_packages
    # Handle string designators properly (symbols, strings, characters)
    name_str = misc_packages._designator_to_string(args[0])
    results = []

    # Special case: NIL is a symbol that deserves special treatment
    # It should be included when searching for "NIL", appearing once in the results
    if name_str.upper() == "NIL":
        results.append(lisptype.NIL)

    # Use all_packages() to include built-in packages
    # Deduplicate by identity to avoid returning the same symbol multiple times
    seen_ids = {id(lisptype.NIL)} if name_str.upper() == "NIL" else set()
    for pkg in misc_packages.all_packages():
        sym, status = pkg.find_symbol(name_str)
        if sym is not None and id(sym) not in seen_ids:
            results.append(sym)
            seen_ids.add(id(sym))

    result = lisptype.NIL
    for sym in reversed(results):
        result = lisptype.lispCons(sym, result)
    return result


@_registry.cl_function('EXPORT')
def export(symbols, package=None):
    """Export symbols from a package (CLHS 11.2).

    `(export sym . package)` makes `sym` (or each symbol in a list of
    them) external in `package` (defaulting to `*package*`).

    Three things this used to skip:
      * A symbol *not present* in the package is not an automatic
        error here -- the harness's `export.4` and `export.5` rely on
        the operator catching the cases CLHS names: a symbol
        reachable from another package but not directly in `package`
        (case (b)), and a name conflict in the used-by list (case
        (c)). The two both signal a CORRECTABLE PACKAGE-ERROR with
        the offending symbol on its `package` slot.
      * A non-symbol element in the designator list used to be
        `str()`-ed and interned under a Python-string symbol name;
        now it is a TYPE-ERROR (`expected-type SYMBOL`).
      * `(export 'b::bar "A")` -- exporting a symbol from a
        *different* package, where `b::bar` exists in `b` and is not
        accessible in `A` -- is the first of the two
        PACKAGE-ERROR cases.
    """
    if isinstance(symbols, lisptype.lispCons):
        symbols = list(symbols)  # lispCons is iterable
    elif not isinstance(symbols, (list, tuple)):
        symbols = [symbols]
    from .misc_packages import coerce_to_package, _externals_of
    pkg = coerce_to_package(package)

    for s in symbols:
        if not lisptype.is_symbol(s):
            raise lisptype.LispTypeError(
                f"EXPORT: {s!r} is not a symbol",
                expected_type='SYMBOL', actual_value=s)
        sym_name = s.name if hasattr(s, 'name') else str(s)

        # Case (b) of CLHS 11.2: a symbol is being exported from a package
        # in which it is not accessible. `s` is a CL:FOO symbol but `pkg`
        # is, say, "A" -- FOO is not in A (interned or inherited). ANSI
        # requires a correctable PACKAGE-ERROR; the harness's `export.4`
        # catches one and expects `package-error` back.
        already_accessible = pkg.find_symbol(sym_name)[0] is not None
        if not already_accessible:
            from .evaluation_conditions import _signal_cerror_object
            condition = lisptype.PackageError(
                package=pkg,
                message=(f"EXPORT: the symbol {s.name} is not accessible "
                         f"in package {pkg.name}"))
            condition.symbol = s
            _signal_cerror_object(
                condition,
                continue_format="Delete the existing symbol or export anyway")
            return lisptype.NIL

        # Case (c) of CLHS 11.2: a *name conflict in the used-by list*.
        # Exporting `s` would cause one of `pkg`'s *using* packages (a
        # package that has `pkg` in its `use_list`, i.e. one that
        # inherits `pkg`'s externals) to inherit the freshly-exported
        # `s` when it already inherits a *different* symbol of the
        # same name (via its own use list or its own interned symbols)
        # -- so two used packages would resolve the same name to two
        # different symbols, breaking CLHS 11.1.2.1's package
        # consistency. The test `export.5` sets this up exactly:
        # TEST2 :uses TEST1 and exports its own `X`; then
        # `(intern "X" "TEST1")` plus `(export sym "TEST1")` would
        # make a using-of-TEST1 package (here TEST2) see two different
        # `X` symbols depending on which it inherited first, and the
        # standard says that's a PACKAGE-ERROR. The check therefore
        # iterates over *using* packages of `pkg` (those that have
        # `pkg` in their `use_list`).
        using_iter = list(getattr(pkg, 'used_by_list', ()) or ())
        # Some code paths populate `used_by_list`, others populate
        # `use_packages` (the inverse relation) on the using side.
        # If `used_by_list` is empty, derive it by walking
        # `state.packages` for any package that lists `pkg` in its
        # `use_packages`.
        if not using_iter:
            for other in list(getattr(state, 'packages', {}).values() if hasattr(state, 'packages') else ()):
                if other is pkg or not isinstance(other, lisptype.Package):
                    continue
                other_use = list(getattr(other, 'use_packages', ()) or ())
                if any((u is pkg) if not isinstance(u, str) else (u.upper() == pkg.name.upper())
                       for u in other_use):
                    using_iter.append(other)
        for used_by_pkg in using_iter:
            if not isinstance(used_by_pkg, lisptype.Package):
                continue
            for other in _externals_of(used_by_pkg):
                if (other is not s
                        and getattr(other, 'name', None) is not None
                        and str(other.name).upper() == sym_name.upper()):
                    from .evaluation_conditions import _signal_cerror_object
                    condition = lisptype.PackageError(
                        package=pkg,
                        message=(f"EXPORT: exporting {s.name} from {pkg.name} "
                                 f"would create a name conflict with the "
                                 f"external symbol of the same name in "
                                 f"using package {used_by_pkg.name}"))
                    condition.symbol = s
                    _signal_cerror_object(
                        condition,
                        continue_format="Unintern the conflicting symbol or export anyway")
                    return lisptype.NIL

        # Intern + export. `pkg.intern_symbol` may promote an inherited
        # symbol to a directly-present one without changing its identity,
        # which is what `export_symbol` expects.
        sym = pkg.intern_symbol(sym_name)
        pkg.export_symbol(sym_name)
    return lisptype.T


@_registry.cl_function('GENTEMP')
def gentemp(*args):
    """Generate temporary interned symbol.

    ANSI: (gentemp &optional (prefix "T") (package *package*))
    Accepts 0-2 args. Prefix must be a string-designator; package may be
    a package object, string-designator, symbol, or character.
    This implementation uses an internal counter to guarantee uniqueness
    and does not alter *GENSYM-COUNTER*.
    """
    from fclpy.lispfunc import registry as _reg  # local import for safety

    # Validate arity
    if len(args) > 2:
        raise lisptype.LispProgramError(
            f"GENTEMP: wrong number of arguments (got {len(args)}, expected 0-2)"
        )

    # Determine environment (for honoring dynamic *PACKAGE* if bound)
    env = getattr(state, 'current_environment', None)

    # Defaults
    prefix = 'T'
    package = None

    # Helper to extract prefix string from possible Lisp types
    if len(args) >= 1:
        p = args[0]
        if isinstance(p, (str, lisptype.LispString)):
            prefix = str(p)
        else:
            raise lisptype.LispTypeError(
                f"GENTEMP: prefix must be a string-designator, got {type(p).__name__}",
                expected_type='STRING-DESIGNATOR',
                actual_value=p
            )

    # Handle package arg
    if len(args) == 2:
        pkg_arg = args[1]
        # Accept Package instances directly
        if isinstance(pkg_arg, lisptype.Package):
            package = pkg_arg
        elif isinstance(pkg_arg, lisptype.Character):
            package = str(pkg_arg.char)
        elif isinstance(pkg_arg, lisptype.LispSymbol):
            package = pkg_arg.name
        elif isinstance(pkg_arg, (str, lisptype.LispString)):
            package = str(pkg_arg)
        else:
            raise lisptype.LispTypeError(
                f"GENTEMP: package must be a package or string-designator, got {type(pkg_arg).__name__}",
                expected_type='PACKAGE OR STRING-DESIGNATOR',
                actual_value=pkg_arg
            )
    else:
        # No explicit package arg: try dynamic *PACKAGE* then fall back to state
        try:
            package_sym = lisptype.COMMON_LISP_PACKAGE.intern_symbol('*PACKAGE*')
            if env is not None:
                try:
                    dyn = env.find_variable(package_sym)
                    if dyn is not None:
                        package = dyn
                except Exception:
                    pass
            if package is None:
                package = getattr(state, 'current_package', None)
        except Exception:
            package = getattr(state, 'current_package', None)

    # Generate unique numeric suffix using process-local counter
    try:
        _gentemp_counter
    except NameError:
        # Initialize module-level counter if not present
        globals()['_gentemp_counter'] = 0

    number = globals()['_gentemp_counter']
    globals()['_gentemp_counter'] = number + 1

    name = f"{prefix}{number}"
    # Delegate to intern() which will handle package/name conversion and binding
    return intern(name, package)


def _apropos_matches(string, package):
    """The symbols APROPOS and APROPOS-LIST both look for (CLHS 25.1.2).

    One search for the two operators, because they are specified as the same
    search with two different reports -- APROPOS-LIST answers the symbols,
    APROPOS prints them. Two copies of the traversal would be two chances to
    disagree about what "matching" means.

    `string` is a *string designator*, resolved through the existing single
    resolver rather than a fourth copy of that rule: the tests pass "F", `#\\F`,
    `:|F|` and `'#:|X|` for the same search, plus every specialized
    character-array shape (`do-special-strings`).

    `package` NIL or omitted searches every package; a package designator
    searches the symbols *accessible* in that package, which is CLHS's wording
    and what `package_symbols(..., 'symbols')` already answers. Matching is
    case-insensitive, as it is in every implementation -- CLHS leaves it
    unspecified and a case-sensitive search would make APROPOS useless on a
    readtable that downcases.
    """
    from .misc_packages import (_designator_to_string, package_symbols,
                                coerce_to_package, all_packages)

    needle = _designator_to_string(string)
    if needle is None:
        raise lisptype.LispTypeError(
            f"APROPOS: not a string designator: {string}",
            expected_type="STRING-DESIGNATOR", actual_value=string)
    needle = needle.upper()

    if package is None or package is lisptype.NIL \
            or isinstance(package, lisptype.lispNull):
        candidates = []
        for pkg in all_packages():
            candidates.extend(package_symbols(pkg, 'present-symbols'))
    else:
        candidates = package_symbols(coerce_to_package(package), 'symbols')

    # Identity is the key: a symbol inherited into several packages, or present
    # in one and accessible from another, is one symbol and must be reported
    # once -- `apropos-list.1` requires `(equal result (list sym))` exactly.
    seen = set()
    matches = []
    for symbol in candidates:
        name = getattr(symbol, 'name', None)
        if not isinstance(name, str) or id(symbol) in seen:
            continue
        if needle in name.upper():
            seen.add(id(symbol))
            matches.append(symbol)
    return matches


@_registry.cl_function('APROPOS-LIST')
def apropos_list(string, package=None):
    """APROPOS-LIST (CLHS 25.1.2) -- the matching symbols, as a *list*.

    A Lisp list, built with `make_lisp_list`: a Python list is a simple
    general *vector* in this implementation, and returning one here would make
    `(member sym (apropos-list "X"))` and `(equal result (list sym))` both
    fail against something that prints convincingly as a list
    (CLAUDE.md, sequence protocol).
    """
    from .sequence_protocol import make_lisp_list
    return make_lisp_list(_apropos_matches(string, package))


@_registry.cl_function('APROPOS')
def apropos(string, package=None):
    """APROPOS (CLHS 25.1.2): print the matching symbols, return no values.

    Prints nothing at all when nothing matches -- `apropos.1` searches for a
    random string until `APROPOS-LIST` says there are no matches and then
    requires the captured output to be exactly `""`, so a header line printed
    unconditionally fails it.

    Each symbol is printed with its home package, and with what its bindings
    are, because that is what makes the operator useful; the only part the
    tests constrain is that the symbol's name appears.
    """
    from .io_write import write_text
    from fclpy.printer import write_object
    import fclpy.state as state

    for symbol in _apropos_matches(string, package):
        home = getattr(symbol, 'package', None)
        home_name = getattr(home, 'name', None)
        prefix = f"{home_name}::" if isinstance(home_name, str) else ""
        notes = []
        env = getattr(state, 'current_environment', None)
        if env is not None and env.find_func(symbol) is not None:
            notes.append("function")
        if getattr(symbol, 'value', None) is not None:
            notes.append("value")
        suffix = f" [{', '.join(notes)}]" if notes else ""
        write_text(f"{prefix}{write_object(symbol, escape=False)}{suffix}\n", None)
    return lisptype.MultipleValues()


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
