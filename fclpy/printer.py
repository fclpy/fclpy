"""The Lisp printer -- CLHS 22.1.

One printer. Every Lisp-visible printed representation is produced here:
``PRIN1``, ``PRINC``, ``PRINT``, ``WRITE``, the three ``*-TO-STRING`` variants,
and ``FORMAT``'s ``~A``/``~S``.

Why this module was rewritten rather than extended
--------------------------------------------------
Printing used to be ``str(obj)``/``repr(obj)`` -- ``lisptype.lisp_str`` and
``lisptype.lisp_repr`` are exactly that -- so the printed representation of
every type lived in that type's Python ``__str__``/``__repr__``. **A dunder
method takes no arguments, so it structurally cannot consult a printer control
variable.** Three consequences followed, and they are one defect:

* The printer control variables were dead. ``io_write.PrinterSettings`` held
  them as Python globals reachable only through ``@cl_function('*PRINT-BASE*')``
  accessors, which no binding form can reach, so ``(let ((*print-base* 2))
  (prin1 5))`` printed ``5`` -- and ``*print-base*`` at top level evaluated to a
  *Python function object* (standing rule 2), because the symbol had no value
  cell and evaluation fell through to the function registry.
* ``PRINC`` and ``PRIN1`` drifted into two unrelated representations
  (``__str__`` vs ``__repr__``) rather than one printer called with
  ``*PRINT-ESCAPE*`` bound differently, which is all CLHS 22.1.3.2 says they
  are. ``Character.__str__`` returning ``#\\a`` is that drift: ``(princ #\\a)``
  printed ``#\\a`` instead of ``a``.
* ``WRITE``/``WRITE-TO-STRING`` discarded their keyword arguments entirely.

Control variables are therefore read here from the **live dynamic
environment**, in the same order the evaluator resolves a variable reference,
and the one recursive :func:`write_object` is parameterized by them.

This module previously held a partial, *unused* printer -- nothing under
``fclpy/`` imported it while ``io_write.py`` printed via ``lisp_str``. It is
rewritten in place rather than added alongside, so the count of printers goes
from three (this file, ``io_write._print_with_limits``, and the
``__str__``/``__repr__`` methods) to one (standing rule 3).

Deliberately out of scope, and why
----------------------------------
* **The pretty printer** (``*PRINT-PRETTY*``, ``PPRINT-*``, ``~<~:>``). A
  separate mechanism with its own cluster; this printer always prints in the
  non-pretty style, which is what ``*PRINT-PRETTY*`` NIL asks for and what the
  printer tests bind.
* **``*PRINT-CIRCLE*``**. Needs a shared labelling pass over the object graph.
  Circular structure is still detected here as a depth cutoff rather than
  recursing forever.
* **Bit vectors as ``#*1011``**. A bit vector and a general vector are both a
  Python ``list`` in this implementation, with no ``element-type`` recorded
  anywhere, so the distinction cannot be recovered at print time. Blocked on
  the array object model (plan.md C6/M9); a bit vector prints as ``#(1 0 1 1)``
  until then. Not guessed at from contents -- ``#(0 1)`` would be
  indistinguishable from ``#*01``.
"""

from fractions import Fraction

import fclpy.lisptype as lisptype
from fclpy.lisptype import (
    Character,
    LispString,
    LispSymbol,
    lispCons,
    lispKeyword,
    lispNull,
)

# ---------------------------------------------------------------------------
# Printer control variables
# ---------------------------------------------------------------------------

#: The printer control variables and their ANSI initial values (CLHS Figure
#: 22-1, and the per-variable pages for the initial value of each). These are
#: the values ``printer/printer-control-vars.lsp`` asserts, so the defaults are
#: testable rather than folklore. Note ``*PRINT-RIGHT-MARGIN*`` and
#: ``*PRINT-MISER-WIDTH*`` are NIL, not numbers -- the old PrinterSettings had
#: 80 and 40, which that file's ``print-right-margin.init.1`` catches.
PRINTER_VARIABLES = {
    '*PRINT-ARRAY*': True,
    '*PRINT-BASE*': 10,
    '*PRINT-CASE*': 'UPCASE',
    '*PRINT-CIRCLE*': False,
    '*PRINT-ESCAPE*': True,
    '*PRINT-GENSYM*': True,
    '*PRINT-LENGTH*': None,
    '*PRINT-LEVEL*': None,
    '*PRINT-LINES*': None,
    '*PRINT-MISER-WIDTH*': None,
    '*PRINT-PRETTY*': False,
    '*PRINT-RADIX*': False,
    '*PRINT-READABLY*': False,
    '*PRINT-RIGHT-MARGIN*': None,
}

#: Maps the keyword argument names ``WRITE`` accepts (CLHS 22.3.1) to the
#: control variable each one overrides for the duration of the call. One table
#: rather than a positional signature per function, so ``WRITE``,
#: ``WRITE-TO-STRING`` and ``PRIN1``/``PRINC`` cannot drift apart in which
#: arguments they honour.
WRITE_KEYWORD_VARIABLES = {
    'array': '*PRINT-ARRAY*',
    'base': '*PRINT-BASE*',
    'case': '*PRINT-CASE*',
    'circle': '*PRINT-CIRCLE*',
    'escape': '*PRINT-ESCAPE*',
    'gensym': '*PRINT-GENSYM*',
    'length': '*PRINT-LENGTH*',
    'level': '*PRINT-LEVEL*',
    'lines': '*PRINT-LINES*',
    'miser_width': '*PRINT-MISER-WIDTH*',
    'pretty': '*PRINT-PRETTY*',
    'radix': '*PRINT-RADIX*',
    'readably': '*PRINT-READABLY*',
    'right_margin': '*PRINT-RIGHT-MARGIN*',
}

def _level_exceeded(ctx, depth):
    """True when an aggregate at nesting `depth` must print as ``#``.

    CLHS 22.1.3.4: the object being printed is at level 0, its components at
    level 1, and so on; an aggregate whose *components* would exceed
    ``*PRINT-LEVEL*`` is abbreviated to ``#``. Two details follow from that and
    both were wrong when the check was a single test at the top of the
    dispatcher: the test belongs at the point an aggregate is entered, not
    before every object, because **an atom never prints as ``#``** however deep
    it sits; and it is `>=`, since with ``*PRINT-LEVEL*`` 0 the outermost object
    is already too deep and `(write-to-string '(1 2) :level 0)` is ``"#"``.
    """
    return ctx.level is not None and depth >= ctx.level


#: A cutoff for structure that is circular or deeper than any real program's.
#: Without ``*PRINT-CIRCLE*`` the printer has no way to label a shared
#: substructure, but it must not recurse forever either: an infinite recursion
#: here aborts a whole ANSI run, which is how a printer bug becomes a
#: *measurement* failure across unrelated directories.
MAX_DEPTH = 256


def _false(value):
    """True when `value` is Lisp false.

    NIL reaches Python as three different objects (``None``, the ``NIL``
    singleton, and a ``LispSymbol`` named NIL in some other package) and Python
    ``False`` is a fourth. ``lisptype.is_truthy`` deliberately is not used:
    ``is_truthy(False)`` returns True, so a Python ``False`` stored in a
    control variable would read as *set* (plan.md's ``is_truthy`` landmine).
    """
    if value is None or value is False or value is lisptype.NIL:
        return True
    return isinstance(value, LispSymbol) and value.name == 'NIL'


def _true(value):
    """True when `value` is Lisp true."""
    return not _false(value)


def _control_symbol(name):
    """Intern a control variable's symbol in COMMON-LISP."""
    return lisptype.COMMON_LISP_PACKAGE.intern_symbol(name)


def resolve_control(name):
    """Return the current value of printer control variable `name`.

    Resolution mirrors ``evaluation_core.eval``'s own order for a variable
    reference -- lexical/dynamic binding in the current environment chain
    first, then the symbol's value cell (the cell ``SET``/``SYMBOL-VALUE``/
    ``PROGV`` and a ``LET`` over a declared special all use) -- so a binding
    made by Lisp code is honoured by this Python-side reader.

    Where the evaluator would next fall through to the *function* registry,
    this falls back to the ANSI initial value instead. That fall-through is
    what used to make ``*print-base*`` evaluate to a Python function.
    """
    import fclpy.state as state

    symbol = _control_symbol(name)
    env = getattr(state, 'current_environment', None)
    if env is not None and env.has_variable(symbol):
        return env.find_variable(symbol)
    value = getattr(symbol, 'value', None)
    if value is not None:
        return value
    return PRINTER_VARIABLES[name]


def _as_case_keyword(value):
    """Normalize a ``*PRINT-CASE*`` value to one of UPCASE/DOWNCASE/CAPITALIZE.

    The variable holds a keyword in Lisp, but the same slot has historically
    been set from Python with a bare string, so accept both rather than letting
    an unrecognized value silently select upcase.
    """
    if isinstance(value, LispSymbol):
        value = value.name
    if isinstance(value, LispString):
        value = str(value)
    if isinstance(value, str):
        name = value.upper().lstrip(':')
        if name in ('UPCASE', 'DOWNCASE', 'CAPITALIZE'):
            return name
    raise lisptype.LispTypeError(
        f"*PRINT-CASE* must be :UPCASE, :DOWNCASE or :CAPITALIZE, not {value!r}")


def _as_count(value):
    """Normalize a ``*PRINT-LEVEL*``/``*PRINT-LENGTH*`` value to int or None."""
    if _false(value):
        return None
    if isinstance(value, bool):
        return None
    if isinstance(value, int):
        return value
    return None


class PrintContext:
    """The printer control variables resolved once for one printing operation.

    Resolved once at the top of a print rather than per object: the values
    cannot change during a single printed representation, and walking the
    environment chain per cons cell would make printing a long list quadratic.
    """

    __slots__ = ('escape', 'base', 'radix', 'case', 'level', 'length',
                 'array', 'gensym', 'readably', 'pretty', 'circle')

    def __init__(self, **overrides):
        unknown = set(overrides) - set(WRITE_KEYWORD_VARIABLES)
        if unknown:
            raise lisptype.LispTypeError(
                f"Unknown printer keyword(s): {', '.join(sorted(unknown))}")

        def value_of(keyword):
            variable = WRITE_KEYWORD_VARIABLES[keyword]
            if keyword in overrides and overrides[keyword] is not _UNSUPPLIED:
                return overrides[keyword]
            return resolve_control(variable)

        self.escape = _true(value_of('escape'))
        self.radix = _true(value_of('radix'))
        self.array = _true(value_of('array'))
        self.gensym = _true(value_of('gensym'))
        self.readably = _true(value_of('readably'))
        self.pretty = _true(value_of('pretty'))
        self.circle = _true(value_of('circle'))
        self.case = _as_case_keyword(value_of('case'))
        self.level = _as_count(value_of('level'))
        self.length = _as_count(value_of('length'))

        base = value_of('base')
        if isinstance(base, bool) or not isinstance(base, int) or not 2 <= base <= 36:
            raise lisptype.LispTypeError(
                f"*PRINT-BASE* must be an integer between 2 and 36, not {base!r}")
        self.base = base

    def with_escape(self, escape):
        """A copy of this context with ``*PRINT-ESCAPE*`` forced.

        ``~A`` inside a ``~S``-printed object, and vice versa, must not inherit
        the surrounding escape setting.
        """
        clone = object.__new__(PrintContext)
        for slot in PrintContext.__slots__:
            setattr(clone, slot, getattr(self, slot))
        clone.escape = escape
        return clone


class _Unsupplied:
    """Distinguishes ``:escape nil`` from ``:escape`` not being passed."""

    def __repr__(self):
        return '#<unsupplied>'


_UNSUPPLIED = _Unsupplied()


# ---------------------------------------------------------------------------
# Numbers
# ---------------------------------------------------------------------------

_DIGITS = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'


def integer_digits(value, base):
    """Render `value` in `base` using upper-case digits, sign first.

    Python's ``hex``/``oct``/``bin`` only cover three bases and prefix their
    output, and ``format(n, 'x')`` produces lower-case digits, so neither can
    serve bases 2-36 with the upper-case digits CLHS 22.1.3.1.1 requires
    (``(let ((*print-base* 16)) (prin1 12))`` is ``C``, not ``c``).
    """
    if value == 0:
        return '0'
    negative = value < 0
    magnitude = -value if negative else value
    digits = []
    while magnitude:
        magnitude, remainder = divmod(magnitude, base)
        digits.append(_DIGITS[remainder])
    if negative:
        digits.append('-')
    return ''.join(reversed(digits))


def _radix_prefix(base):
    """The ``#b``/``#o``/``#x``/``#nr`` prefix for `base` (CLHS 22.1.3.1.1)."""
    if base == 2:
        return '#b'
    if base == 8:
        return '#o'
    if base == 16:
        return '#x'
    return f'#{base}r'


def _write_integer(value, ctx):
    """Print an integer honouring ``*PRINT-BASE*`` and ``*PRINT-RADIX*``.

    The radix marker goes *before* the sign (``#b-1``, not ``-#b1``), and base
    ten uses a trailing decimal point rather than a prefix.
    """
    digits = integer_digits(value, ctx.base)
    if not ctx.radix:
        return digits
    if ctx.base == 10:
        return digits + '.'
    return _radix_prefix(ctx.base) + digits


def _write_ratio(value, ctx):
    """Print a ratio as ``numerator/denominator`` in ``*PRINT-BASE*``.

    Python's ``repr`` of a ``fractions.Fraction`` is ``Fraction(1, 2)``, which
    is not readable Lisp -- a ratio is one of the values that used to leak a
    Python representation into Lisp output.
    """
    numerator = integer_digits(value.numerator, ctx.base)
    denominator = integer_digits(value.denominator, ctx.base)
    body = f'{numerator}/{denominator}'
    if not ctx.radix:
        return body
    if ctx.base == 10:
        return '#10r' + body
    return _radix_prefix(ctx.base) + body


def _write_float(value, ctx):
    """Print a float in the ANSI exponential/positional syntax.

    Python renders some floats in ways the Lisp reader would not accept back:
    ``1e+20`` (the ``+`` and the missing ``.0``) and ``inf``/``nan``. A float is
    always printed in base ten -- ``*PRINT-BASE*`` applies only to rationals
    (CLHS 22.1.3.1.1).

    An infinity or a NaN has no standard printed syntax, so it is
    implementation-defined output -- but only when ``*PRINT-READABLY*`` is
    false. Under ``*PRINT-READABLY*`` the printer promises the result can be
    read back, and must signal ``PRINT-NOT-READABLE`` rather than emit
    something no reader accepts (CLHS 22.1.3.13).
    """
    if value != value:
        return _write_unreadable_checked(value, 'NOT-A-NUMBER', ctx)
    if value in (float('inf'), float('-inf')):
        sign = 'NEGATIVE' if value < 0 else 'POSITIVE'
        return _write_unreadable_checked(value, f'{sign}-INFINITY', ctx)
    text = repr(float(value))
    if 'e' in text:
        mantissa, exponent = text.split('e')
        if '.' not in mantissa:
            mantissa += '.0'
        return f'{mantissa}e{int(exponent)}'
    if '.' not in text:
        text += '.0'
    return text


def _write_complex(value, ctx):
    """Print a complex as ``#C(real imaginary)`` (CLHS 22.1.3.1.4)."""
    real = write_object(value.real, ctx)
    imaginary = write_object(value.imag, ctx)
    return f'#C({real} {imaginary})'


# ---------------------------------------------------------------------------
# Characters and strings
# ---------------------------------------------------------------------------

#: Printed names for the characters that have them (CLHS 13.1.7). Kept here so
#: there is one table: ``Character.__repr__``, ``Character.__str__``,
#: ``printer._print_character`` and ``FORMAT``'s ``~C`` each had their own and
#: they disagreed on coverage.
CHARACTER_NAMES = {
    ' ': 'Space',
    '\n': 'Newline',
    '\t': 'Tab',
    '\r': 'Return',
    '\b': 'Backspace',
    '\f': 'Page',
    '\x7f': 'Rubout',
    '\0': 'Null',
    '\x1b': 'Escape',
}


def character_name(char):
    """The printed name of `char` after ``#\\``."""
    if char in CHARACTER_NAMES:
        return CHARACTER_NAMES[char]
    if char.isprintable():
        return char
    return f'U+{ord(char):04X}'


def _write_character(value, ctx):
    """Print a character: ``a`` under PRINC, ``#\\a`` under PRIN1.

    This is the clearest case of the ``__str__``/``__repr__`` split being
    wrong: ``Character.__str__`` also produced ``#\\a``, so ``PRINC`` of a
    character was escaped even though escaping is exactly what ``PRINC`` turns
    off (CLHS 22.1.3.2).
    """
    char = value.char if isinstance(value, Character) else str(value)
    if not ctx.escape:
        return char
    return '#\\' + character_name(char)


def _write_string(value, ctx):
    r"""Print a string, quoting and escaping only under ``*PRINT-ESCAPE*``.

    Only ``"`` and ``\`` are escaped. CLHS 2.4.5 makes backslash a single
    escape character that is included *without interpretation*, so a newline
    inside a string prints as an actual newline, not as ``\n`` -- emitting
    ``\n`` would make the string read back as the two characters ``\`` and
    ``n``.
    """
    text = str(value)
    if not ctx.escape:
        return text
    escaped = text.replace('\\', '\\\\').replace('"', '\\"')
    return f'"{escaped}"'


# ---------------------------------------------------------------------------
# Symbols
# ---------------------------------------------------------------------------

#: Characters that force a symbol name to be printed inside ``|...|``, because
#: the reader would otherwise give them syntactic meaning.
_SYMBOL_ESCAPE_CHARS = set('()\'"`,;|\\#')


def _readtable_case():
    """The current readtable's case sensitivity mode, upper-cased.

    An unrecognized value falls back to ``UPCASE`` -- the ANSI default -- rather
    than being silently treated as ``PRESERVE``, which would make symbol case
    depend on a typo.
    """
    from fclpy.readtable import get_current_readtable
    case = get_current_readtable().readtable_case()
    if isinstance(case, LispSymbol):
        case = case.name
    if isinstance(case, str):
        return case.upper().lstrip(':')
    return 'UPCASE'


def _apply_print_case(name, ctx):
    """Apply ``*PRINT-CASE*`` to `name` under the current ``READTABLE-CASE``.

    CLHS 22.1.3.3.2. ``*PRINT-CASE*`` does not simply recase the name -- which
    characters it governs depends on the readtable:

    * ``:upcase``   -- upper-case characters follow ``*PRINT-CASE*``;
                      lower-case characters print as they are.
    * ``:downcase`` -- the mirror image: lower-case characters follow
                      ``*PRINT-CASE*``, upper-case print as they are.
    * ``:preserve`` -- everything prints as it is; ``*PRINT-CASE*`` is ignored.
    * ``:invert``   -- a name of uniform case is inverted, a mixed-case name
                      printed as is; ``*PRINT-CASE*`` is ignored.

    So ``'|XYZ|`` with a ``:downcase`` readtable prints ``XYZ`` for *every*
    value of ``*PRINT-CASE*``, which is what ``print.symbol.1`` checks.
    """
    readtable_case = _readtable_case()

    if readtable_case == 'PRESERVE':
        return name
    if readtable_case == 'INVERT':
        has_upper = any(c.isupper() for c in name)
        has_lower = any(c.islower() for c in name)
        if has_upper and not has_lower:
            return name.lower()
        if has_lower and not has_upper:
            return name.upper()
        return name

    governed = str.isupper if readtable_case == 'UPCASE' else str.islower

    if ctx.case == 'UPCASE':
        convert = str.upper
    elif ctx.case == 'DOWNCASE':
        convert = str.lower
    else:
        return _capitalize_governed(name, governed)

    return ''.join(convert(c) if governed(c) else c for c in name)


def _capitalize_governed(name, governed):
    """Capitalize each word of `name`, considering only governed characters.

    ``:capitalize`` means the first letter of each word upper case and the rest
    lower, where a word is a run of alphanumerics -- ``str.title`` gets this
    wrong for names containing digits or hyphens.
    """
    out = []
    starting_word = True
    for char in name:
        if not char.isalnum():
            out.append(char)
            starting_word = True
            continue
        if not governed(char):
            out.append(char)
            starting_word = False
            continue
        out.append(char.upper() if starting_word else char.lower())
        starting_word = False
    return ''.join(out)


def _name_needs_escaping(name, ctx):
    """True when `name` must be printed as ``|name|`` to read back as itself."""
    if name == '':
        return True
    if any(c in _SYMBOL_ESCAPE_CHARS or c.isspace() for c in name):
        return True
    # A name that would read as a number must be escaped, or `(prin1 '|123|)`
    # would print 123 and read back as an integer.
    if _looks_like_a_number(name, ctx.base):
        return True
    # Under an upcasing readtable a lower-case character does not survive a
    # read/print round trip unless it is escaped.
    readtable_case = _readtable_case()
    if readtable_case == 'UPCASE' and any(c.islower() for c in name):
        return True
    if readtable_case == 'DOWNCASE' and any(c.isupper() for c in name):
        return True
    return False


def _looks_like_a_number(name, base):
    """True when `name` would be read as a number rather than a symbol."""
    body = name[1:] if name[:1] in '+-' else name
    if body == '':
        return False
    if all(c in _DIGITS[:base] for c in body):
        return True
    if all(c in _DIGITS[:base] for c in body.rstrip('.')) and body.endswith('.'):
        return True
    try:
        float(name)
        return True
    except ValueError:
        return False


def current_package():
    """The current package: the value of ``*PACKAGE*``.

    ``*PACKAGE*`` is the authority and ``state.current_package`` only mirrors
    it, so the variable is read first. Reading only the mirror is wrong in
    exactly the case that matters here -- the mirror is unset until something
    assigns it, and a symbol in the current package would then acquire a
    spurious ``COMMON-LISP-USER::`` prefix.
    """
    import fclpy.state as state

    symbol = _control_symbol('*PACKAGE*')
    env = getattr(state, 'current_environment', None)
    if env is not None and env.has_variable(symbol):
        package = env.find_variable(symbol)
        if isinstance(package, lisptype.Package):
            return package
    package = getattr(symbol, 'value', None)
    if isinstance(package, lisptype.Package):
        return package
    package = getattr(state, 'current_package', None)
    if isinstance(package, lisptype.Package):
        return package
    return lisptype.COMMON_LISP_USER_PACKAGE


def _package_prefix(symbol, ctx):
    """The ``PKG:``/``PKG::``/``#:`` prefix for `symbol`, or ``''``.

    CLHS 22.1.3.3. A symbol needs no prefix when it is accessible in the
    current package; otherwise the prefix records whether it is exported
    (single colon) or internal (double colon).
    """
    package = getattr(symbol, 'package', None)
    if package is None:
        # Uninterned. `*PRINT-GENSYM*` decides whether the reader is told so.
        return '#:' if ctx.gensym else ''
    if package is lisptype.KEYWORD_PACKAGE or isinstance(symbol, lispKeyword):
        return ':'

    if _accessible_in(symbol, current_package()):
        return ''

    prefix = _apply_print_case(package.name, ctx)
    external = getattr(package, 'external_symbols', None)
    is_external = bool(external) and symbol.name in external
    return f'{prefix}:' if is_external else f'{prefix}::'


def _accessible_in(symbol, package):
    """True when `symbol` can be named without a package prefix from `package`.

    Delegates to ``Package.find_symbol``, which already implements the
    present/inherited distinction CLHS 22.1.3.3 needs -- including resolving
    the *names* this package model stores in ``use_packages`` alongside actual
    ``Package`` objects. Re-deriving accessibility here would be a second
    package-lookup mechanism that could disagree with the one ``FIND-SYMBOL``
    reports.

    The identity test matters: a *different* symbol of the same name being
    accessible is exactly when a prefix is required.
    """
    if getattr(symbol, 'package', None) is package:
        return True
    found, status = package.find_symbol(symbol.name)
    return found is symbol and status is not None


def _write_symbol(value, ctx):
    """Print a symbol, with package prefix and case conversion under escape.

    Without ``*PRINT-ESCAPE*`` a symbol is just its name recased -- no package
    prefix, no ``|...|`` (CLHS 22.1.3.3), which is why ``(princ :foo)`` is
    ``FOO`` and not ``:FOO``.
    """
    if value is lisptype.T or (isinstance(value, LispSymbol) and value.name == 'T'
                               and getattr(value, 'package', None) is lisptype.COMMON_LISP_PACKAGE):
        return _apply_print_case('T', ctx)

    name = value.name
    if not ctx.escape:
        return _apply_print_case(name, ctx)

    prefix = _package_prefix(value, ctx)
    if _name_needs_escaping(name, ctx):
        body = '|' + name.replace('\\', '\\\\').replace('|', '\\|') + '|'
    else:
        body = _apply_print_case(name, ctx)
    return prefix + body


# ---------------------------------------------------------------------------
# Aggregates
# ---------------------------------------------------------------------------

def _write_cons(value, ctx, depth):
    """Print a list, honouring ``*PRINT-LEVEL*``, ``*PRINT-LENGTH*`` and dots.

    ``*PRINT-LENGTH*`` elides with ``...`` after that many elements, and a
    non-list tail is printed after a dot (CLHS 22.1.3.5).
    """
    if _level_exceeded(ctx, depth):
        return '#'
    parts = []
    current = value
    count = 0
    while isinstance(current, lispCons):
        if ctx.length is not None and count >= ctx.length:
            parts.append('...')
            current = lisptype.NIL
            break
        parts.append(_write(current.car, ctx, depth + 1))
        count += 1
        current = current.cdr
        if current is None or isinstance(current, lispNull):
            current = lisptype.NIL
            break
        if not isinstance(current, lispCons):
            parts.append('.')
            parts.append(_write(current, ctx, depth + 1))
            break
    return '(' + ' '.join(parts) + ')'


def _vector_elements(value):
    """The live elements of any of this implementation's vector shapes.

    A Lisp vector is a Python ``list`` (from ``MAKE-ARRAY``/``VECTOR``) or an
    ``AdjustableVector`` (what the reader returns for ``#(...)``) -- two
    representations of one type, which is why ``#(1 2 3)`` and ``(vector 1 2 3)``
    used to print differently. A fill pointer bounds the printed elements.
    """
    from fclpy.lispfunc.vectors import AdjustableVector

    if isinstance(value, AdjustableVector):
        limit = value.fill_pointer
        data = list(value.data)
        return data if limit is None else data[:limit]
    return list(value)


def _write_vector(value, ctx, depth):
    """Print a vector as ``#(...)``, or as ``#<...>`` when ``*PRINT-ARRAY*`` is NIL.

    A Python ``list`` is a *vector* here, not a list; printing it as ``(1 2 3)``
    -- which is what ``str()`` did -- made every vector read back as a cons.
    """
    if _level_exceeded(ctx, depth):
        return '#'
    if not ctx.array:
        return _unreadable(value, 'VECTOR')
    elements = _vector_elements(value)
    parts = []
    for index, element in enumerate(elements):
        if ctx.length is not None and index >= ctx.length:
            parts.append('...')
            break
        parts.append(_write(element, ctx, depth + 1))
    return '#(' + ' '.join(parts) + ')'


def _write_array(value, ctx, depth):
    """Print a multi-dimensional array as ``#rankA(nested lists)`` (CLHS 22.1.3.4).

    ``Array.__repr__`` produced ``#(ARRAY (2, 2))`` -- a Python tuple's repr
    embedded in what claimed to be Lisp syntax, and no element contents.
    """
    if _level_exceeded(ctx, depth):
        return '#'
    if not ctx.array:
        return _unreadable(value, 'ARRAY')

    def nested(indices, dimension):
        if dimension == value.rank:
            # An element. `_write` applies *PRINT-LEVEL* to it if it is itself
            # an aggregate; the check must not happen here, because an atom is
            # never abbreviated to `#` however deep it sits.
            return _write(value[tuple(indices)], ctx, depth + dimension)
        # Each dimension of an array is one level, so a rank-2 array's elements
        # are two levels below the array itself.
        if _level_exceeded(ctx, depth + dimension):
            return '#'
        parts = []
        for index in range(value.dimensions[dimension]):
            if ctx.length is not None and index >= ctx.length:
                parts.append('...')
                break
            parts.append(nested(indices + [index], dimension + 1))
        return '(' + ' '.join(parts) + ')'

    # A zero dimension needs no special case: `range(0)` is empty, so a
    # dimension of length 0 yields `()` and the shape still prints -- e.g. a
    # 2x0 array as `#2A(() ())`.
    return f'#{value.rank}A' + nested([], 0)


def _write_structure(value, ctx, depth):
    """Print a structure instance as ``#S(NAME :SLOT value ...)`` (CLHS 22.1.3.10).

    DEFSTRUCT creates a fresh Python class per structure, so ``isinstance``
    cannot recognise one; the marker attribute is what the evaluator already
    tests for.
    """
    if _level_exceeded(ctx, depth):
        return '#'
    parts = [_apply_print_case(str(value._struct_type), ctx)]
    for slot, slot_value in value._slots.items():
        parts.append(':' + _apply_print_case(str(slot), ctx))
        parts.append(_write(slot_value, ctx, depth + 1))
    return '#S(' + ' '.join(parts) + ')'


def _write_hash_table_dict(value, ctx):
    """Print the dict-based hash table as ``#<HASH-TABLE ...>``.

    ``MAKE-HASH-TABLE`` returns a ``HashTableDict``, whose test and sizing are
    attributes rather than entries, so every key in it is a real key.
    """
    test = getattr(value, 'test', 'EQL')
    return f'#<HASH-TABLE :TEST {test} :COUNT {len(value)}>'


def _unreadable(value, kind):
    """An ``#<...>`` representation for an object with no readable syntax.

    CLHS 22.1.3.13. ``*PRINT-READABLY*`` promises that whatever is printed can
    be read back, so printing an unreadable object under it must signal
    ``PRINT-NOT-READABLE`` rather than quietly emit ``#<...>`` -- which no
    reader accepts, making the promise false (standing rule 4).
    """
    return f'#<{kind} {id(value):X}>'


def _write_unreadable_checked(value, kind, ctx):
    """`_unreadable`, but honouring ``*PRINT-READABLY*``."""
    if ctx.readably:
        condition = lisptype.PrintNotReadable(
            object=value,
            message=f"Cannot print {kind} readably: {_unreadable(value, kind)}")
        from fclpy.lispfunc.evaluation_core import ConditionException
        raise ConditionException(condition, recoverable=False)
    return _unreadable(value, kind)


# ---------------------------------------------------------------------------
# The dispatcher
# ---------------------------------------------------------------------------

def _write(value, ctx, depth):
    """Print `value` at nesting `depth`.

    Dispatch order is significant. ``lispKeyword`` before ``LispSymbol``, and
    ``bool`` before ``int``, because each is a subclass of the next and Python
    would otherwise take the wrong branch -- the same class of mistake as
    ``isinstance(x, str)`` standing in for "is a string" (plan.md finding M).
    """
    if depth > MAX_DEPTH:
        # Circular or absurdly deep. Report it rather than overflowing the
        # stack: an unhandled RecursionError here kills an entire ANSI run.
        return '...'

    # NIL, in each of the forms it takes.
    if value is None or value is lisptype.NIL or isinstance(value, lispNull):
        return _apply_print_case('NIL', ctx)
    if value is lisptype.T:
        return _apply_print_case('T', ctx)
    if value is True:
        return _apply_print_case('T', ctx)
    if value is False:
        return _apply_print_case('NIL', ctx)

    if isinstance(value, lispKeyword):
        return _write_symbol(value, ctx)
    if isinstance(value, LispSymbol):
        return _write_symbol(value, ctx)

    if isinstance(value, Character):
        return _write_character(value, ctx)
    if isinstance(value, LispString):
        return _write_string(value, ctx)
    if isinstance(value, str):
        # A bare Python `str` is a string here. A length-1 `str` is *also* used
        # as a character in places; that ambiguity belongs to the string
        # representation split (plan.md finding I / M9), and guessing
        # "length 1 means character" here would print `(string #\a)` as `#\a`.
        return _write_string(value, ctx)

    if isinstance(value, int):
        return _write_integer(value, ctx)
    if isinstance(value, Fraction):
        return _write_ratio(value, ctx)
    if isinstance(value, float):
        return _write_float(value, ctx)
    if isinstance(value, complex):
        return _write_complex(value, ctx)

    if isinstance(value, lispCons):
        return _write_cons(value, ctx, depth)

    if hasattr(value, '_struct_type') and hasattr(value, '_slots'):
        return _write_structure(value, ctx, depth)

    from fclpy.lispfunc.vectors import AdjustableVector, Array
    if isinstance(value, Array):
        return _write_array(value, ctx, depth)
    if isinstance(value, (AdjustableVector, list, tuple)):
        return _write_vector(value, ctx, depth)

    if isinstance(value, dict):
        return _write_hash_table_dict(value, ctx)

    if isinstance(value, lisptype.Package):
        return f'#<PACKAGE {value.name}>'

    from fclpy.lispfunc.pathnames import Pathname
    if isinstance(value, Pathname):
        return '#P' + _write_string(str(value.original), ctx.with_escape(True))

    if isinstance(value, lisptype.Condition):
        # A condition's printed representation is its report (CLHS 9.1.3), and
        # under escape the type is named too.
        if ctx.escape:
            type_name = type(value).__name__.upper()
            return f'#<{type_name} {value.message}>'
        from fclpy.lispfunc.evaluation_conditions import condition_report_text
        report = condition_report_text(value)
        return str(value.message) if report is None else report

    from fclpy.classes import LispClass, LispInstance
    if isinstance(value, LispInstance):
        return _write_unreadable_checked(
            value, _apply_print_case(value.lisp_class.name.name, ctx), ctx)
    if isinstance(value, LispClass):
        return f'#<STANDARD-CLASS {_apply_print_case(value.name.name, ctx)}>'

    if isinstance(value, type) and issubclass(value, lisptype.Condition):
        # FIND-CLASS returns the raw Python class for a condition type
        # (built-in or DEFINE-CONDITION-created) rather than a CLOS
        # `LispClass` -- see `classes.find_class_fn` -- so without this branch
        # it fell through to the generic `callable(value)` case below and
        # printed as though it were a function, since classes are callable
        # too (calling one constructs an instance).
        return f'#<STANDARD-CLASS {_apply_print_case(value.__name__, ctx)}>'

    if callable(value):
        name = getattr(value, '__name__', None) or 'ANONYMOUS'
        return _write_unreadable_checked(value, f'FUNCTION {name}', ctx)

    return _write_unreadable_checked(value, type(value).__name__.upper(), ctx)


def write_object(value, ctx=None, **overrides):
    """Return the printed representation of `value` as a Python string.

    This is the single entry point. `ctx` reuses an already-resolved
    :class:`PrintContext` (so a recursive call does not re-read the control
    variables); `overrides` name any of :data:`WRITE_KEYWORD_VARIABLES`.
    """
    if ctx is None:
        ctx = PrintContext(**overrides)
    elif overrides:
        raise lisptype.LispError(
            "write_object: pass either a PrintContext or keyword overrides")
    return _write(value, ctx, 0)


def prin1_to_string(value):
    """The escaped printed representation -- ``*PRINT-ESCAPE*`` true."""
    return write_object(value, escape=True)


def princ_to_string(value):
    """The unescaped printed representation -- ``*PRINT-ESCAPE*`` false.

    CLHS 22.1.3.2: ``PRINC`` is ``PRIN1`` with ``*PRINT-ESCAPE*`` bound to NIL
    (and ``*PRINT-READABLY*`` to NIL), not a separate representation.
    """
    return write_object(value, escape=False, readably=False)


# Backwards-compatible aliases. This module's previous `prin1`/`princ`/
# `print_object` returned a string rather than printing, and were imported only
# by tests; keeping the names avoids breaking those imports while the real
# entry points are the CL functions in `lispfunc/io_write.py`.
prin1 = prin1_to_string
princ = princ_to_string


def print_object(value, escape=True):
    """Deprecated alias for :func:`write_object`."""
    return write_object(value, escape=escape)
