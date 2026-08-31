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

import math
import re
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

#: How many aggregates one printing operation may enter. Cycle detection alone
#: does not bound the work: with cycles cut, a cons *graph* still enumerates its
#: simple paths, of which a twenty-node out-degree-two graph has exponentially
#: many -- `print.cons.random.2` builds exactly that. High enough that no real
#: structure reaches it, low enough that no graph can outrun it.
PRINT_BUDGET = 100_000


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
                 'array', 'gensym', 'readably', 'pretty', 'circle',
                 'in_progress', 'budget', 'circle_map', 'next_label',
                 'label_seen')

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

        # Circularity state, one per printing operation -- see `_in_progress`.
        # `budget` is a one-element list so that `with_escape`'s clone shares
        # the same counter by reference rather than getting a fresh one.
        self.in_progress = set()
        self.budget = [PRINT_BUDGET]

        # `*PRINT-CIRCLE*` labelling. `circle_map` is `id(obj) -> label` for
        # every aggregate that is shared or part of a cycle; `next_label` is
        # the next free label number. `label_seen` records which labels have
        # already been printed (so `#N=` is emitted only the first time the
        # object is seen, and `#N#` on every subsequent visit). The map is
        # populated by `_compute_circle_map` when the context is built and
        # *only* when `circle` is true; the print path then consults it.
        self.circle_map = {}
        self.next_label = 1
        self.label_seen = set()

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

        # CLHS 22.1.3: when `*PRINT-READABLY*` is true, printing proceeds "as
        # if `*print-escape*`, `*print-array*`, and `*print-gensym*` were true,
        # and as if `*print-length*`, `*print-level*`, and `*print-lines*` were
        # false". This is not a convenience -- those six variables are the only
        # way the printer can produce output that does not read back, so
        # honouring them under `:readably t` makes the *promise of readability
        # itself* unsatisfiable. It is also why `randomly-check-readability`
        # (which binds `*print-readably*` T and then randomizes all six) was
        # failing across `printer/`: with `*print-level*` 0 the printer
        # answered `"#"` for every object, and reading that back is an
        # end-of-file, not the object.
        #
        # Additionally, when `*PRINT-READABLY*` is true and `*PRINT-BASE*` is not 10,
        # `*PRINT-RADIX*` must be true. Otherwise, numbers like 5 printed as "101" in
        # base 2 cannot be read back (they would be read as decimal 101, not binary 5).
        # This is implicit in the readability contract: output that cannot be read
        # back violates the contract regardless of whether CLHS explicitly names it.
        #
        # The remaining controls are unaffected by design: `*print-case*`,
        # `*print-circle*` and `*print-pretty*` cannot make output unreadable,
        # and the same test randomizes them and expects the round trip to hold.
        if self.readably:
            self.escape = True
            self.array = True
            self.gensym = True
            self.level = None
            self.length = None

        base = value_of('base')
        if isinstance(base, bool) or not isinstance(base, int) or not 2 <= base <= 36:
            raise lisptype.LispTypeError(
                f"*PRINT-BASE* must be an integer between 2 and 36, not {base!r}")
        self.base = base

        # If readably is true and base is not 10, force radix to true.
        # Numbers in non-base-10 without radix markers cannot be read back.
        if self.readably and self.base != 10:
            self.radix = True

        # If readably is true and base is 10, force radix to false.
        # The base-10 radix marker (a trailing decimal point) makes integers
        # unreadable: they read back as floats, violating the promise of
        # *PRINT-READABLY*. An integer 1 printed with radix in base 10 becomes
        # "1.", which reads back as 1.0 (a float), not 1 (an integer).
        if self.readably and self.base == 10:
            self.radix = False

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
    """Distinguishes ``:escape nil`` from ``:escape` not being passed."""

    def __repr__(self):
        return '#<unsupplied>'


_UNSUPPLIED = _Unsupplied()


# ---------------------------------------------------------------------------
# *PRINT-CIRCLE* label assignment
# ---------------------------------------------------------------------------

def _aggregate_pieces(value):
    """The sub-objects of an aggregate that the circle pre-pass must traverse.

    Returns an iterable of objects to recurse into, or `None` for objects that
    are not aggregates (atoms, strings, characters, numbers, etc.). This is the
    one place a type decides "is this an aggregate" for the purposes of
    ``*PRINT-CIRCLE*`` -- keeping it here means the print dispatcher and the
    pre-pass cannot disagree on what counts.
    """
    if isinstance(value, lispCons):
        return (value.car, value.cdr)
    if isinstance(value, (list, tuple)):
        return value
    from fclpy.lispfunc.arrays import LispArray
    if isinstance(value, LispArray):
        # Walk every element the array exposes through `__getitem__`. The
        # `range(total_size)` formulation was wrong for fill-pointered and
        # displaced arrays: their `len()` is the live element count, not
        # the product of dimensions, and the two diverge for those
        # representations (`lispfunc/arrays.py`'s `LispArray.__getitem__`
        # bounds the index on `len(self)`).
        return [value[i] for i in range(len(value))]
    from fclpy.classes import LispInstance
    if isinstance(value, LispInstance):
        return list(value.slot_values.values())
    return None


def _is_aggregate(value):
    """True when `value` is a cons/vector/array/structure -- an object whose
    body the printer walks and emits a matching ``(``...``)`` for.

    The aggregate cases prepend the ``#N=`` label themselves; atoms do not
    need a body-walk, so the prefix is added here in `_write` for the
    shared-atom case. Without the split, a shared aggregate that contained
    only atoms would get the label twice (once from `_write` for its body,
    once from `_write_cons` for its parenthesised form).
    """
    if isinstance(value, lispCons):
        return True
    if isinstance(value, (list, tuple)):
        return True
    from fclpy.lispfunc.arrays import LispArray
    if isinstance(value, LispArray):
        return True
    from fclpy.classes import LispInstance
    if isinstance(value, LispInstance):
        return True
    return False


def _compute_circle_map(value, ctx):
    """Populate `ctx.circle_map` with labels for every shared/cyclic aggregate.

    The standard ``*PRINT-CIRCLE*`` algorithm, in its linear form: one walk
    of the object graph that counts the *references* to each object. An
    object gets a label when a second reference to it is seen -- whether
    that second reference comes from a different parent (a DAG) or the same
    one (``(cons x x)``, whose car and cdr are two references to ``x``) --
    or when a walk meets an object already on its own path (a cycle; the
    back-edge target is the entry that carries the ``#N=``). Atoms are
    labelled the same way, per reference, which is what makes
    ``print.cons.5``/``.6``'s shared gensyms print as ``#1=#:X . #1#``.

    The previous version of this pass tracked the *set of parents* that
    reached each aggregate and re-walked every reachable subtree once per
    incoming path. Both halves were wrong:

    - **Per-parent sets missed same-parent sharing.** A cons referenced by
      the car and the cdr of one parent -- the shape ``print.cons.random.2``
      builds at random -- has a one-element parent set and so got no label;
      the print then re-walked it at every occurrence, producing output that
      was exponential in the graph's simple paths, elided with ``...`` where
      the re-walk re-entered an ancestor, and could not be read back.
    - **Re-walking made the pass itself exponential.** It ran on the same
      `ctx.budget` the print spends, so a random 20-cons graph could drain
      the budget before the print even started, leaving later cycle entries
      unlabelled.

    Counting references instead of parents needs no re-walk: a second
    reference is counted and labelled without descending again, which keeps
    the walk linear in the number of *objects* (each unique aggregate is
    entered once) and keeps the inner conses of a shared aggregate from
    looking "shared" merely because their parent is.

    The cdr chain of a cons is walked iteratively, mirroring `_write_cons`,
    so list length does not become recursion depth; only the cars recurse.
    The pass is still bounded by `ctx.budget`, as a belt against a graph so
    large the walk itself matters.
    """
    counts = {}  # `id(obj) -> reference count`, aggregates and atoms alike
    on_path = set()
    map_ = ctx.circle_map
    next_label = [1]

    def label(key):
        if key not in map_:
            map_[key] = next_label[0]
            next_label[0] += 1

    def note(key):
        """One more reference to an already-seen object: shared, so labelled."""
        counts[key] = counts.get(key, 0) + 1
        if counts[key] > 1:
            label(key)

    def visit(obj):
        if ctx.budget[0] <= 0:
            return
        if obj is None or obj is lisptype.NIL:
            return
        key = id(obj)
        # Atoms and non-aggregates: a per-reference count. A gensym that is
        # both the car and the cdr of one cons (e.g. ``(cons s s)`` in
        # `print.cons.5`) is "shared" -- it appears twice in the printed
        # output -- so the count fires on two references from one parent.
        if isinstance(obj, (bool, int, float, complex, str, bytes, Character,
                            LispString, LispSymbol, lispKeyword, type, Fraction)):
            note(key)
            return
        pieces = _aggregate_pieces(obj)
        if pieces is None:
            return
        if key in on_path:
            # A back-edge: `obj` is an ancestor of the reference, so it is
            # part of a cycle and the entry that carries the `#N=` label.
            label(key)
            return
        if key in counts:
            # Already walked once: count this reference, do not descend
            # again. Descending is what made the pass exponential and made
            # the inner conses of a shared aggregate look shared.
            note(key)
            return
        counts[key] = 1
        ctx.budget[0] -= 1
        on_path.add(key)
        try:
            if not isinstance(obj, lispCons):
                for sub in pieces:
                    if sub is None or sub is lisptype.NIL:
                        continue
                    visit(sub)
                return
            # A cons walks its own cdr chain iteratively (mirroring
            # `_write_cons`), recursing into cars only.
            current = obj
            walked = []
            try:
                while isinstance(current, lispCons) and ctx.budget[0] > 0:
                    car, cdr = current.car, current.cdr
                    if car is not None and car is not lisptype.NIL:
                        visit(car)
                    if cdr is None or isinstance(cdr, lispNull):
                        break
                    if not isinstance(cdr, lispCons):
                        visit(cdr)
                        break
                    ckey = id(cdr)
                    if ckey in on_path:
                        # Back-edge closing the chain -- the self-cycle
                        # `(setf (cdr a) a)` reaches here too.
                        label(ckey)
                        break
                    if ckey in counts:
                        note(ckey)
                        break
                    counts[ckey] = 1
                    ctx.budget[0] -= 1
                    on_path.add(ckey)
                    walked.append(ckey)
                    current = cdr
            finally:
                for walked_key in walked:
                    on_path.discard(walked_key)
        finally:
            on_path.discard(key)

    visit(value)
    ctx.next_label = next_label[0]


def _circle_prefix(value, ctx):
    """The ``#N=`` or ``#N#`` to emit for `value`, or ``''``.

    For a labelled value, emits ``#N=`` the first time it is seen and
    ``#N#`` on every subsequent visit; for an unlabelled value emits
    nothing. Tracks seen-labels on the context so the first-vs-subsequent
    decision is one integer set lookup rather than re-walking the graph.
    """
    if not ctx.circle:
        return ''
    label = ctx.circle_map.get(id(value))
    if label is None:
        return ''
    if label in ctx.label_seen:
        return f'#{label}#'
    ctx.label_seen.add(label)
    return f'#{label}='


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


def float_shortest_digits(mag):
    """The shortest decimal digit string that reads back as the float `mag`
    (> 0), as `(digits, decpt)` where `mag == 0.<digits> * 10**decpt` --
    e.g. `123.456` is `('123456', 3)` and `0.001` is `('1', -2)`.

    Built from `repr(mag)`, which Python already renders as the shortest
    round-tripping decimal; this just re-derives the digit string and
    decimal-point position independently of whether Python chose plain or
    exponential notation, so a caller can place the point wherever *it*
    wants (this printer's own exponential threshold below, and `FORMAT`'s
    `~E`/`~F`, which slice this same digit string at a `k`- or `w`-chosen
    position rather than Python's).
    """
    s = repr(mag)
    if 'e' in s or 'E' in s:
        mantissa, exp_s = re.split('[eE]', s)
        exp = int(exp_s)
    else:
        mantissa, exp = s, 0
    if '.' in mantissa:
        int_part, frac_part = mantissa.split('.')
    else:
        int_part, frac_part = mantissa, ''
    digits = int_part + frac_part
    decpt = len(int_part) + exp
    stripped = digits.lstrip('0')
    decpt -= (len(digits) - len(stripped))
    digits = stripped.rstrip('0')
    if digits == '':
        digits = '0'
        decpt = 1
    return digits, decpt


def _write_float(value, ctx):
    """Print a float in the ANSI exponential/positional syntax.

    A float is always printed in base ten -- ``*PRINT-BASE*`` applies only
    to rationals (CLHS 22.1.3.1.1). Notation is chosen from the value's
    magnitude rather than by asking Python's ``repr`` to pick: ``repr``
    switches to exponential notation outside roughly ``[1e-5, 1e17)``,
    but CLHS's own worked examples (and the ansi-test suite, e.g.
    ``format.e.1``/``.2``/``.26``, which compare ``FORMAT``'s ``~E``
    against ``PRIN1`` for values chosen specifically to fall outside it)
    assume the conventional ``[1e-3, 1e7)`` -- so a magnitude like ``1e-4``
    must already print as ``1.e-4``, a boundary Python's own ``repr``
    would not cross for another order of magnitude.

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
    if value == 0.0:
        return '-0.0' if math.copysign(1.0, value) < 0 else '0.0'
    sign = '-' if value < 0 else ''
    mag = abs(value)
    digits, decpt = float_shortest_digits(mag)
    exponent = decpt - 1
    if -3 <= exponent < 7:
        if decpt <= 0:
            int_part, frac_part = '0', '0' * (-decpt) + digits
        elif decpt >= len(digits):
            int_part, frac_part = digits + '0' * (decpt - len(digits)), '0'
        else:
            int_part, frac_part = digits[:decpt], digits[decpt:]
        return f'{sign}{int_part}.{frac_part}'
    # CLHS 22.1.3.1.3's exponent-marker float syntax is `{digit}+
    # [decimal-point {digit}*] exponent` -- unlike the no-exponent form,
    # digits after the point are optional, so a single-digit mantissa
    # (e.g. `1e+300`) only needs the point added (`1.e+300`), not a
    # manufactured `.0`.
    mantissa = f'{digits[0]}.{digits[1:]}'
    return f'{sign}{mantissa}e{exponent}'


def _write_complex(value, ctx):
    """Print a complex as ``#C(real imaginary)`` (CLHS 22.1.3.1.4)."""
    real = write_object(value.real, ctx)
    imaginary = write_object(value.imag, ctx)
    return f'#C({real} {imaginary})'


# ---------------------------------------------------------------------------
# Characters and strings
# ---------------------------------------------------------------------------

def character_name(char):
    """The printed name of `char` after ``#\\``.

    ``isprintable()`` takes priority over the name: `printer/print-
    characters.lsp`'s `PRINT.CHAR.3`/`.4`/`.9` (under ``*print-readably*``
    NIL, CLHS 22.1.3.6's non-readable mode, where the printer is explicitly
    *not* required to produce something that reads back) print Space as a
    bare space -- `"#\\ "` -- not `#\\Space`, and `.3`'s loop over
    `+base-chars+` explicitly excludes `#\\Space` from the "named form
    required" check. Newline is the one exception CLHS still names
    explicitly (`.5`/`.9`), so it keeps its standard name even though
    Python considers it non-printable anyway.

    A non-printable character's name comes from CHAR-NAME
    (`lispfunc/characters.py`, the one home of the character-name
    concept) -- not from a second table here. Two tables was how
    `format.s.8` came to compare `#\\~:c`'s "Bell" against the printer's
    "U+0007" for the same character and fail. The ``U+XXXX`` fallback is
    dead code while CHAR-NAME answers a name for every non-printable
    character, and stays as the belt for a character CHAR-NAME does not
    know.
    """
    if char.isprintable():
        return char
    from fclpy.lispfunc.characters import char_name
    name = char_name(Character(char))
    if name is not None:
        return name
    # Fallback for non-printable characters without standard names is
    # implementation-defined; we use Unicode hex notation. This cannot be read
    # back, so it violates *PRINT-READABLY*, which is handled in _write_character.
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
    # CLHS 22.1.3.3: a character *always* has a readable spelling -- #\Name
    # when named, #\x when graphic, #\U+XXXX otherwise -- so unlike other
    # objects a character never signals PRINT-NOT-READABLE under
    # *PRINT-READABLY* (PRINT.CHAR.2/.8/.9). The #\U+XXXX spelling is only
    # as readable as the reader's #\ name syntax: fclpy's reader does not
    # accept #\U+XXXX yet (reported -- readtable.py's #\ handler), which is
    # what still fails the char.8/.9 round trips.
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
#: the reader would otherwise give them syntactic meaning. ``:`` is one of
#: them -- an unescaped colon in a token is a package marker, so
#: `(write (make-symbol ":") :readably t)` printed ``#::`` and the round trip
#: died in the reader (`read-symbol.10`); escaped as ``#:|:|`` it reads back.
_SYMBOL_ESCAPE_CHARS = set('()\'"`,;|\\#:')


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
    # CLHS 22.1.3.3.2: under *PRINT-READABLY* the printed form must read
    # back the same, and the read-back may run through a *different*
    # readtable -- ansi-test's randomly-check-readability prints under a
    # :downcase/:invert/:preserve table and reads back through
    # (copy-readtable nil), the standard :upcase one. A name whose printed
    # spelling the :upcase reader would case-convert cannot survive
    # unescaped: printing |a| as "a" under :downcase/:invert/:preserve read
    # back as A, the wrong symbol (PRINT.SYMBOL.RANDOM.1-4). The check is
    # the :upcase reader's own per-character rule (readtable.py's
    # convert_case_chars) applied to the *printed* spelling against the
    # original name. Only the non-:upcase modes need this: under :upcase
    # the two rules above already escape exactly the names that would not
    # survive, and non-readably printing is allowed to be unreadable.
    if ctx.readably and readtable_case != 'UPCASE':
        printed = _apply_print_case(name, ctx)
        if ''.join(c.upper() for c in printed) != name:
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

    Delegates to `state.current_package_value`, the one resolver -- the
    printer's copy of this decision was the only correct one of the five that
    existed, so it became that function.
    """
    import fclpy.state as state
    return state.current_package_value()


def _package_is_live(package):
    """True while `package` is still the registry entry its name denotes.

    DELETE-PACKAGE removes a package's names from the registry but leaves
    `symbol-package` slots pointing at the dead object (the package system's
    defect, reported separately): a symbol whose home package is dead prints
    a prefix naming a package `(find-package ...)` no longer answers, so the
    printed form could not be read back. PRINT.SYMBOL.PREFIX.8 deletes the
    home package and requires `#:ABC`.
    """
    from fclpy.lisptype import find_package

    return find_package(package.name) is package


def _package_prefix(symbol, ctx):
    """The ``PKG:``/``PKG::``/``#:`` prefix for `symbol`, or ``''``.

    CLHS 22.1.3.3. A symbol needs no prefix when it is accessible in the
    current package; otherwise the prefix records whether it is exported
    (single colon) or internal (double colon).
    """
    package = getattr(symbol, 'package', None)
    if package is not None and not _package_is_live(package):
        # The home package was deleted; the symbol is uninterned as far as
        # any reader can tell.
        package = None
    if package is None:
        # Uninterned. `*PRINT-GENSYM*` decides whether the reader is told so.
        return '#:' if ctx.gensym else ''
    if package is lisptype.KEYWORD_PACKAGE or isinstance(symbol, lispKeyword):
        return ':'

    if _accessible_in(symbol, current_package()):
        return ''

    prefix = _apply_print_case(package.name, ctx)
    # The readable rule covers the package name too (CLHS 22.1.3.3.2): the
    # read-back -- a standard :upcase reader -- must find the same package,
    # so a prefix spelling that reader would case-convert needs |...| (the
    # reader accepts a multiple-escaped package token: |cl-user|::foo).
    if (ctx.readably and _readtable_case() != 'UPCASE'
            and ''.join(c.upper() for c in prefix) != package.name):
        prefix = '|' + package.name.replace('\\', '\\\\').replace('|', '\\|') + '|'
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


def _bool_atom_name(name, ctx):
    """Print the name of the canonical ``NIL`` or ``T`` object.

    These two symbols used to be short-circuited straight to
    ``_apply_print_case`` -- a copy of the escape logic that forgot the
    number-syntax half of it. CLHS 22.1.3.3's escape rule is about the
    *name*: with printer escaping enabled and ``*PRINT-BASE*`` >= 24, the
    names NIL and T are potential-number syntax (2.3.1.1), bare ``NIL``
    reads back as the integer #24rNIL under *READ-BASE* 24 (2.3.4), and
    ``randomly-check-readability`` draws exactly that. Routing them through
    the same ``_name_needs_escaping`` check every other symbol goes through
    is the fix; the bars preserve the name, which is never case-converted.
    """
    if ctx.escape and _name_needs_escaping(name, ctx):
        return '|' + name + '|'
    return _apply_print_case(name, ctx)


def _write_symbol(value, ctx):
    """Print a symbol, with package prefix and case conversion under escape.

    Without ``*PRINT-ESCAPE*`` a symbol is just its name recased -- no package
    prefix, no ``|...|`` (CLHS 22.1.3.3), which is why ``(princ :foo)`` is
    ``FOO`` and not ``:FOO``.
    """
    if value is lisptype.T or (isinstance(value, LispSymbol) and value.name == 'T'
                               and getattr(value, 'package', None) is lisptype.COMMON_LISP_PACKAGE):
        return _bool_atom_name('T', ctx)

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

def _in_progress(value, ctx, writer, depth):
    """Print an aggregate, refusing to re-enter one already being printed.

    **The other half of the circularity cutoff.** ``MAX_DEPTH`` bounds
    *recursion*, and `_write_cons` now bounds the cdr *walk*, but neither
    bounds a cycle that runs through an aggregate's **elements**: a cons whose
    car is an ancestor of itself sends `_write` back down the same path, and
    because each level then re-walks its own cdr chain the work is
    *exponential* in the depth, not merely unbounded. That is how
    `print.cons.random.2` -- twenty conses wired into a random cons graph, so
    whether it cycles at all depends on the draw -- held a full ANSI run at
    10GB with the depth guard doing nothing, and why the same run had
    completed before: the test is randomized.

    An aggregate on the current path is a genuine cycle, so it elides as
    ``...``. Structure that is merely *shared* (a DAG) is still printed at each
    of its occurrences, which is what an implementation without
    ``*PRINT-CIRCLE*`` must do -- the labels ``#1=``/``#1#`` are the real fix
    and belong to the printer's own milestone (plan.md section 5). Tracking the
    path rather than every object seen is what keeps those two cases apart.

    The set is keyed by ``id`` and carried on the context, whose lifetime is
    exactly one printing operation. `PrintContext.with_escape` copies slots by
    reference on purpose, so a ``~A`` nested inside a ``~S`` shares the path
    rather than starting a fresh one and re-entering the cycle.

    When `*PRINT-CIRCLE*` is true, the pre-pass has already assigned a label
    to every shared/cyclic aggregate; if this object is one of them and we
    are seeing it again, the elision must be the label ``#N#`` rather than
    the ``...`` that means "I gave up" -- they are not interchangeable
    (``print.cons.5`'s ``#1#`` would fail round-trip if it read back as
    ``...``).

    **Cutting cycles is still not a termination proof, so there is also a
    budget.** Cycles removed, the traversal enumerates *simple paths*, and a
    twenty-node graph of out-degree two has exponentially many of them --
    measured, some `print.cons.random.2` draws take minutes with cycle
    detection alone. `PRINT_BUDGET` caps the aggregates one printing operation
    may enter, at a level no real program approaches (a 100,000-cons structure
    prints in full) but which no graph can outrun. It is the same trade
    ``MAX_DEPTH`` already makes, for the same stated reason: the printer must
    never be the thing that aborts a run.
    """
    key = id(value)
    if key in ctx.in_progress:
        # Re-entry into an object already being printed. With
        # `*PRINT-CIRCLE*` this is exactly the cycle case the pre-pass
        # labelled; emit the back-reference rather than the lossy `...`.
        label = ctx.circle_map.get(key)
        if label is not None:
            return f'#{label}#'
        return '...'
    if ctx.budget[0] <= 0:
        return '...'
    ctx.budget[0] -= 1
    ctx.in_progress.add(key)
    try:
        return writer(value, ctx, depth)
    finally:
        ctx.in_progress.discard(key)


def _write_cons(value, ctx, depth):
    """Print a list, honouring ``*PRINT-LEVEL*``, ``*PRINT-LENGTH*`` and dots.

    ``*PRINT-LENGTH*`` elides with ``...`` after that many elements, and a
    non-list tail is printed after a dot (CLHS 22.1.3.5).

    **The cdr chain is walked, not recursed, so ``MAX_DEPTH`` does not bound
    it.** That is one of the two halves of the circularity cutoff that were
    missing (`_in_progress` is the other): `_write` guards *recursion* depth,
    which covers a deeply nested car, but a cdr cycle keeps `depth` constant
    forever and simply appended to `parts` until the process ran out of memory.
    `(let ((a (list 17 nil))) (setf (cdr a) a) a)` answered `MemoryError` as
    the value of the form.

    The cells of *this* chain are tracked, and only for the length of this
    call: a cycle elides as ``...`` (the same elision ``*PRINT-LENGTH*`` uses,
    since without ``*PRINT-CIRCLE*`` there is no ``#1#`` label to emit), while
    a tail legitimately **shared** between two lists still prints at both
    occurrences. A cutoff on the element *count* would instead have truncated
    every long proper list.

    With ``*PRINT-CIRCLE*``, the back-edge that closes the chain emits the
    matching ``#N#`` rather than ``...`` so the output reads back as the
    original object (``print.cons.7`'s ``#1=(17 . #1#)``).
    """
    if _level_exceeded(ctx, depth):
        return '#'
    # Emit the ``#N=`` label for this cons *before* walking its cdr chain:
    # the chain's back-edge adds the same label to `label_seen` and emits
    # the matching ``#N#``, and ``#N=`` must come first so a reader can
    # associate them. ``_circle_prefix`` itself adds to `label_seen` -- so
    # we compute the prefix string here, then walk, then prepend.
    #
    # **A back-reference means "don't walk again"** -- the body has already
    # been emitted under the matching ``#N=`` somewhere up the call stack,
    # so a second walk would print the same elements twice. (`print.cons.6`'s
    # `(list s1 s2 s1 s2)`: the second `s1` should be ``#1#``, not another
    # ``(1 2)``.)
    prefix = _circle_prefix(value, ctx)
    if prefix and not prefix.endswith('='):
        return prefix
    parts = []
    current = value
    seen = set()
    count = 0
    while isinstance(current, lispCons):
        if ctx.length is not None and count >= ctx.length:
            parts.append('...')
            break
        seen.add(id(current))
        parts.append(_write(current.car, ctx, depth + 1))
        count += 1
        current = current.cdr
        if current is None or isinstance(current, lispNull):
            break
        if not isinstance(current, lispCons):
            parts.append('.')
            parts.append(_write(current, ctx, depth + 1))
            break
        if id(current) in seen:
            # Back-edge closing the chain. Under `*PRINT-CIRCLE*` the pre-pass
            # labelled every cycle entry; this is the back-reference, not
            # the lossy `...` truncation `*PRINT-LENGTH*` uses. The label was
            # already added to `label_seen` by `_circle_prefix` at the top
            # of this call, so the matching back-reference here is `#N#`
            # rather than `#N=`. A back-edge is also a dotted terminator:
            # the cdr is a cons, not NIL, so the syntax requires a `.`
            # between the last element and the back-reference.
            label = ctx.circle_map.get(id(current))
            if label is not None:
                parts.append(f'. #{label}#')
            else:
                parts.append('...')
            break
        if ctx.circle and (id(current) in ctx.in_progress
                           or (id(current) in ctx.circle_map
                               and ctx.circle_map[id(current)] in ctx.label_seen)):
            # The cdr is a cons *other than a cell of this chain* whose body
            # is already spoken for: an ancestor still being printed higher
            # up (a cycle through cars), or a shared cons already emitted
            # under its `#N=` (`print.cons.random.2`'s random graph is full
            # of both). Inlining its elements as the continuation of this
            # list would splice another object's cells into it -- the read
            # back is then a different graph. Reference it instead; the cdr
            # is a cons, so the reference is a dotted terminator.
            label = ctx.circle_map.get(id(current))
            if label is not None:
                parts.append(f'. #{label}#')
            else:
                parts.append('...')
            break
        if ctx.circle and id(current) in ctx.circle_map:
            # Labelled but not yet emitted anywhere: this dotted reference
            # is the definition. Delegate to `_write` so `#N=` prefixes the
            # body (inlining the elements here would never emit the `#N=`
            # and leave every other `#N#` dangling).
            parts.append('.')
            parts.append(_write(current, ctx, depth + 1))
            break
    return prefix + '(' + ' '.join(parts) + ')'


def _vector_elements(value):
    """The live elements of any of this implementation's vector shapes.

    A Lisp vector is a Python ``list`` (a simple general vector) or a
    ``LispArray`` (one that records an element type, a fill pointer,
    adjustability or displacement) -- representations of one type, which is
    why ``#(1 2 3)`` and ``(vector 1 2 3)`` used to print differently. A fill
    pointer bounds the printed elements, and the array model applies it.
    """
    from fclpy.lispfunc.arrays import array_elements

    return array_elements(value)


def _string_text_of_array(value):
    """The text of a rank-1 LispArray that *is* a string, or None.

    CLHS 22.1.3.4: a string prints with string syntax. The array model
    (CLAUDE.md) gives strings three representations, and the printer only
    handled two -- `(make-array 4 :element-type 'character :displaced-to ...)`
    printed `#(#\\c #\\d ...)` (PRINT.STRING.12), and an `(array nil 0)`
    printed `#()` (PRINT.STRING.NIL.1/.2). `characters.is_string` is the one
    classifier, NIL element type included (the suite's
    ``:nil-vectors-are-strings`` choice); this only falls back to None -- and
    so to the vector spelling -- when an element is not actually a character,
    which no character- or NIL-element-type array the tests construct is.
    """
    from fclpy.lispfunc.arrays import array_elements
    from fclpy.lispfunc.characters import is_string

    if not is_string(value):
        return None
    parts = []
    for element in array_elements(value):
        if isinstance(element, Character):
            parts.append(element.char)
        elif isinstance(element, str) and len(element) == 1:
            parts.append(element)
        else:
            return None
    return ''.join(parts)


def _write_vector(value, ctx, depth):
    """Print a vector as ``#(...)``, or as ``#<...>`` when ``*PRINT-ARRAY*`` is NIL.

    A Python ``list`` is a *vector* here, not a list; printing it as ``(1 2 3)``
    -- which is what ``str()`` did -- made every vector read back as a cons.

    `*PRINT-LENGTH*` controls the maximum number of elements to print.
    """
    if _level_exceeded(ctx, depth):
        return '#'
    if not ctx.array:
        return _unreadable(value, 'VECTOR')
    prefix = _circle_prefix(value, ctx)
    if prefix and not prefix.endswith('='):
        # A back-reference: the body was already emitted under the matching
        # `#N=` (`_write_cons`'s rule -- a second walk would print the
        # elements twice, once after the reference that already names them).
        return prefix
    elements = _vector_elements(value)
    parts = []
    for index, element in enumerate(elements):
        if ctx.length is not None and index >= ctx.length:
            parts.append('...')
            break
        parts.append(_write(element, ctx, depth + 1))
    return prefix + '#(' + ' '.join(parts) + ')'


def _write_bit_vector(value, ctx):
    """Print a bit vector as ``#*1011`` (CLHS 22.1.3.7).

    A bit vector printed as ``#(1 0 1 1)`` reads back as a *general* vector,
    which is a different type -- the distinction only became printable once
    the array model recorded an element type.

    `*PRINT-LENGTH*` does not apply to bit-vectors (CLHS 22.1.4.4), so they
    always print in full regardless of the length limit.
    """
    if not ctx.array:
        return _unreadable(value, 'BIT-VECTOR')
    from fclpy.lispfunc.arrays import array_elements

    bits = array_elements(value)
    return '#*' + ''.join(str(b) for b in bits)


def _write_array(value, ctx, depth):
    """Print a multi-dimensional array as ``#rankA(nested lists)`` (CLHS 22.1.3.4).

    ``Array.__repr__`` produced ``#(ARRAY (2, 2))`` -- a Python tuple's repr
    embedded in what claimed to be Lisp syntax, and no element contents.
    """
    if _level_exceeded(ctx, depth):
        return '#'
    if not ctx.array:
        return _unreadable(value, 'ARRAY')

    # The reader infers dimension k of a #nA form from the length of the
    # first (k-1)-indexed sub-list, so a zero dimension makes the next one
    # unrecoverable: a (0 1) array prints #2A() and reads back (0 0), and a
    # (2 0 1) array reads back (2 0 0). (A trailing zero dimension *is*
    # recoverable -- each row exists and is empty -- which is why #2A(() ())
    # round-trips for (2 0).) Under *PRINT-READABLY* the printed form must
    # read back similar (CLHS 22.1.3.6), so signal PRINT-NOT-READABLE rather
    # than emit a shape that reads back as a different array; the harness's
    # RANDOMLY-CHECK-READABILITY passes :can-fail t and accepts exactly that
    # (PRINT.ARRAY.2.21/.22/.23). The conforming alternative the reader would
    # need is the full #A(element-type dimensions contents) syntax, which
    # fclpy's reader does not accept yet -- reported, not fixed here.
    dimensions = value.dimensions
    if ctx.readably and any(dimensions[k] > 0 for k in range(1, len(dimensions))
                            if dimensions[k - 1] == 0):
        return _write_unreadable_checked(value, 'ARRAY', ctx)

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

    # A trailing zero dimension needs no special case: `range(0)` is empty,
    # so a dimension of length 0 yields `()` and the shape still prints --
    # e.g. a 2x0 array as `#2A(() ())`, which reads back (2 0). A *leading*
    # zero dimension is the lossy case guarded above.
    prefix = _circle_prefix(value, ctx)
    if prefix and not prefix.endswith('='):
        # A back-reference: the body was already emitted under the matching
        # `#N=` (`_write_cons`'s rule).
        return prefix
    return prefix + f'#{value.rank}A' + nested([], 0)


def _write_structure(value, ctx, depth):
    """Print a structure instance as ``#S(NAME :SLOT value ...)`` (CLHS 22.1.3.10).

    DEFSTRUCT builds an ordinary `classes.LispInstance` whose class has
    ``metaclass_name == 'STRUCTURE-CLASS'`` -- the same object model
    DEFCLASS uses -- so a structure is told apart from a standard-object
    instance (which prints unreadably, the branch below this one) by its
    class's metaclass, not by a separate representation. Slot order comes
    from `LispClass.get_all_slots`, which preserves declaration order across
    `:include` inheritance the way the constructor already relies on.

    `*PRINT-LENGTH*` controls the number of slots printed; when exceeded, the
    output is truncated with `...` (CLHS 22.1.4.4).
    """
    if _level_exceeded(ctx, depth):
        return '#'
    parts = [_apply_print_case(value.lisp_class.name.name, ctx)]
    all_slots = value.lisp_class.get_all_slots()
    for slot_index, slot_name in enumerate(all_slots):
        if ctx.length is not None and slot_index >= ctx.length:
            parts.append('...')
            break
        parts.append(':' + _apply_print_case(slot_name, ctx))
        parts.append(_write(value.slot_values.get(slot_name), ctx, depth + 1))
    return '#S(' + ' '.join(parts) + ')'


def _write_hash_table(value, ctx):
    """Print a hash table as ``#<HASH-TABLE ...>``.

    A hash table has no readable representation, so this goes through
    `_write_unreadable_checked`'s promise-keeping rather than emitting
    ``#<...>`` unconditionally -- see that function on ``*PRINT-READABLY*``.
    The test is `HASH-TABLE-TEST`'s symbol; `len()` is not asked of the table
    because a hash table is no longer a `dict` (see `misc_hashtables`).
    """
    if ctx.readably:
        return _write_unreadable_checked(value, 'HASH-TABLE', ctx)
    return f'#<HASH-TABLE :TEST {value.test} :COUNT {value.count()}>'


def _write_random_state(value, ctx):
    """Print a random state, readably if possible.

    CLHS does not specify a readable syntax for random states, but the ANSI
    test suite expects them to be printable and readable. We represent them
    as quoted tuples that can be used as the seed argument to MAKE-RANDOM-STATE.
    """
    # Under readably, try to make it readable
    if ctx.readably:
        try:
            state = value.getstate()
            # state is a tuple of (index, tuple-of-values)
            # Write as: #.(MAKE-RANDOM-STATE '(state-tuple...))
            state_form = _write(list(state), ctx, 1)
            return f"#.(MAKE-RANDOM-STATE '{state_form})"
        except (AttributeError, TypeError, ValueError):
            # Fallback if we can't get the state
            pass

    # Unreadable form
    return f'#<RANDOM-STATE {id(value):x}>'


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

    # A shared atom (most often a gensym, e.g. ``print.cons.5``/``.6``) needs
    # its ``#N=``/``#N#`` label before its body. The aggregate cases
    # (``_write_cons`` / `_write_vector` / `_write_array` / `_write_structure`)
    # handle their own prefix, so skip those branches to avoid emitting the
    # label twice on a shared aggregate that contains only atoms.
    if ctx.circle and not _is_aggregate(value):
        prefix = _circle_prefix(value, ctx)
        if prefix and not prefix.endswith('='):
            # The label was already emitted (a back-reference) -- return it
            # as-is, the actual body of this object is not printed again.
            return prefix
    else:
        prefix = ''

    def _emit(s):
        """Prefix an atom's printed body with its ``#N=`` label, if any.

        Used for every dispatch branch below that handles an atom (symbol,
        number, character, string, etc.) -- the aggregate branches call
        their own writers and prepend their own labels.
        """
        return prefix + s

    # NIL, in each of the forms it takes. These are the canonical booleans,
    # not just symbols named NIL/T -- but the printed *name* still obeys the
    # symbol escape rule (see `_bool_atom_name`).
    if value is None or value is lisptype.NIL or isinstance(value, lispNull):
        return _emit(_bool_atom_name('NIL', ctx))
    if value is lisptype.T:
        return _emit(_bool_atom_name('T', ctx))
    if value is True:
        return _emit(_bool_atom_name('T', ctx))
    if value is False:
        return _emit(_bool_atom_name('NIL', ctx))

    if isinstance(value, lispKeyword):
        return _emit(_write_symbol(value, ctx))
    if isinstance(value, LispSymbol):
        return _emit(_write_symbol(value, ctx))

    if isinstance(value, Character):
        return _emit(_write_character(value, ctx))
    if isinstance(value, LispString):
        return _emit(_write_string(value, ctx))
    if isinstance(value, str):
        # A bare Python `str` is a string here. A length-1 `str` is *also* used
        # as a character in places; that ambiguity belongs to the string
        # representation split (plan.md finding I / M9), and guessing
        # "length 1 means character" here would print `(string #\a)` as `#\a`.
        return _emit(_write_string(value, ctx))

    if isinstance(value, int):
        return _emit(_write_integer(value, ctx))
    if isinstance(value, Fraction):
        return _emit(_write_ratio(value, ctx))
    if isinstance(value, float):
        return _emit(_write_float(value, ctx))
    if isinstance(value, complex):
        return _emit(_write_complex(value, ctx))

    if isinstance(value, lispCons):
        return _in_progress(value, ctx, _write_cons, depth)

    from fclpy.classes import LispInstance as _LispInstance
    if isinstance(value, _LispInstance) and value.lisp_class.metaclass_name == 'STRUCTURE-CLASS':
        return _in_progress(value, ctx, _write_structure, depth)

    from fclpy.lispfunc.arrays import LispArray, BIT_TYPE
    if isinstance(value, LispArray):
        if value.element_type is BIT_TYPE and value.rank == 1:
            return _write_bit_vector(value, ctx)
        if value.rank == 1:
            # A rank-1 array of character element type (NIL included) is a
            # string under the array model and prints with string syntax,
            # exactly as a LispString does (CLHS 22.1.3.4).
            text = _string_text_of_array(value)
            if text is not None:
                return _emit(_write_string(lisptype.LispString(text), ctx))
            return _in_progress(value, ctx, _write_vector, depth)
        return _in_progress(value, ctx, _write_array, depth)
    if isinstance(value, (list, tuple)):
        return _in_progress(value, ctx, _write_vector, depth)

    from fclpy.lispfunc.misc_hashtables import is_hash_table
    if is_hash_table(value):
        return _write_hash_table(value, ctx)

    if isinstance(value, lisptype.Package):
        return f'#<PACKAGE {value.name}>'

    from fclpy.lispfunc.pathnames import Pathname
    if isinstance(value, Pathname):
        return '#P' + _write_string(value.namestring(), ctx.with_escape(True))

    from fclpy.lispfunc.utilities_system import RandomState
    if isinstance(value, RandomState):
        return _write_random_state(value, ctx)

    if isinstance(value, lisptype.Restart):
        # CLHS 9.1: under PRINC (escape false), a restart's printed
        # representation is produced by its report function; under PRIN1
        # there is no readable syntax for a restart, so it prints as an
        # ordinary unreadable object naming its restart-name.
        name = value.name.name if isinstance(value.name, lisptype.LispSymbol) else 'NIL'
        if not ctx.escape and value.report_function is not None:
            from fclpy.lispfunc.evaluation_conditions import restart_report_text
            report = restart_report_text(value)
            if report is not None:
                return report
        return f'#<RESTART {name}>'

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
    # `*PRINT-CIRCLE*` requires a labelling pre-pass over the object graph
    # before printing starts, so the print path knows which objects to mark
    # with ``#N=``/``#N#``. The pass is O(|reachable|) in the worst case and
    # is bounded by the same budget the print path uses, so a cyclic graph
    # cannot make it run forever either.
    if ctx.circle and not ctx.circle_map:
        _compute_circle_map(value, ctx)
        # Seed `next_label` from the map so the print path can hand out fresh
        # labels if it ever needs to (currently it does not -- every label is
        # assigned up front).
        ctx.next_label = max(ctx.circle_map.values(), default=0) + 1
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
