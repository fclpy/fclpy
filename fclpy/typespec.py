"""The one type-specifier model: CLHS 4.2.3 type specifiers, decided once.

Three *partial* interpretations of a type specifier used to coexist here, and
none of them could see what the others knew:

- `comparison.typep` walked a ladder of `elif type_name == ...` branches over
  an *object*. It could answer "is this object an integer in [0,10]" but it has
  no way to represent a type, so it could not answer anything about two types.
- `comparison.subtypep` was a table of hardcoded *string pairs* --
  `if t1 == 'INTEGER' and t2 in ['RATIONAL','REAL','NUMBER']` -- with no entry
  for any compound specifier at all. `(subtypep '(integer 0 10) 'integer)`
  answered NIL, and so did `(subtypep 'fixnum 'integer)`.
- `DEFTYPE` stored its expander in `global_env.user_types` and **nothing ever
  read that dict**, so a user type was invisible to both of the above.

They also disagreed on facts. TYPEP called an integer a FIXNUM below 2**29
while `MOST-POSITIVE-FIXNUM` answered 2**63-1, so
`(typep most-positive-fixnum 'fixnum)` was false -- a tautology failing because
the same constant had two homes (see `MOST_POSITIVE_FIXNUM` below, which is now
the one home both read).

This module replaces all three with a single representation. `parse_type`
turns a specifier into a `Ctype`; `type_contains` answers TYPEP and
`type_subtypep` answers SUBTYPEP *from the same parse*, so they cannot drift.

## Why a set algebra and not a bigger table

ansi-test does not merely ask `(subtypep 'a 'b)`. Its `check-equivalence`
(auxiliary/types-aux.lsp) demands **twelve** answers per call and requires all
twelve to be marked *certain*, among them

    (subtypep '(and TYPE1 (not TYPE2)) nil)     ; is this difference empty?
    (subtypep t '(or TYPE1 (not TYPE2)))        ; is this union everything?

so nothing short of a decision procedure over complement, union and
intersection can answer them. That is what this module is: a type is a set,
and `(subtypep a b)` is decided as `a \\ b == {}`.

## The representation

The CL universe is partitioned into disjoint **sorts** (`SORT_*` below). Types
in different sorts are automatically disjoint, which is most of what
disjointness reasoning needs. Each sort carries a representation closed under
union, intersection and difference:

    INTEGER    a set of integer intervals            (IntBulk)
    RATIO      a set of dense intervals              (DenseBulk)
    FLOAT      a set of dense intervals              (DenseBulk)
    COMPLEX    present or absent                     (ComplexBulk)
    CHARACTER  a set of partition cells              (CellBulk)
    SYMBOL     a set of partition cells              (CellBulk)
    CLASS      a set of class cells                  (CellBulk)
    CONS       a set of car/cdr rectangles           (ConsBulk)
    ARRAY      per (simplicity, element-type) shapes (ArrayBulk)

A sort's set is a `SortSet`, which wraps its bulk region with two finite
EQL-keyed adjustments -- objects added (`extra`) and objects removed
(`removed`). That is how `(member #\\a #\\b)` and `(and character (not (eql
#\\a)))` are represented in a sort whose bulk cannot name individual objects,
and it is why `(and integer (not (eql 10)))` comes out as exactly
`(or (integer * 9) (integer 11 *))` -- the integer bulk *can* name it, so the
adjustment is folded straight into the intervals.

## Certainty, and why `satisfies` must stay undecided

`(satisfies f)` is not decidable in general, and ansi-test checks that an
implementation does not pretend otherwise: `subtypep.cons.44` builds a type
from four predicates that are literally `(= 1 (random 2))` and requires
SUBTYPEP to answer **NIL NIL** in both directions. So an undecidable specifier
becomes an `Opaque` literal rather than a guess, and a `Ctype` is a *disjunction
of conjuncts* over those literals (`Conjunct`) rather than a bare region.

When there are no opaque literals -- the overwhelmingly common case -- there is
exactly one conjunct and the answer is exact. Opaque literals still cancel
propositionally, which is what makes `(and X X)` equivalent to `X` even for a
type this module cannot interpret.

The one case where an opaque literal *is* decided is the case ansi-test
requires to be decided: when the rest of the conjunct is a finite set of
concrete objects, the predicate is simply called on each of them. That is how
`(member a b c d)` is certainly a subtype of `(satisfies symbolp)`
(`subtypep.member.27`) while the random-predicate type above stays undecided.
"""

from __future__ import annotations

import math
from fractions import Fraction

from fclpy import lisptype


# ---------------------------------------------------------------------------
# Implementation limits with exactly one home
# ---------------------------------------------------------------------------

# The fixnum range. `comparison.typep` used 2**29 while `MOST-POSITIVE-FIXNUM`
# answered 2**63-1, so FIXNUM the *type* and MOST-POSITIVE-FIXNUM the *constant*
# described different implementations. Both now read these, so
# `(typep most-positive-fixnum 'fixnum)` is true by construction and
# `(subtypep '(integer most-negative-fixnum most-positive-fixnum) 'fixnum)` --
# ansi-test's `subtypep.fixnum.integer` -- is decidable.
MOST_POSITIVE_FIXNUM = 2 ** 63 - 1
MOST_NEGATIVE_FIXNUM = -(2 ** 63)


class LispTypeSpecError(Exception):
    """A specifier this module cannot interpret as a type at all.

    Distinct from "undecidable": an unknown *name* means SUBTYPEP has no type
    to reason about (CLHS 4.3.4 permits returning NIL NIL), whereas
    `(satisfies f)` is a perfectly good type whose membership is merely not
    computable. Conflating them is what would make `subtypep.cons.44` answer
    with false confidence.
    """


class _GiveUp(Exception):
    """Raised inside a bulk operation whose exact result got too large.

    Caught at the `Ctype` boundary and turned into an `Opaque` literal, i.e.
    into *uncertainty*, never into a wrong answer. Standing rule 4: a loud gap,
    not a silent one -- and here "loud" means SUBTYPEP's second value goes NIL.
    """


# ---------------------------------------------------------------------------
# Small helpers over the Lisp object model (all lazily imported: this module
# sits underneath comparison.py, arrays.py and classes.py, which import it)
# ---------------------------------------------------------------------------

def _is_nil(obj):
    return obj is None or obj is lisptype.NIL or isinstance(obj, lisptype.lispNull)


def _cons_p(obj):
    return isinstance(obj, lisptype.lispCons)


def _lisp_list(obj):
    """A Lisp list as a Python list. NIL is the empty list."""
    out = []
    while _cons_p(obj):
        out.append(obj.car)
        obj = obj.cdr
    return out


def _type_name(spec):
    """The name a type-specifier atom denotes, upper-cased, or None."""
    if isinstance(spec, lisptype.LispSymbol):
        return spec.name.upper()
    if isinstance(spec, lisptype.LispString):
        return str(spec).upper()
    if isinstance(spec, str):
        return spec.upper()
    return None


def _is_wild(spec):
    """True for the `*` that means "unspecified" in a compound specifier."""
    return _type_name(spec) == '*'


def eql_objects(a, b):
    """EQL, asked of the one implementation of it.

    Reimplementing EQL here would be standing rule 3's defect in the one place
    it matters most: `(member <bignum> ...)` requires EQL-by-value for
    integers (`subtypep.member.9`) while a freshly copied list must *not* be
    EQL to its original (`subtypep.member.11`), and a second copy of that rule
    would eventually disagree with the first.
    """
    from fclpy.lispfunc.comparison import eql as _cl_eql
    return _cl_eql(a, b) == lisptype.T


# ---------------------------------------------------------------------------
# EqlSet -- a finite set of Lisp objects under EQL
# ---------------------------------------------------------------------------

class EqlSet:
    """A small finite set of objects compared with EQL.

    Deliberately a linear-scan tuple rather than a hashed set: EQL is not
    Python `==`/`hash` (0.0 and -0.0 are EQL floats, two equal fresh lists are
    not EQL), and building a hash key that respects that is a second EQL. These
    sets hold the elements of a `member` specifier, so they are tiny.
    """

    __slots__ = ('items',)

    def __init__(self, items=()):
        kept = []
        for item in items:
            if not any(eql_objects(item, k) for k in kept):
                kept.append(item)
        self.items = tuple(kept)

    def __bool__(self):
        return bool(self.items)

    def __len__(self):
        return len(self.items)

    def __iter__(self):
        return iter(self.items)

    def contains(self, obj):
        return any(eql_objects(obj, item) for item in self.items)

    def union(self, other):
        return EqlSet(self.items + tuple(other))

    def difference(self, other):
        return EqlSet([i for i in self.items if not other.contains(i)])


EMPTY_EQL_SET = EqlSet()


# ---------------------------------------------------------------------------
# Bulk regions, one per sort
#
# Every bulk implements the same six operations, which is what lets `SortSet`
# and `Region` be written once instead of once per sort:
#
#   is_empty()  union(o)  intersect(o)  subtract(o)  contains(obj)
#   finite_elements() -> list | None      ("None" means "not a small finite set")
# ---------------------------------------------------------------------------

# `finite_elements` is used only to decide emptiness of "bulk minus a finite
# set", so returning None for a *large but finite* region is safe: a large
# region minus finitely many named objects is still nonempty.
_FINITE_ENUMERATION_CAP = 4096


class IntBulk:
    """A set of integers, as inclusive intervals. `None` is an infinite end."""

    __slots__ = ('spans',)

    def __init__(self, spans=()):
        self.spans = tuple(_normalize_int_spans(spans))

    @staticmethod
    def universe():
        return IntBulk([(None, None)])

    @staticmethod
    def interval(low, low_ex, high, high_ex):
        """The integers in a possibly-exclusive, possibly-fractional interval.

        CLHS 12.1.2's exclusive bound `(9)` and a rational bound like `-1/2`
        both collapse onto inclusive integer bounds here, which is exactly why
        `(integer (9))` and `(integer 10)` come out *equal* rather than merely
        related, and why `(and integer (real -1/2 1/2))` reduces to
        `(integer 0 0)` (ansi-test `subtypep.real.9`).
        """
        lo = None if low is None else (math.floor(low) + 1 if low_ex else math.ceil(low))
        hi = None if high is None else (math.ceil(high) - 1 if high_ex else math.floor(high))
        if lo is not None and hi is not None and lo > hi:
            return IntBulk()
        return IntBulk([(lo, hi)])

    def is_empty(self):
        return not self.spans

    def union(self, other):
        return IntBulk(self.spans + other.spans)

    def intersect(self, other):
        out = []
        for a_lo, a_hi in self.spans:
            for b_lo, b_hi in other.spans:
                lo = _max_low(a_lo, b_lo)
                hi = _min_high(a_hi, b_hi)
                if lo is None or hi is None or lo <= hi:
                    out.append((lo, hi))
        return IntBulk(out)

    def subtract(self, other):
        spans = list(self.spans)
        for b_lo, b_hi in other.spans:
            nxt = []
            for a_lo, a_hi in spans:
                # the part of [a_lo,a_hi] strictly below b_lo
                if b_lo is not None and (a_lo is None or a_lo < b_lo):
                    top = b_lo - 1 if a_hi is None else min(a_hi, b_lo - 1)
                    nxt.append((a_lo, top))
                # the part strictly above b_hi
                if b_hi is not None and (a_hi is None or a_hi > b_hi):
                    bottom = b_hi + 1 if a_lo is None else max(a_lo, b_hi + 1)
                    nxt.append((bottom, a_hi))
                if b_lo is None and b_hi is None:
                    pass
            spans = nxt
        return IntBulk(spans)

    def contains(self, obj):
        if not isinstance(obj, int) or isinstance(obj, bool):
            if isinstance(obj, Fraction) and obj.denominator == 1:
                obj = int(obj)
            else:
                return False
        for lo, hi in self.spans:
            if (lo is None or obj >= lo) and (hi is None or obj <= hi):
                return True
        return False

    def finite_elements(self):
        total = 0
        for lo, hi in self.spans:
            if lo is None or hi is None:
                return None
            total += hi - lo + 1
            if total > _FINITE_ENUMERATION_CAP:
                return None
        out = []
        for lo, hi in self.spans:
            out.extend(range(lo, hi + 1))
        return out


def _normalize_int_spans(spans):
    """Sort, merge overlapping *and adjacent* integer spans.

    Adjacency matters: without merging `[0,5]` and `[6,10]` into `[0,10]`,
    `(or (integer 0 5) (integer 6 10))` would not be recognised as equal to
    `(integer 0 10)`, and ansi-test compares exactly such pairs.
    """
    items = [s for s in spans if s[0] is None or s[1] is None or s[0] <= s[1]]
    items.sort(key=lambda s: (s[0] is not None, s[0] if s[0] is not None else 0))
    out = []
    for lo, hi in items:
        if not out:
            out.append((lo, hi))
            continue
        p_lo, p_hi = out[-1]
        if p_hi is None:
            continue
        if lo is not None and lo > p_hi + 1:
            out.append((lo, hi))
        else:
            out[-1] = (p_lo, None if hi is None else max(p_hi, hi))
    return out


def _max_low(a, b):
    if a is None:
        return b
    if b is None:
        return a
    return max(a, b)


def _min_high(a, b):
    if a is None:
        return b
    if b is None:
        return a
    return min(a, b)


class DenseBulk:
    """A set of intervals over a dense numeric domain (RATIO or FLOAT).

    Kept dense -- not enumerated -- because the questions asked of it are about
    *bounds*: `(and (real 0 (10)) (real (5) 15))` must come out as
    `(real (5) (10))`, brackets included.

    RATIO is a dense domain here even though it is the rationals *minus* the
    integers, which shows up in `_interval_nonempty`: `(rational 1 1)` names a
    nonempty type but its RATIO part is empty, because 1 is an integer.
    """

    __slots__ = ('domain', 'spans')

    def __init__(self, domain, spans=()):
        self.domain = domain
        self.spans = tuple(_normalize_dense_spans(domain, spans))

    def _new(self, spans):
        return DenseBulk(self.domain, spans)

    def universe(self):
        return self._new([(None, False, None, False)])

    def is_empty(self):
        return not self.spans

    def union(self, other):
        return self._new(self.spans + other.spans)

    def intersect(self, other):
        out = []
        for a in self.spans:
            for b in other.spans:
                lo, lo_ex = _max_dense_low(a[0], a[1], b[0], b[1])
                hi, hi_ex = _min_dense_high(a[2], a[3], b[2], b[3])
                out.append((lo, lo_ex, hi, hi_ex))
        return self._new(out)

    def subtract(self, other):
        spans = list(self.spans)
        for b_lo, b_lo_ex, b_hi, b_hi_ex in other.spans:
            nxt = []
            for a_lo, a_lo_ex, a_hi, a_hi_ex in spans:
                if b_lo is not None:
                    # everything below b's low end
                    hi, hi_ex = _min_dense_high(a_hi, a_hi_ex, b_lo, not b_lo_ex)
                    nxt.append((a_lo, a_lo_ex, hi, hi_ex))
                if b_hi is not None:
                    lo, lo_ex = _max_dense_low(a_lo, a_lo_ex, b_hi, not b_hi_ex)
                    nxt.append((lo, lo_ex, a_hi, a_hi_ex))
            spans = nxt
        return self._new(spans)

    def contains(self, obj):
        if self.domain == 'FLOAT':
            if not isinstance(obj, float):
                return False
        else:
            if isinstance(obj, Fraction):
                if obj.denominator == 1:
                    return False
            else:
                return False
        for lo, lo_ex, hi, hi_ex in self.spans:
            if lo is not None:
                if obj < lo or (lo_ex and obj == lo):
                    continue
            if hi is not None:
                if obj > hi or (hi_ex and obj == hi):
                    continue
            return True
        return False

    def finite_elements(self):
        """Only a union of degenerate points is finite here.

        `(float 0.0 0.0)` is the single float 0.0 and must be recognised as
        such, because ansi-test's signed-zero group compares it against
        `(member -0.0 0.0)`; `(float 0.0 1.0)` is treated as infinite.
        """
        out = []
        for lo, lo_ex, hi, hi_ex in self.spans:
            if lo is None or hi is None or lo != hi or lo_ex or hi_ex:
                return None
            out.append(lo)
        return out


def _normalize_dense_spans(domain, spans):
    kept = [s for s in spans if _interval_nonempty(domain, s)]
    kept.sort(key=lambda s: (s[0] is not None,
                             s[0] if s[0] is not None else 0,
                             s[1]))
    out = []
    for span in kept:
        if not out:
            out.append(span)
            continue
        prev = out[-1]
        merged = _merge_dense(domain, prev, span)
        if merged is None:
            out.append(span)
        else:
            out[-1] = merged
    return out


def _merge_dense(domain, a, b):
    """Merge two dense intervals if their union is again one interval."""
    a_lo, a_lo_ex, a_hi, a_hi_ex = a
    b_lo, b_lo_ex, b_hi, b_hi_ex = b
    if a_hi is not None and b_lo is not None:
        if a_hi < b_lo:
            return None
        if a_hi == b_lo and a_hi_ex and b_lo_ex:
            # a ends before the point and b starts after it: the point itself is
            # missing, so this is genuinely two intervals -- unless the domain
            # has no member at that point anyway (an integer, for RATIO).
            if not (domain == 'RATIO' and _is_integral(a_hi)):
                return None
    lo, lo_ex = _min_dense_low(a_lo, a_lo_ex, b_lo, b_lo_ex)
    hi, hi_ex = _max_dense_high(a_hi, a_hi_ex, b_hi, b_hi_ex)
    return (lo, lo_ex, hi, hi_ex)


def _is_integral(value):
    if isinstance(value, int) and not isinstance(value, bool):
        return True
    if isinstance(value, Fraction):
        return value.denominator == 1
    if isinstance(value, float):
        return value.is_integer()
    return False


def _interval_nonempty(domain, span):
    lo, lo_ex, hi, hi_ex = span
    if lo is None or hi is None:
        return True
    if lo > hi:
        return False
    if lo == hi:
        if lo_ex or hi_ex:
            return False
        # A single point is a member of FLOAT, but a single *integer* point is
        # not a member of RATIO.
        return not (domain == 'RATIO' and _is_integral(lo))
    return True


def _max_dense_low(a, a_ex, b, b_ex):
    if a is None:
        return b, b_ex
    if b is None:
        return a, a_ex
    if a > b:
        return a, a_ex
    if b > a:
        return b, b_ex
    return a, a_ex or b_ex


def _min_dense_low(a, a_ex, b, b_ex):
    if a is None or b is None:
        return None, False
    if a < b:
        return a, a_ex
    if b < a:
        return b, b_ex
    return a, a_ex and b_ex


def _min_dense_high(a, a_ex, b, b_ex):
    if a is None:
        return b, b_ex
    if b is None:
        return a, a_ex
    if a < b:
        return a, a_ex
    if b < a:
        return b, b_ex
    return a, a_ex or b_ex


def _max_dense_high(a, a_ex, b, b_ex):
    if a is None or b is None:
        return None, False
    if a > b:
        return a, a_ex
    if b > a:
        return b, b_ex
    return a, a_ex and b_ex


class ComplexBulk:
    """Complexes, present or absent.

    fclpy represents a complex as a Python `complex`, so every `(complex X)`
    with a nonempty X upgrades to the same one type (CLHS 12.1.5.3 permits an
    implementation a single complex representation). Recording that as a
    boolean rather than as a set of part types is what makes
    `check-complex-upgrading`'s "certainly T when the upgraded parts are equal"
    come out T for every pair -- and it keeps this module consistent with
    `UPGRADED-COMPLEX-PART-TYPE`, which likewise answers one thing.
    """

    __slots__ = ('present',)

    def __init__(self, present=False):
        self.present = bool(present)

    def is_empty(self):
        return not self.present

    def union(self, other):
        return ComplexBulk(self.present or other.present)

    def intersect(self, other):
        return ComplexBulk(self.present and other.present)

    def subtract(self, other):
        return ComplexBulk(self.present and not other.present)

    def contains(self, obj):
        return self.present and isinstance(obj, complex)

    def finite_elements(self):
        return [] if not self.present else None


class CellBulk:
    """A set of cells from a finite partition of one sort.

    The trick this makes possible: a named type's *cell* is "objects of exactly
    this type", so a named type is the set of cells in its subtree, and a set
    built from named types, unions, intersections and complements is always
    again a set of cells. Closure is what `check-equivalence` needs, and it is
    why CHARACTER, SYMBOL and the whole class hierarchy share one
    implementation instead of three.
    """

    __slots__ = ('cells',)

    def __init__(self, cells=()):
        self.cells = frozenset(cells)

    def is_empty(self):
        return not self.cells

    def union(self, other):
        return CellBulk(self.cells | other.cells)

    def intersect(self, other):
        return CellBulk(self.cells & other.cells)

    def subtract(self, other):
        return CellBulk(self.cells - other.cells)

    def contains(self, obj):
        cell = _cell_of_object(obj)
        return cell is not None and cell in self.cells

    def finite_elements(self):
        return [] if not self.cells else None


class _Unrestricted:
    """The `*` component of a cons type, as in `(cons * *)`.

    Held as a placeholder rather than as the universal `Ctype` because the
    universal type *contains* the universal cons type: materialising one inside
    the other does not terminate. `_component` resolves it at the point of use,
    by which time there is a universe to resolve it to.
    """

    __slots__ = ()

    def is_definitely_empty(self):
        return False

    def is_definitely_nonempty(self):
        return True


ANY = _Unrestricted()


def _component(spec):
    return top() if spec is ANY else spec


# `ANY` must survive the component operations rather than being resolved into a
# universe first: resolving it means building the universal type, which contains
# the universal *cons* type, whose components are `ANY` again -- so
# `(cons * *) & (cons * *)` would recurse without bound. Keeping `ANY` as an
# identity for intersection and an annihilator for difference is what makes the
# recursion terminate, and it terminates on the other side too because a real
# component is always structurally smaller than the type it came from.
def _intersect_components(left, right):
    if left is ANY:
        return right
    if right is ANY:
        return left
    return left.intersect(right)


def _subtract_components(left, right):
    if right is ANY:
        return bottom()
    if left is ANY:
        return top().subtract(right)
    return left.subtract(right)


def _component_contains(spec, obj):
    if spec is ANY:
        return True
    return spec.contains(obj)


class ConsBulk:
    """A union of car/cdr rectangles.

    Cons types are closed under difference, which is the fact that makes them
    decidable: `(a x b) \\ (c x d)` is `((a\\c) x b) u ((a&c) x (b\\d))`, again a
    union of rectangles. That identity is what lets
    `(and cons (not (cons symbol symbol)))` be recognised as
    `(or (cons (not symbol) *) (cons * (not symbol)))` -- ansi-test
    `subtypep.cons.7`.
    """

    __slots__ = ('rects',)

    _RECT_CAP = 256

    def __init__(self, rects=()):
        kept = [(a, d) for (a, d) in rects
                if not a.is_definitely_empty() and not d.is_definitely_empty()]
        if len(kept) > self._RECT_CAP:
            raise _GiveUp('cons type too complex')
        self.rects = tuple(kept)

    @staticmethod
    def universe():
        return ConsBulk([(ANY, ANY)])

    def is_empty(self):
        return not self.rects

    def union(self, other):
        return ConsBulk(self.rects + other.rects)

    def intersect(self, other):
        out = []
        for a1, d1 in self.rects:
            for a2, d2 in other.rects:
                out.append((_intersect_components(a1, a2),
                            _intersect_components(d1, d2)))
        return ConsBulk(out)

    def subtract(self, other):
        rects = list(self.rects)
        for a2, d2 in other.rects:
            nxt = []
            for a1, d1 in rects:
                nxt.append((_subtract_components(a1, a2), d1))
                nxt.append((_intersect_components(a1, a2),
                            _subtract_components(d1, d2)))
            rects = [(a, d) for (a, d) in nxt
                     if not a.is_definitely_empty() and not d.is_definitely_empty()]
            if len(rects) > self._RECT_CAP:
                raise _GiveUp('cons difference too complex')
        return ConsBulk(rects)

    def contains(self, obj):
        if not _cons_p(obj):
            return False
        return any(_component_contains(a, obj.car) and _component_contains(d, obj.cdr)
                   for a, d in self.rects)

    def finite_elements(self):
        return [] if not self.rects else None

    def is_definitely_nonempty(self):
        """Does some rectangle certainly hold a cons?

        A rectangle (a, d) is inhabited when *some* cons exists with car in a
        and cdr in d -- which needs both components to be certainly
        non-empty. Pruning on `is_definitely_empty` alone (the constructor's
        rule) keeps rectangles whose components are merely *possibly*
        inhabited, so "no rects" is the only emptiness this class can prove
        on its own; ansi-test subtypep.cons.44 builds a difference out of
        rectangles with `(satisfies ...)` components, which are exactly the
        possibly-inhabited kind, and requires SUBTYPEP to answer unknown
        rather than certainly-empty."""
        return any(_component(a).is_definitely_nonempty()
                   and _component(d).is_definitely_nonempty()
                   for (a, d) in self.rects)


# ---------------------------------------------------------------------------
# Arrays
#
# An array type is a question about three things (CLHS 15.1): simplicity, the
# *upgraded* element type, and the dimensions. fclpy upgrades every element
# type to one of exactly four (T, BIT, CHARACTER, NIL -- see
# `arrays.upgraded_element_type`), and simplicity is a boolean, so those two
# axes are an eight-element key rather than something needing its own algebra.
# Only the dimensions do, and they get `DimSet`.
#
# NIL is its own key, disjoint from the other three, rather than folded into
# T: UPGRADED-ARRAY-ELEMENT-TYPE(NIL) is NIL, not T (CLHS 15.1.2.1's
# monotonicity requirement -- NIL is a subtype of both BIT and CHARACTER, so
# its UAET must be a subtype of both UAET(BIT)=BIT and UAET(CHARACTER)=
# CHARACTER, and only NIL itself is a subtype of two disjoint types). Folding
# it into T made `(array t)` and `(array nil)` the same lattice region
# instead of disjoint ones, which is what `subtypep.array.7`'s
# `check-disjointness` over `*array-element-types*` (which includes `nil`)
# caught the moment `upgraded_element_type` stopped answering `T` for it.
# ---------------------------------------------------------------------------

_ARRAY_ETYPES = ('T', 'BIT', 'CHARACTER', 'NIL')
_ARRAY_KEYS = tuple((simple, etype)
                    for simple in (True, False)
                    for etype in _ARRAY_ETYPES)


class DimSet:
    """A set of array shapes.

    `other_ranks` is the wildcard: it means every rank *not* named in
    `per_rank` is wholly included. Without it, complementing `(array t (*))`
    would have to enumerate every rank up to `array-rank-limit`; with it,
    `(not (array t (*)))` is "rank 1 minus that shape, plus all other ranks".
    """

    __slots__ = ('other_ranks', 'per_rank')

    def __init__(self, other_ranks=False, per_rank=None):
        pruned = {}
        for rank, axes in (per_rank or {}).items():
            if other_ranks:
                # a rank whose entry is already "everything" is redundant
                if _axes_is_universe(rank, axes):
                    continue
            elif not axes:
                continue
            pruned[rank] = tuple(axes)
        self.other_ranks = bool(other_ranks)
        self.per_rank = pruned

    @staticmethod
    def universe():
        return DimSet(other_ranks=True)

    @staticmethod
    def empty():
        return DimSet(other_ranks=False)

    def is_empty(self):
        return not self.other_ranks and not self.per_rank

    def _ranks(self, other):
        return set(self.per_rank) | set(other.per_rank)

    def _axes_for(self, rank):
        if rank in self.per_rank:
            return self.per_rank[rank]
        return (_axes_universe(rank),) if self.other_ranks else ()

    def union(self, other):
        per = {}
        for rank in self._ranks(other):
            per[rank] = self._axes_for(rank) + other._axes_for(rank)
        return DimSet(self.other_ranks or other.other_ranks, per)

    def intersect(self, other):
        per = {}
        for rank in self._ranks(other):
            per[rank] = _axes_intersect(self._axes_for(rank), other._axes_for(rank))
        return DimSet(self.other_ranks and other.other_ranks, per)

    def subtract(self, other):
        per = {}
        for rank in self._ranks(other):
            per[rank] = _axes_subtract(rank, self._axes_for(rank), other._axes_for(rank))
        return DimSet(self.other_ranks and not other.other_ranks, per)

    def contains_dimensions(self, dimensions):
        rank = len(dimensions)
        for axes in self._axes_for(rank):
            if all(axis.contains(size) for axis, size in zip(axes, dimensions)):
                return True
        return False


def _axes_universe(rank):
    """The all-shapes rectangle for a given rank: every axis unconstrained."""
    return tuple(IntBulk([(0, None)]) for _ in range(rank))


_ANY_SIZE_SPANS = ((0, None),)


def _axes_is_universe(rank, axes):
    """True when this rectangle set is *every* shape of that rank.

    Used only to prune redundant `per_rank` entries when `other_ranks` already
    covers them, so a conservative False merely costs a redundant entry.
    """
    if not axes:
        return False
    if rank == 0:
        return True
    if len(axes) != 1:
        return False
    return all(axis.spans == _ANY_SIZE_SPANS for axis in axes[0])


def _axes_intersect(left, right):
    out = []
    for a in left:
        for b in right:
            merged = tuple(x.intersect(y) for x, y in zip(a, b))
            if all(not m.is_empty() for m in merged) or not merged:
                out.append(merged)
    return tuple(out)


def _axes_subtract(rank, left, right):
    """Rectangle difference, one axis at a time (the ConsBulk identity, n-ary)."""
    rects = list(left)
    for b in right:
        nxt = []
        for a in rects:
            if not rank:
                # rank 0 has a single shape: subtracting it removes everything
                continue
            for axis in range(rank):
                piece = list(a)
                piece[axis] = a[axis].subtract(b[axis])
                for before in range(axis):
                    piece[before] = a[before].intersect(b[before])
                if all(not p.is_empty() for p in piece):
                    nxt.append(tuple(piece))
        rects = nxt
        if len(rects) > 512:
            raise _GiveUp('array dimension difference too complex')
    return tuple(rects)


class ArrayBulk:
    """Array shapes, keyed by (simplicity, upgraded element type)."""

    __slots__ = ('by_key',)

    def __init__(self, by_key=None):
        self.by_key = {k: v for k, v in (by_key or {}).items() if not v.is_empty()}

    @staticmethod
    def universe():
        return ArrayBulk({k: DimSet.universe() for k in _ARRAY_KEYS})

    def is_empty(self):
        return not self.by_key

    def union(self, other):
        out = {}
        for key in set(self.by_key) | set(other.by_key):
            a = self.by_key.get(key, DimSet.empty())
            b = other.by_key.get(key, DimSet.empty())
            out[key] = a.union(b)
        return ArrayBulk(out)

    def intersect(self, other):
        out = {}
        for key in set(self.by_key) & set(other.by_key):
            out[key] = self.by_key[key].intersect(other.by_key[key])
        return ArrayBulk(out)

    def subtract(self, other):
        out = {}
        for key, dims in self.by_key.items():
            if key in other.by_key:
                out[key] = dims.subtract(other.by_key[key])
            else:
                out[key] = dims
        return ArrayBulk(out)

    def contains(self, obj):
        from fclpy.lispfunc import arrays as _arrays
        if not _arrays.is_array(obj):
            return False
        if isinstance(obj, str) and len(obj) == 1:
            return False  # a one-character Python str is a character here
        key = (bool(_arrays.is_simple_array(obj)),
               _etype_key(_arrays.element_type_of(obj)))
        dims = self.by_key.get(key)
        if dims is None:
            return False
        return dims.contains_dimensions(_arrays.array_dimensions_of(obj))

    def finite_elements(self):
        return [] if not self.by_key else None


def _etype_key(element_type):
    from fclpy.lispfunc import arrays as _arrays
    if element_type is _arrays.BIT_TYPE:
        return 'BIT'
    if element_type is _arrays.CHARACTER_TYPE:
        return 'CHARACTER'
    if element_type is _arrays.NIL_TYPE:
        return 'NIL'
    return 'T'


# ---------------------------------------------------------------------------
# Sorts
# ---------------------------------------------------------------------------

SORT_INTEGER = 'INTEGER'
SORT_RATIO = 'RATIO'
SORT_FLOAT = 'FLOAT'
SORT_COMPLEX = 'COMPLEX'
SORT_CHARACTER = 'CHARACTER'
SORT_SYMBOL = 'SYMBOL'
SORT_CONS = 'CONS'
SORT_ARRAY = 'ARRAY'
SORT_CLASS = 'CLASS'

ALL_SORTS = (SORT_INTEGER, SORT_RATIO, SORT_FLOAT, SORT_COMPLEX,
             SORT_CHARACTER, SORT_SYMBOL, SORT_CONS, SORT_ARRAY, SORT_CLASS)

# CHARACTER is partitioned into the standard characters and the rest. fclpy has
# a single character representation, so BASE-CHAR is CHARACTER and
# EXTENDED-CHAR is empty -- and ansi-test is written to discover that rather
# than to assume otherwise (`subtypep.extended-char.1`/`.2` are mutually
# implying conditionals, and `*disjoint-types-list2*` drops EXTENDED-CHAR when
# `(subtypep 'character 'base-char)` is true).
_CHAR_CELLS = ('STANDARD-CHAR', 'OTHER-CHAR')

# SYMBOL is partitioned into keywords and everything else; NIL and T are
# ordinary non-keyword symbols, so NULL and BOOLEAN are finite adjustments
# rather than cells.
_SYMBOL_CELLS = ('KEYWORD', 'OTHER-SYMBOL')


def _sort_universe_bulk(sort):
    if sort == SORT_INTEGER:
        return IntBulk.universe()
    if sort == SORT_RATIO:
        return DenseBulk('RATIO', [(None, False, None, False)])
    if sort == SORT_FLOAT:
        return DenseBulk('FLOAT', [(None, False, None, False)])
    if sort == SORT_COMPLEX:
        return ComplexBulk(True)
    if sort == SORT_CHARACTER:
        return CellBulk(_CHAR_CELLS)
    if sort == SORT_SYMBOL:
        return CellBulk(_SYMBOL_CELLS)
    if sort == SORT_CONS:
        return ConsBulk.universe()
    if sort == SORT_ARRAY:
        return ArrayBulk.universe()
    if sort == SORT_CLASS:
        return CellBulk(_all_class_cells())
    raise AssertionError(sort)


def _sort_empty_bulk(sort):
    if sort == SORT_INTEGER:
        return IntBulk()
    if sort == SORT_RATIO:
        return DenseBulk('RATIO')
    if sort == SORT_FLOAT:
        return DenseBulk('FLOAT')
    if sort == SORT_COMPLEX:
        return ComplexBulk(False)
    if sort in (SORT_CHARACTER, SORT_SYMBOL, SORT_CLASS):
        return CellBulk()
    if sort == SORT_CONS:
        return ConsBulk()
    if sort == SORT_ARRAY:
        return ArrayBulk()
    raise AssertionError(sort)


# ---------------------------------------------------------------------------
# The class sort: named built-in classes plus every CLOS / condition /
# structure class currently defined
# ---------------------------------------------------------------------------

# Parent -> children for the built-in classes that are not covered by another
# sort. `classes.py`'s own `_init_builtin_classes` makes every built-in class a
# direct subclass of T, i.e. it records no hierarchy at all (CLAUDE.md notes
# this, and it is why CLOS method specificity cannot order INTEGER before
# NUMBER). This table is the hierarchy; it is kept here so that there is one
# copy of it rather than one per consumer.
# Values are *tuples* because the metaobject classes genuinely inherit from more
# than one place (CLHS 4.3.7 / 22.2): a STANDARD-GENERIC-FUNCTION is both a
# FUNCTION and a STANDARD-OBJECT, and CLASS, METHOD and METHOD-COMBINATION are
# STANDARD-OBJECTs -- which ansi-test's `*subtype-table*` asserts directly, with
# rows `(class standard-object)` and `(method standard-object)`.
_CLASS_PARENTS = {
    'COMPILED-FUNCTION': ('FUNCTION',),
    'GENERIC-FUNCTION': ('FUNCTION', 'STANDARD-OBJECT'),
    'STANDARD-GENERIC-FUNCTION': ('GENERIC-FUNCTION',),
    'METHOD': ('STANDARD-OBJECT',),
    'STANDARD-METHOD': ('METHOD',),
    'METHOD-COMBINATION': ('STANDARD-OBJECT',),
    'CLASS': ('STANDARD-OBJECT',),
    'BUILT-IN-CLASS': ('CLASS',),
    'STANDARD-CLASS': ('CLASS',),
    'STRUCTURE-CLASS': ('CLASS',),
    # The condition metaclass (classes.py's _BUILTIN_CLASS_TABLE): a
    # condition class object's cell is its metaclass name, so without this
    # row `(typep (find-class 'condition) 'class)` would lose the CLASS
    # ancestor. It is deliberately *not* under STANDARD-CLASS or
    # BUILT-IN-CLASS -- that exclusion is what
    # all-standard-classes-are-subtypes-of-standard-object and
    # slot-boundp.error.5/slot-makunbound.error.4 both turn on.
    'CONDITION-CLASS': ('CLASS',),
    'LOGICAL-PATHNAME': ('PATHNAME',),
    'BROADCAST-STREAM': ('STREAM',),
    'CONCATENATED-STREAM': ('STREAM',),
    'ECHO-STREAM': ('STREAM',),
    'FILE-STREAM': ('STREAM',),
    'STRING-STREAM': ('STREAM',),
    'SYNONYM-STREAM': ('STREAM',),
    'TWO-WAY-STREAM': ('STREAM',),
}

_CLASS_ROOTS = (
    'FUNCTION', 'HASH-TABLE', 'PACKAGE', 'PATHNAME', 'STREAM', 'READTABLE',
    'RANDOM-STATE', 'RESTART', 'STRUCTURE-OBJECT', 'STANDARD-OBJECT',
)

_BUILTIN_CLASS_NAMES = tuple(_CLASS_ROOTS) + tuple(_CLASS_PARENTS)


def _cell_key(node):
    """A *hashable* identity for a class cell.

    Cells go into a `frozenset`, and a `classes.LispClass` is unhashable
    (it defines `__eq__` without `__hash__`, so Python sets `__hash__` to None).
    Putting the objects in directly made every use of a class object as a type
    specifier answer `TypeError: unhashable type: 'LispClass'` *as the value of
    the Lisp form* -- standing rule 2, and it broke every ansi-test that passes
    `(find-class ...)` to SUBTYPEP. A CLOS class is identified by object
    identity (two DEFCLASSes of the same name are the same cell only if they are
    the same object), so `id` is the right key; the objects stay alive in the
    class registry, so it stays stable.
    """
    if isinstance(node, str) or isinstance(node, type):
        return node
    return ('CLOS', id(node))


def _condition_cells():
    """Every condition class, as cells. The condition hierarchy is real Python
    inheritance in `lisptype_extended.py`, so it needs no table here."""
    from fclpy.lispfunc import evaluation_conditions as _conditions
    classes = set()
    registry = getattr(_conditions, '_USER_CONDITION_CLASSES', None)
    if isinstance(registry, dict):
        for value in registry.values():
            if isinstance(value, type):
                classes.add(value)
    classes.add(lisptype.Condition)
    stack = list(classes)
    while stack:
        cls = stack.pop()
        for sub in cls.__subclasses__():
            if sub not in classes:
                classes.add(sub)
                stack.append(sub)
    return classes


def _clos_classes():
    from fclpy import classes as _classes
    found = []
    registry = getattr(getattr(_classes, '_class_registry', None), '_classes', None)
    if isinstance(registry, dict):
        for value in registry.values():
            if isinstance(value, _classes.LispClass) and value not in found:
                found.append(value)
    return found


def _clos_cells():
    return {_cell_key(cls) for cls in _clos_classes()}


_class_cell_cache = None
_class_cell_cache_key = None


def _all_class_cells():
    """Every cell of the CLASS sort.

    Memoized on the *number* of defined condition and CLOS classes, because
    `top()` is rebuilt on every complement and ansi-test's transitivity test
    alone runs some eleven thousand SUBTYPEP calls. The count is a sufficient
    key: classes are only ever added, never mutated, so a changed population
    always changes the count.
    """
    global _class_cell_cache, _class_cell_cache_key
    key = _class_population_key()
    if _class_cell_cache is not None and _class_cell_cache_key == key:
        return _class_cell_cache
    try:
        conditions = _condition_cells()
    except Exception:
        conditions = set()
    try:
        clos = _clos_cells()
    except Exception:
        clos = set()
    cells = set(_BUILTIN_CLASS_NAMES) | {_cell_key(c) for c in conditions} | clos
    _class_cell_cache = cells
    _class_cell_cache_key = key
    return cells


def _class_population_key():
    """An O(1) fingerprint of the defined-class population.

    Deliberately computed *without* walking `__subclasses__` or the class
    registry, because this runs on the hot path: `top()` rebuilds the universal
    region on every complement, `check-equivalence` complements twelve times per
    call, and ansi-test's transitivity test alone drives some eleven thousand
    SUBTYPEP calls. Computing the cells and then comparing them would have made
    the cache pure overhead.
    """
    try:
        from fclpy.lispfunc import evaluation_conditions as _conditions
        conditions = len(getattr(_conditions, '_USER_CONDITION_CLASSES', ()) or ())
    except Exception:
        conditions = 0
    try:
        from fclpy import classes as _classes
        registry = getattr(getattr(_classes, '_class_registry', None), '_classes', None)
        clos = len(registry) if isinstance(registry, dict) else 0
    except Exception:
        clos = 0
    return (conditions, clos)


def _class_cone(node):
    """Every cell at or below `node` in the class sort, as hashable cell keys.

    This is what makes class subtyping work in all three of the class
    representations that coexist here (plan.md Finding L): a built-in name, a
    Python condition class, and a CLOS `LispClass`.
    """
    cells = set()
    if isinstance(node, str):
        cells.add(node)
        for child, parents in _CLASS_PARENTS.items():
            if node in parents:
                cells |= _class_cone(child)
        # Every name in `_BUILTIN_CLASS_NAMES` is also a real registered
        # `classes.LispClass` object (classes.py's `_init_builtin_classes`),
        # not just a cell in this string-keyed table -- so which CLOS classes
        # actually descend from *this* name is a real ancestry question, not
        # "every CLOS class there is". (The old blanket `cells |=
        # _clos_cells()` happened to agree only while every CLOS class
        # descended from STANDARD-OBJECT; once DEFSTRUCT grew its own
        # LispClass hierarchy rooted at STRUCTURE-OBJECT, unioning in every
        # CLOS cell would also mark it a STANDARD-OBJECT subtype, and vice
        # versa, when the two are meant to be disjoint -- see
        # structures/structure-00.lsp's *disjoint-types-list*.) A user
        # subclass of a built-in class -- `(defclass
        # substandard-generic-function (standard-generic-function) ...)`,
        # defgeneric.30 -- is a CLOS cell the string table knows nothing
        # about, and its instances land in that cell (`_class_cell_of`
        # records the class a DEFGENERIC's :generic-function-class option
        # named), so the cone of the *string* must contain it for
        # `(typep fn 'standard-generic-function)` to stay T while
        # `(typep fn 'substandard-generic-function)` becomes T too.
        from fclpy import classes as _classes
        root_class = _classes.find_class(node)
        if root_class is not None:
            for candidate in _clos_classes():
                try:
                    supers = candidate.get_linearized_superclasses()
                except Exception:
                    continue
                if any(sup is root_class for sup in supers):
                    cells.add(_cell_key(candidate))
        return cells
    if isinstance(node, type):
        seen = {node}
        stack = [node]
        while stack:
            cls = stack.pop()
            for sub in cls.__subclasses__():
                if sub not in seen:
                    seen.add(sub)
                    stack.append(sub)
        return {_cell_key(cls) for cls in seen}
    from fclpy import classes as _classes
    if isinstance(node, _classes.LispClass):
        cells.add(_cell_key(node))
        for candidate in _clos_classes():
            try:
                supers = candidate.get_linearized_superclasses()
            except Exception:
                continue
            if any(sup is node for sup in supers):
                cells.add(_cell_key(candidate))
        return cells
    return cells


def _cell_of_object(obj):
    """The partition cell an object belongs to, or None if it is not in a
    cell-partitioned sort."""
    sort = _object_sort(obj)
    if sort == SORT_CHARACTER:
        from fclpy.lispfunc.characters import standard_char_p
        try:
            standard = standard_char_p(obj) == lisptype.T
        except Exception:
            standard = False
        return 'STANDARD-CHAR' if standard else 'OTHER-CHAR'
    if sort == SORT_SYMBOL:
        return 'KEYWORD' if isinstance(obj, lisptype.lispKeyword) else 'OTHER-SYMBOL'
    if sort == SORT_CLASS:
        return _class_cell_of(obj)
    return None


def _class_cell_of(obj):
    from fclpy import classes as _classes
    if isinstance(obj, _classes.LispInstance):
        return _cell_key(obj.lisp_class)
    if isinstance(obj, lisptype.Condition):
        return _cell_key(type(obj))
    if isinstance(obj, _classes.LispClass):
        # A class object is an instance of its *metaclass* (CLHS 4.3.7):
        # a DEFCLASS's product of STANDARD-CLASS, a DEFSTRUCT's of
        # STRUCTURE-CLASS (`evaluation_special_forms.py` records exactly
        # that), the built-in type classes of BUILT-IN-CLASS. This is the
        # same rule `classes.class_of` applies to a class object, so TYPEP
        # and CLASS-OF cannot disagree about it -- the hardcoded
        # 'STANDARD-CLASS' here made `(typep (find-class 's)
        # 'structure-class)` NIL for every DEFSTRUCT, which is what
        # ansi-test's STRUCT-TEST-nn/14 and STRUCTURE-1-13 assert.
        return getattr(obj, 'metaclass_name', 'STANDARD-CLASS')
    if isinstance(obj, _classes.GenericFunction):
        # A generic function is an instance of the class its DEFGENERIC's
        # :generic-function-class option named (CLHS 7.7) -- STANDARD-GENERIC-
        # FUNCTION when the option is absent. Defgeneric.30 typep's the object
        # against both, so the recorded class must be the one CLASS-OF answers
        # -- and this is the *cell* CLASS-OF answers, not merely the name:
        # TYPEP decides by cell membership, so recording only a name here made
        # `(typep fn 'substandard-generic-function)` NIL even while CLASS-OF
        # answered the right class and `(typep fn 'standard-generic-function)`
        # stayed T (the name's cone reaches the subclass cell through the
        # ancestry walk in `_class_cone`).
        gf_class = getattr(obj, 'gf_class', None)
        if gf_class is not None:
            return _cell_key(gf_class)
        return 'STANDARD-GENERIC-FUNCTION'
    from fclpy.lispfunc.misc_hashtables import is_hash_table
    if is_hash_table(obj):
        return 'HASH-TABLE'
    if isinstance(obj, lisptype.Package):
        return 'PACKAGE'
    from fclpy.readtable import Readtable
    if isinstance(obj, Readtable):
        return 'READTABLE'
    try:
        from fclpy.lispfunc.utilities_system import RandomState
        if isinstance(obj, RandomState):
            return 'RANDOM-STATE'
    except Exception:
        pass
    # RESTART and PATHNAME are `_CLASS_ROOTS` like HASH-TABLE and PACKAGE
    # above, but had no cell here, so every restart and every pathname fell
    # through to STRUCTURE-OBJECT: `(typep r 'restart)` and `(typep p
    # 'pathname)` were NIL, and `(type-of r)` was T. TYPEP delegates a
    # *symbol* specifier to `type_contains` before ever reaching
    # `comparison.py`'s ladder, so the RESTART branch that ladder does have
    # was dead for `(typep r 'restart)` and live only for the string
    # spelling -- the two disagreed about the same object (standing rule 3).
    if isinstance(obj, lisptype.Restart):
        return 'RESTART'
    from fclpy.lispfunc.pathnames import Pathname
    if isinstance(obj, Pathname):
        # A logical pathname is a *subtype* here (`_CLASS_PARENTS`:
        # LOGICAL-PATHNAME under PATHNAME), so the two names must land in
        # different cells or `(typep p 'logical-pathname)` is NIL for every
        # logical pathname.
        return 'LOGICAL-PATHNAME' if getattr(obj, 'logical', False) else 'PATHNAME'
    try:
        # A stream is an instance of exactly one of the CLHS 21.2 stream
        # classes; `stream_type_matches` is the same object model STREAMP
        # answers from, so the cell and the predicate cannot disagree. The
        # concrete subtypes are asked first so the cell is the most specific
        # class; a bare base `Stream` (the console streams) is a STREAM.
        from fclpy.lispfunc.streams import Stream, stream_type_matches
        for _stream_type in ('STRING-STREAM', 'FILE-STREAM', 'TWO-WAY-STREAM',
                             'ECHO-STREAM', 'CONCATENATED-STREAM',
                             'BROADCAST-STREAM', 'SYNONYM-STREAM'):
            if stream_type_matches(obj, _stream_type):
                return _stream_type
        if isinstance(obj, Stream):
            return 'STREAM'
    except Exception:
        pass
    if isinstance(obj, _classes.Method):
        # A DEFMETHOD product is a STANDARD-METHOD (CLHS 7.6.2) --
        # METHOD/STANDARD-METHOD both answer T for it through `_CLASS_PARENTS`.
        return 'STANDARD-METHOD'
    if callable(obj):
        # A DEFINE-CONDITION :READER is a generic function (CLHS 9.4) but is
        # not a `classes.GenericFunction` -- that object model is not wired
        # into FUNCALL/APPLY (plan.md Finding L), so the reader is a plain
        # callable carrying the marker `_make_condition_reader` sets.
        # `comparison.py`'s GENERIC-FUNCTION branch already reads that marker
        # and is dead for a *symbol* specifier, which is the spelling
        # `condition-27-reader-is-generic` uses; deciding it here is what
        # makes the two agree. It is not a STANDARD-GENERIC-FUNCTION: this
        # cell sits below GENERIC-FUNCTION and beside it.
        if getattr(obj, '_condition_reader_generic', False):
            return 'GENERIC-FUNCTION'
        # COMPILED-FUNCTION-P's own answer (everything callable here is
        # Python, so a function object with a code object is a compiled
        # function): the cell must agree with the predicate or
        # `check-type-predicate` collects the mismatch. FUNCTION still
        # contains this cell (`_CLASS_PARENTS`).
        if hasattr(obj, '__code__'):
            return 'COMPILED-FUNCTION'
        return 'FUNCTION'
    return 'STRUCTURE-OBJECT'


def _object_sort(obj):
    """Which sort an object lives in. Order matters: a one-character Python
    `str` is a CHARACTER here (matching TYPEP), even though it is also one of
    the shapes `arrays.is_array` accepts."""
    if _is_nil(obj):
        return SORT_SYMBOL
    if isinstance(obj, bool):
        return SORT_CLASS
    if isinstance(obj, int):
        return SORT_INTEGER
    if isinstance(obj, Fraction):
        return SORT_INTEGER if obj.denominator == 1 else SORT_RATIO
    if isinstance(obj, float):
        return SORT_FLOAT
    if isinstance(obj, complex):
        return SORT_COMPLEX
    if isinstance(obj, lisptype.Character):
        return SORT_CHARACTER
    if isinstance(obj, str) and len(obj) == 1:
        return SORT_CHARACTER
    if isinstance(obj, lisptype.LispSymbol):
        return SORT_SYMBOL
    if _cons_p(obj):
        return SORT_CONS
    from fclpy.lispfunc import arrays as _arrays
    if _arrays.is_array(obj):
        return SORT_ARRAY
    return SORT_CLASS


# ---------------------------------------------------------------------------
# SortSet -- a bulk region with finite EQL-keyed adjustments
# ---------------------------------------------------------------------------

def _bulk_definitely_nonempty(bulk):
    """Is this bulk certainly inhabited?

    Every bulk except ConsBulk decides emptiness exactly (`is_empty` is
    interval/cell arithmetic with no "unknown" answer), so for them
    certainly-inhabited is just not-empty. ConsBulk carries its own answer,
    because its rectangles are pruned on *possible* inhabitance only."""
    fn = getattr(bulk, 'is_definitely_nonempty', None)
    if fn is not None:
        return fn()
    return not bulk.is_empty()


class SortSet:
    """`(bulk \\ removed) u extra`, maintained so that `extra` is outside the
    bulk and `removed` inside it.

    The binary operations are all one function (`_combine`): do the operation
    on the bulks, then repair every object either operand mentions by asking
    what the answer *should* be there. That is correct by construction -- away
    from those finitely many objects a `SortSet` agrees with its bulk -- and it
    is a single implementation instead of three fiddly ones.
    """

    __slots__ = ('sort', 'bulk', 'extra', 'removed')

    def __init__(self, sort, bulk, extra=EMPTY_EQL_SET, removed=EMPTY_EQL_SET):
        self.sort = sort
        self.bulk = bulk
        self.extra = EqlSet([x for x in extra if not bulk.contains(x)])
        self.removed = EqlSet([x for x in removed if bulk.contains(x)])

    @staticmethod
    def empty(sort):
        return SortSet(sort, _sort_empty_bulk(sort))

    @staticmethod
    def universe(sort):
        return SortSet(sort, _sort_universe_bulk(sort))

    def is_empty(self):
        if self.extra:
            return False
        if self.bulk.is_empty():
            return True
        if not self.removed:
            return False
        finite = self.bulk.finite_elements()
        if finite is None:
            return False
        return all(self.removed.contains(x) for x in finite)

    def is_definitely_nonempty(self):
        """The mirror of `is_empty` for decisions that must not guess.

        A cons bulk whose rectangles depend on opaque components is neither
        certainly empty nor certainly inhabited, and reporting the latter is
        how SUBTYPEP once answered a certain NIL for subtypep.cons.44's
        random-predicate types -- the one answer CLHS 4.3.4 forbids there."""
        if self.extra:
            return True
        if not _bulk_definitely_nonempty(self.bulk):
            return False
        if not self.removed:
            return True
        finite = self.bulk.finite_elements()
        if finite is None:
            # an infinite inhabited bulk minus finitely many points stays
            # inhabited
            return True
        return any(not self.removed.contains(x) for x in finite)

    def contains(self, obj):
        if self.extra.contains(obj):
            return True
        return self.bulk.contains(obj) and not self.removed.contains(obj)

    def finite_elements(self):
        finite = self.bulk.finite_elements()
        if finite is None:
            return None
        kept = [x for x in finite if not self.removed.contains(x)]
        kept.extend(self.extra)
        return kept

    def _points(self, other):
        return (tuple(self.extra) + tuple(self.removed)
                + tuple(other.extra) + tuple(other.removed))

    def _combine(self, other, bulk_op, logical):
        bulk = bulk_op(self.bulk, other.bulk)
        extra, removed = [], []
        for point in self._points(other):
            want = logical(self.contains(point), other.contains(point))
            if want and not bulk.contains(point):
                extra.append(point)
            elif bulk.contains(point) and not want:
                removed.append(point)
        return SortSet(self.sort, bulk, EqlSet(extra), EqlSet(removed))

    def union(self, other):
        return self._combine(other, lambda a, b: a.union(b),
                             lambda x, y: x or y)

    def intersect(self, other):
        return self._combine(other, lambda a, b: a.intersect(b),
                             lambda x, y: x and y)

    def subtract(self, other):
        return self._combine(other, lambda a, b: a.subtract(b),
                             lambda x, y: x and not y)

    def complement(self):
        return SortSet.universe(self.sort).subtract(self)


# ---------------------------------------------------------------------------
# Region -- one SortSet per sort
# ---------------------------------------------------------------------------

class Region:
    """A decidable set of Lisp objects: a `SortSet` per sort.

    Sorts are disjoint by construction, so a region is their disjoint union and
    every operation is pointwise. This is where "an integer is never a symbol"
    comes from -- there is no rule for it, the two live in different slots.
    """

    __slots__ = ('sets',)

    def __init__(self, sets=None):
        self.sets = {k: v for k, v in (sets or {}).items() if not v.is_empty()}

    @staticmethod
    def empty():
        return Region()

    @staticmethod
    def universe():
        return Region({sort: SortSet.universe(sort) for sort in ALL_SORTS})

    @staticmethod
    def of_sort(sort, sort_set):
        return Region({sort: sort_set})

    def get(self, sort):
        return self.sets.get(sort) or SortSet.empty(sort)

    def is_empty(self):
        return not self.sets

    def is_definitely_nonempty(self):
        # sorts are disjoint, so the region is inhabited as soon as one
        # sort's slice certainly is
        return any(ss.is_definitely_nonempty() for ss in self.sets.values())

    def union(self, other):
        return Region({s: self.get(s).union(other.get(s)) for s in ALL_SORTS})

    def intersect(self, other):
        return Region({s: self.get(s).intersect(other.get(s)) for s in ALL_SORTS})

    def subtract(self, other):
        return Region({s: self.get(s).subtract(other.get(s)) for s in ALL_SORTS})

    def complement(self):
        return Region.universe().subtract(self)

    def contains(self, obj):
        sort_set = self.sets.get(_object_sort(obj))
        return bool(sort_set) and sort_set.contains(obj)

    def finite_elements(self):
        out = []
        for sort_set in self.sets.values():
            finite = sort_set.finite_elements()
            if finite is None:
                return None
            out.extend(finite)
        return out


# ---------------------------------------------------------------------------
# Opaque literals and the propositional layer
# ---------------------------------------------------------------------------

class Opaque:
    """A type this module will not guess about: `(satisfies f)`, a parameterized
    `(function ...)`, or a region operation that got too large.

    Identity is by *printed specifier*, so the same `(satisfies foo)` written
    twice is one literal and cancels -- which is what makes `(and X X)`
    equivalent to `X` even when X is uninterpretable.
    """

    __slots__ = ('key', 'predicate')

    def __init__(self, key, predicate=None):
        self.key = key
        self.predicate = predicate

    def __eq__(self, other):
        return isinstance(other, Opaque) and self.key == other.key

    def __hash__(self):
        return hash(self.key)

    def __repr__(self):
        return 'Opaque(%r)' % (self.key,)

    def test(self, obj):
        """Call the predicate. Only reached when the conjunct's decidable part
        is a finite set of concrete objects -- `(member a b c d)` against
        `(satisfies symbolp)` (ansi-test `subtypep.member.27`, which requires a
        *certain* T). Returns None when it cannot be called at all."""
        if self.predicate is None:
            return None
        from fclpy.lispfunc.evaluation_core import funcall
        try:
            result = funcall(self.predicate, obj)
        except Exception:
            return None
        return not (_is_nil(result) or result is False)


class Conjunct:
    """`region & (all of pos) & (none of neg)`."""

    __slots__ = ('region', 'pos', 'neg')

    def __init__(self, region, pos=frozenset(), neg=frozenset()):
        self.region = region
        self.pos = frozenset(pos)
        self.neg = frozenset(neg)

    def is_definitely_empty(self):
        if self.region.is_empty():
            return True
        if self.pos & self.neg:
            return True
        decided = self._decide_finitely()
        if decided is not None:
            return not decided
        return False

    def is_definitely_nonempty(self):
        if not self.region.is_definitely_nonempty():
            return False
        if not self.pos and not self.neg:
            return True
        decided = self._decide_finitely()
        if decided is not None:
            return bool(decided)
        return False

    def _decide_finitely(self):
        """Decide this conjunct exactly by testing every element, when the
        decidable part is a small finite set. Returns the surviving elements,
        or None when the region is not finite or a predicate is uncallable."""
        finite = self.region.finite_elements()
        if finite is None:
            return None
        survivors = []
        for obj in finite:
            ok = True
            for literal in self.pos:
                verdict = literal.test(obj)
                if verdict is None:
                    return None
                if not verdict:
                    ok = False
                    break
            if ok:
                for literal in self.neg:
                    verdict = literal.test(obj)
                    if verdict is None:
                        return None
                    if verdict:
                        ok = False
                        break
            if ok:
                survivors.append(obj)
        return survivors

    def contains(self, obj):
        if not self.region.contains(obj):
            return False
        for literal in self.pos:
            if literal.test(obj) is not True:
                return False
        for literal in self.neg:
            if literal.test(obj) is not False:
                return False
        return True

    def intersect(self, other):
        return Conjunct(self.region.intersect(other.region),
                        self.pos | other.pos, self.neg | other.neg)


# The number of conjuncts a Ctype may carry before it collapses to a single
# opaque literal. Only opaque literals create conjuncts, so this is never
# reached by an ordinary type.
_CONJUNCT_CAP = 64


class Ctype:
    """A parsed type: a disjunction of `Conjunct`s.

    With no opaque literals there is exactly one conjunct holding one `Region`,
    and every answer about the type is exact.
    """

    __slots__ = ('conjuncts',)

    def __init__(self, conjuncts=()):
        kept = [c for c in conjuncts if not c.region.is_empty()]
        if len(kept) > _CONJUNCT_CAP:
            raise _GiveUp('type too complex')
        self.conjuncts = tuple(kept)

    # -- constructors -----------------------------------------------------

    @staticmethod
    def empty():
        return Ctype()

    @staticmethod
    def universe():
        return Ctype([Conjunct(Region.universe())])

    @staticmethod
    def of_region(region):
        return Ctype([Conjunct(region)])

    @staticmethod
    def of_opaque(literal):
        return Ctype([Conjunct(Region.universe(), pos={literal})])

    # -- algebra ----------------------------------------------------------

    def is_definitely_empty(self):
        return all(c.is_definitely_empty() for c in self.conjuncts)

    def is_definitely_nonempty(self):
        return any(c.is_definitely_nonempty() for c in self.conjuncts)

    def union(self, other):
        return Ctype(self.conjuncts + other.conjuncts)

    def intersect(self, other):
        out = []
        for a in self.conjuncts:
            for b in other.conjuncts:
                out.append(a.intersect(b))
        return Ctype(out)

    def complement(self):
        """De Morgan: the complement of a disjunction is the intersection of the
        complements, and the complement of one conjunct is its region's
        complement *or* the negation of any one of its literals."""
        result = Ctype.universe()
        for conjunct in self.conjuncts:
            parts = [Conjunct(conjunct.region.complement())]
            for literal in conjunct.pos:
                parts.append(Conjunct(Region.universe(), neg={literal}))
            for literal in conjunct.neg:
                parts.append(Conjunct(Region.universe(), pos={literal}))
            result = result.intersect(Ctype(parts))
        return result

    def subtract(self, other):
        return self.intersect(other.complement())

    def contains(self, obj):
        return any(c.contains(obj) for c in self.conjuncts)


def top():
    """The type T.

    A function rather than a constant because the CLASS sort's universe is the
    set of classes that currently exist: DEFCLASS, DEFSTRUCT and
    DEFINE-CONDITION add cells, so a universe captured at import time would
    permanently omit every user type and `(subtypep '(not my-class) t)` would go
    wrong. Cheap enough to rebuild -- SUBTYPEP is not on a hot path.
    """
    return Ctype.universe()


def bottom():
    """The type NIL."""
    return Ctype.empty()


def _region_type(sort, sort_set):
    return Ctype.of_region(Region.of_sort(sort, sort_set))


# ---------------------------------------------------------------------------
# The parser
# ---------------------------------------------------------------------------

_REAL_SORTS = (SORT_INTEGER, SORT_RATIO, SORT_FLOAT)

_FLOAT_NAMES = ('FLOAT', 'SHORT-FLOAT', 'SINGLE-FLOAT',
                'DOUBLE-FLOAT', 'LONG-FLOAT')

# CLHS 4.2.3 atomic specifiers that are unions of other types.
_ATOMIC_UNIONS = {
    'NUMBER': (SORT_INTEGER, SORT_RATIO, SORT_FLOAT, SORT_COMPLEX),
    'REAL': (SORT_INTEGER, SORT_RATIO, SORT_FLOAT),
    'RATIONAL': (SORT_INTEGER, SORT_RATIO),
}


def parse_type(spec, environment=None, depth=0, discrimination=False):
    """A type specifier as a `Ctype`.

    Raises `LispTypeSpecError` when `spec` does not name a type at all -- the
    caller decides whether that is an error (TYPEP must signal) or merely
    undecided (SUBTYPEP may answer NIL NIL).

    `discrimination` marks a TYPEP-shaped use (CLHS 4.2.3 "discrimination"):
    the list form of the `function` type specifier "can be used only for
    declaration and not for discrimination", so it is an error there while
    SUBTYPEP (a declaration-shaped use) may still compare the parts.
    """
    if depth > 32:
        raise _GiveUp('type specifier nested too deeply')

    if _is_nil(spec):
        return bottom()
    if spec is lisptype.T:
        return top()

    if _cons_p(spec):
        return _parse_compound(spec, environment, depth, discrimination)

    # A class object used as a type specifier: `(find-class 'array)` appears
    # directly in ansi-test `subtypep.array.1`, and every CL class name must be
    # type-equivalent to its class object (`types-and-class.lsp`).
    if isinstance(spec, type) and issubclass(spec, lisptype.Condition):
        return _region_type(SORT_CLASS, SortSet(SORT_CLASS, CellBulk(_class_cone(spec))))
    from fclpy import classes as _classes
    if isinstance(spec, _classes.LispClass):
        name = _type_name(spec.name)
        if name is not None and _is_known_atomic(name, environment):
            return _parse_atomic(name, environment, depth, discrimination)
        return _region_type(SORT_CLASS, SortSet(SORT_CLASS, CellBulk(_class_cone(spec))))

    name = _type_name(spec)
    if name is None:
        raise LispTypeSpecError('not a type specifier: %r' % (spec,))
    return _parse_atomic(name, environment, depth, discrimination)


def _real_union(sorts, low=None, low_ex=False, high=None, high_ex=False):
    """A numeric type over one or more real sorts, with a shared interval.

    `(real 0 10)` is this over all three real sorts, `(rational 0 10)` over two
    and `(integer 0 10)` over one -- which is what makes
    `(and integer (real 4 10))` reduce to `(integer 4 10)` rather than needing a
    rule about REAL and INTEGER.
    """
    sets = {}
    for sort in sorts:
        if sort == SORT_INTEGER:
            bulk = IntBulk.interval(low, low_ex, high, high_ex)
        elif sort == SORT_COMPLEX:
            bulk = ComplexBulk(True)
        else:
            bulk = DenseBulk('RATIO' if sort == SORT_RATIO else 'FLOAT',
                             [(low, low_ex, high, high_ex)])
        sets[sort] = SortSet(sort, bulk)
    return Ctype.of_region(Region(sets))


def _bound(spec):
    """A numeric interval bound: `(value, exclusive)`, `(None, False)` for `*`.

    The bound is validated here rather than being handed to `math.floor` and
    friends, because a specifier like `(integer most-positive-fixnum)` -- the
    *symbol*, not its value -- would otherwise surface a Python
    `TypeError: must be real number, not LispSymbol` as the value of a Lisp form
    (standing rule 2). Raising `LispTypeSpecError` instead degrades it to
    SUBTYPEP's permitted "NIL NIL" and to a real TYPE-ERROR from TYPEP.
    """
    if spec is None or _is_wild(spec):
        return None, False
    if _cons_p(spec):
        inner = spec.car
        if _is_wild(inner):
            return None, False
        return _check_bound(inner), True
    if isinstance(spec, (list, tuple)) and len(spec) == 1:
        return _check_bound(spec[0]), True
    return _check_bound(spec), False


def _check_bound(value):
    if isinstance(value, bool) or not isinstance(value, (int, float, Fraction)):
        raise LispTypeSpecError('not a numeric interval bound: %r' % (value,))
    return value


def _interval_args(args):
    low, low_ex = _bound(args[0] if len(args) > 0 else None)
    high, high_ex = _bound(args[1] if len(args) > 1 else None)
    return low, low_ex, high, high_ex


def _int_interval(low, high):
    return _region_type(SORT_INTEGER,
                        SortSet(SORT_INTEGER, IntBulk.interval(low, False, high, False)))


def _cell_type(sort, cells):
    return _region_type(sort, SortSet(sort, CellBulk(cells)))


def _class_type(name):
    return _region_type(SORT_CLASS, SortSet(SORT_CLASS, CellBulk(_class_cone(name))))


def _eql_type(obj):
    """`(eql x)` -- a one-element type, placed in the sort `x` belongs to.

    Numbers land in an interval, so `(and integer (not (eql 10)))` comes out as
    two integer intervals; everything else lands in its sort's finite
    `extra` set.
    """
    sort = _object_sort(obj)
    if sort == SORT_INTEGER:
        value = int(obj) if isinstance(obj, Fraction) else obj
        bulk = IntBulk([(value, value)])
        return _region_type(sort, SortSet(sort, bulk))
    if sort in (SORT_RATIO, SORT_FLOAT):
        bulk = DenseBulk('RATIO' if sort == SORT_RATIO else 'FLOAT',
                         [(obj, False, obj, False)])
        return _region_type(sort, SortSet(sort, bulk))
    return _region_type(sort, SortSet(sort, _sort_empty_bulk(sort), EqlSet([obj])))


def _is_known_atomic(name, environment):
    try:
        _parse_atomic(name, environment, 0)
    except (LispTypeSpecError, _GiveUp):
        return False
    return True


def _parse_atomic(name, environment, depth, discrimination=False):
    if name == 'T':
        return top()
    if name in ('NIL', 'EXTENDED-CHAR'):
        # EXTENDED-CHAR is empty because fclpy has one character representation,
        # so BASE-CHAR is CHARACTER (CLHS 4.4 permits exactly this).
        return bottom()

    if name in _ATOMIC_UNIONS:
        return _real_union(_ATOMIC_UNIONS[name])
    if name == 'INTEGER' or name == 'SIGNED-BYTE':
        # `*subtype-table*` records SIGNED-BYTE and INTEGER as mutual subtypes.
        return _real_union((SORT_INTEGER,))
    if name == 'UNSIGNED-BYTE':
        return _int_interval(0, None)
    if name == 'BIT':
        return _int_interval(0, 1)
    if name == 'FIXNUM':
        return _int_interval(MOST_NEGATIVE_FIXNUM, MOST_POSITIVE_FIXNUM)
    if name == 'BIGNUM':
        return _real_union((SORT_INTEGER,)).subtract(_parse_atomic('FIXNUM', environment, depth))
    if name == 'RATIO':
        return _real_union((SORT_RATIO,))
    if name in _FLOAT_NAMES:
        return _real_union((SORT_FLOAT,))
    if name == 'COMPLEX':
        return _real_union((SORT_COMPLEX,))

    if name in ('CHARACTER', 'BASE-CHAR'):
        return _cell_type(SORT_CHARACTER, _CHAR_CELLS)
    if name == 'STANDARD-CHAR':
        return _cell_type(SORT_CHARACTER, ('STANDARD-CHAR',))

    if name == 'SYMBOL':
        return _cell_type(SORT_SYMBOL, _SYMBOL_CELLS)
    if name == 'KEYWORD':
        return _cell_type(SORT_SYMBOL, ('KEYWORD',))
    if name == 'NULL':
        return _eql_type(lisptype.NIL)
    if name == 'BOOLEAN':
        return _eql_type(lisptype.NIL).union(_eql_type(lisptype.T))

    if name == 'CONS':
        return _region_type(SORT_CONS, SortSet(SORT_CONS, ConsBulk.universe()))
    if name == 'LIST':
        return _parse_atomic('CONS', environment, depth,
                             discrimination).union(_eql_type(lisptype.NIL))
    if name == 'ATOM':
        return _parse_atomic('CONS', environment, depth,
                             discrimination).complement()
    if name == 'SEQUENCE':
        return (_parse_atomic('LIST', environment, depth)
                .union(_array_type('VECTOR', (), environment, depth)))

    from fclpy.lispfunc import arrays as _arrays
    if _arrays.is_array_type_name(name):
        return _array_type(name, (), environment, depth)

    if name in _BUILTIN_CLASS_NAMES:
        return _class_type(name)
    if name == 'CONDITION':
        return _region_type(SORT_CLASS,
                            SortSet(SORT_CLASS, CellBulk(_class_cone(lisptype.Condition))))

    expansion = _deftype_expansion(name, (), environment)
    if expansion is not None:
        return parse_type(expansion, environment, depth + 1)

    condition_class = _condition_class(name)
    if condition_class is not None:
        return _region_type(SORT_CLASS,
                            SortSet(SORT_CLASS, CellBulk(_class_cone(condition_class))))

    clos_class = _find_clos_class(name)
    if clos_class is not None:
        return _region_type(SORT_CLASS,
                            SortSet(SORT_CLASS, CellBulk(_class_cone(clos_class))))

    raise LispTypeSpecError('unknown type: %s' % (name,))


def _condition_class(name):
    from fclpy.lispfunc.evaluation_conditions import _condition_class_for_name
    try:
        cls = _condition_class_for_name(name)
    except Exception:
        return None
    return cls if isinstance(cls, type) else None


def _find_clos_class(name):
    from fclpy import classes as _classes
    try:
        cls = _classes.find_class(name)
    except Exception:
        return None
    return cls if isinstance(cls, _classes.LispClass) else None


def _parse_compound(spec, environment, depth, discrimination=False):
    head = _type_name(spec.car)
    args = _lisp_list(spec.cdr)

    if head is None:
        raise LispTypeSpecError('not a type specifier: %r' % (spec,))

    if head == 'AND':
        result = top()
        for arg in args:
            result = result.intersect(parse_type(arg, environment, depth + 1,
                                                 discrimination))
        return result
    if head == 'OR':
        result = bottom()
        for arg in args:
            result = result.union(parse_type(arg, environment, depth + 1,
                                             discrimination))
        return result
    if head == 'NOT':
        if len(args) != 1:
            raise LispTypeSpecError('(NOT ...) takes one type')
        return parse_type(args[0], environment, depth + 1,
                          discrimination).complement()

    if head == 'MEMBER':
        result = bottom()
        for arg in args:
            result = result.union(_eql_type(arg))
        return result
    if head == 'EQL':
        if len(args) != 1:
            raise LispTypeSpecError('(EQL ...) takes one object')
        return _eql_type(args[0])

    if head == 'SATISFIES':
        if len(args) != 1:
            raise LispTypeSpecError('(SATISFIES ...) takes one predicate name')
        return Ctype.of_opaque(Opaque(('SATISFIES', _opaque_key(args[0])), args[0]))

    # `(signed-byte)` and `(signed-byte *)` are handled by the SIGNED-BYTE branch
    # below, which already answers "every integer" for a missing size.
    if head == 'INTEGER':
        low, low_ex, high, high_ex = _interval_args(args)
        return _real_union((SORT_INTEGER,), low, low_ex, high, high_ex)
    if head == 'RATIONAL':
        low, low_ex, high, high_ex = _interval_args(args)
        return _real_union((SORT_INTEGER, SORT_RATIO), low, low_ex, high, high_ex)
    if head == 'REAL':
        low, low_ex, high, high_ex = _interval_args(args)
        return _real_union(_REAL_SORTS, low, low_ex, high, high_ex)
    if head == 'RATIO':
        low, low_ex, high, high_ex = _interval_args(args)
        return _real_union((SORT_RATIO,), low, low_ex, high, high_ex)
    if head in _FLOAT_NAMES:
        low, low_ex, high, high_ex = _interval_args(args)
        return _real_union((SORT_FLOAT,), low, low_ex, high, high_ex)

    if head == 'MOD':
        if len(args) != 1 or not isinstance(args[0], int):
            raise LispTypeSpecError('(MOD n) requires a positive integer')
        return _int_interval(0, args[0] - 1)
    if head == 'UNSIGNED-BYTE':
        size = args[0] if args else None
        if size is None or _is_wild(size):
            return _int_interval(0, None)
        return _int_interval(0, 2 ** size - 1)
    if head == 'SIGNED-BYTE':
        size = args[0] if args else None
        if size is None or _is_wild(size):
            return _real_union((SORT_INTEGER,))
        limit = 2 ** (size - 1)
        return _int_interval(-limit, limit - 1)

    if head == 'COMPLEX':
        part = args[0] if args else None
        if part is None or _is_wild(part):
            return _real_union((SORT_COMPLEX,))
        if parse_type(part, environment, depth + 1,
                      discrimination).is_definitely_empty():
            return bottom()
        return _real_union((SORT_COMPLEX,))

    if head == 'CONS':
        car_spec = args[0] if len(args) > 0 else None
        cdr_spec = args[1] if len(args) > 1 else None
        car_type = (top() if car_spec is None or _is_wild(car_spec)
                    else parse_type(car_spec, environment, depth + 1,
                                    discrimination))
        cdr_type = (top() if cdr_spec is None or _is_wild(cdr_spec)
                    else parse_type(cdr_spec, environment, depth + 1,
                                    discrimination))
        if car_type.is_definitely_empty() or cdr_type.is_definitely_empty():
            # ansi-test subtypep.cons.2: `(cons nil t)` is the empty type, and
            # the bottom must propagate out of the product rather than
            # producing a cons type with an impossible car.
            return bottom()
        return _region_type(SORT_CONS,
                            SortSet(SORT_CONS, ConsBulk([(car_type, cdr_type)])))

    from fclpy.lispfunc import arrays as _arrays
    if _arrays.is_array_type_name(head):
        return _array_type(head, args, environment, depth)

    if head == 'FUNCTION':
        # CLHS 4.2.3 leaves `(function ...)` subtyping to the implementation and
        # explicitly allows SUBTYPEP to punt; treated as an opaque refinement of
        # FUNCTION so that `(function (t) integer)` is still known to be a
        # function and still cancels against itself.
        #
        # The *list* form "can be used only for declaration and not for
        # discrimination" (CLHS System Class FUNCTION) -- so a TYPEP-shaped
        # use raises, which `comparison.typep` turns into a TYPE-ERROR, while
        # SUBTYPEP keeps comparing the parts (subtypep-function.3/.4 need
        # those answers).
        if discrimination:
            raise LispTypeSpecError(
                'the list form of the function type specifier can be used '
                'only for declaration and not for discrimination: %s' % (head,))
        base = _class_type('FUNCTION')
        if not args:
            return base
        return base.intersect(Ctype.of_opaque(Opaque(('FUNCTION', _opaque_key(spec)))))

    if head == 'VALUES':
        raise LispTypeSpecError('VALUES is not a type specifier for TYPEP/SUBTYPEP')

    expansion = _deftype_expansion(head, args, environment)
    if expansion is not None:
        return parse_type(expansion, environment, depth + 1, discrimination)

    raise LispTypeSpecError('unknown compound type: %s' % (head,))


def _opaque_key(spec):
    """A stable key for an uninterpretable specifier, so two spellings of the
    same thing are one literal."""
    from fclpy.lispfunc import io_write
    try:
        return str(io_write.write_to_string(spec))
    except Exception:
        return repr(spec)


# ---------------------------------------------------------------------------
# Array type specifiers
# ---------------------------------------------------------------------------

def _array_type(name, args, environment, depth):
    """One of the ten array type specifiers, as a `Ctype`.

    The definitional identities ansi-test checks -- `simple-vector` is
    `(simple-array t (*))`, `base-string` is `(vector base-char)`,
    `vector` is `(array * (*))` -- are *not* special cases here. They fall out
    of every array name reducing to the same (simplicity, element type,
    dimensions) triple.
    """
    args = list(args)
    simple_required = name.startswith('SIMPLE-')
    base = name[len('SIMPLE-'):] if simple_required else name

    # Only `array`/`simple-array` and the *non-simple* `vector` take an element
    # type first. `(simple-vector size)` takes a size, because its element type
    # is already fixed to T (CLHS 4.2.3) -- reading its argument as an element
    # type made `(simple-vector 17)` mean "a simple vector of 17s", so it was not
    # recognised as `(simple-array t (17))`.
    if base == 'ARRAY' or name == 'VECTOR':
        etype_spec = args[0] if args else None
        dim_spec = args[1] if len(args) > 1 else None
    else:
        etype_spec = None
        dim_spec = args[0] if args else None

    # element type
    if base == 'BIT-VECTOR':
        etypes = ('BIT',)
    elif base == 'STRING':
        # CLHS 15.1: a string is a specialized array whose element type is a
        # subtype of CHARACTER, and NIL is a subtype of every type -- so
        # STRING is CHARACTER's region *and* NIL's, not CHARACTER's alone
        # (`*-is-not-vector-of-character.*`'s `:nil-vectors-are-strings`
        # tests). `(vector base-char)`/`base-string` carry no such clause
        # below because a *specific* element-type argument, unlike the bare
        # name STRING, resolves through the generic `etype_spec` branch and
        # never includes NIL on its own.
        etypes = ('CHARACTER', 'NIL')
    elif base == 'BASE-STRING':
        etypes = ('CHARACTER',)
    elif name == 'SIMPLE-VECTOR':
        etypes = ('T',)
    elif etype_spec is None or _is_wild(etype_spec):
        etypes = _ARRAY_ETYPES
    else:
        from fclpy.lispfunc import arrays as _arrays
        etypes = (_etype_key(_arrays.upgraded_element_type(etype_spec)),)

    simplicities = (True,) if simple_required else (True, False)

    dims = _dim_set(base, dim_spec)
    if dims.is_empty():
        return bottom()

    by_key = {}
    for simple in simplicities:
        for etype in etypes:
            by_key[(simple, etype)] = dims
    return _region_type(SORT_ARRAY, SortSet(SORT_ARRAY, ArrayBulk(by_key)))


def _dim_set(base, dim_spec):
    """The dimension argument of an array specifier, as a `DimSet`.

    Three shapes share this argument position and they mean different things:
    for `array` an integer is a *rank*, for a vector type an integer is a
    *size*, and NIL is the empty dimension list, i.e. rank zero -- not `*`.
    Conflating NIL with `*` is what would make `(array t nil)` a supertype of
    every array instead of the rank-0 arrays (ansi-test `subtypep.array.10`).
    """
    if base != 'ARRAY':
        # every vector type is rank 1
        if dim_spec is None or _is_wild(dim_spec):
            return DimSet(per_rank={1: (_axes_universe(1),)})
        if isinstance(dim_spec, int) and not isinstance(dim_spec, bool):
            return DimSet(per_rank={1: ((IntBulk([(dim_spec, dim_spec)]),),)})
        raise LispTypeSpecError('bad vector size: %r' % (dim_spec,))

    if dim_spec is None or _is_wild(dim_spec):
        return DimSet.universe()
    if _is_nil(dim_spec):
        return DimSet(per_rank={0: ((),)})
    if isinstance(dim_spec, int) and not isinstance(dim_spec, bool):
        return DimSet(per_rank={dim_spec: (_axes_universe(dim_spec),)})

    axes = []
    for axis in _lisp_list(dim_spec):
        if _is_wild(axis):
            axes.append(IntBulk([(0, None)]))
        elif isinstance(axis, int) and not isinstance(axis, bool):
            axes.append(IntBulk([(axis, axis)]))
        else:
            raise LispTypeSpecError('bad array dimension: %r' % (axis,))
    return DimSet(per_rank={len(axes): (tuple(axes),)})


# ---------------------------------------------------------------------------
# DEFTYPE
# ---------------------------------------------------------------------------

def _deftype_table(environment=None):
    """The DEFTYPE expander table, which lives on the global environment.

    `DEFTYPE` has always written here (`evaluation_core.py`'s DEFTYPE branch);
    nothing ever read it, so a user-defined type was invisible to both TYPEP
    and SUBTYPEP. This is the reader.
    """
    from fclpy import state
    env = environment
    if env is None or not hasattr(env, 'parent'):
        env = state.current_environment
    while env is not None and getattr(env, 'parent', None) is not None:
        env = env.parent
    return getattr(env, 'user_types', None) or {}


def _deftype_expansion(name, args, environment):
    """Expand a DEFTYPE'd name, or None if there is no such type.

    An unsupplied `&optional`/`&key` parameter of a deftype lambda list defaults
    to `*`, not NIL (CLHS 4.2.3) -- which is what makes
    `(deftype foo (&optional x) `(integer 0 ,x))` name `(integer 0 *)` when
    written bare, i.e. UNSIGNED-BYTE (ansi-test `deftype.9`).
    """
    table = _deftype_table(environment)
    entry = table.get(name)
    if entry is None:
        return None
    from fclpy.lispfunc.evaluation_core import expand_deftype
    return expand_deftype(entry, list(args))


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

def type_contains(obj, spec, environment=None):
    """TYPEP: is `obj` of type `spec`?

    This is the *discrimination* face of `parse_type` (CLHS 4.2.3): the list
    form of the `function` type specifier is declaration-only, so asking
    TYPEP about it signals rather than answers. SUBTYPEP parses with
    `discrimination=False` and may still compare the parts."""
    return parse_type(spec, environment, discrimination=True).contains(obj)


def type_subtypep(spec1, spec2, environment=None):
    """SUBTYPEP: `(subtype-p, certain)` as a pair of Python bools (CLHS 4.3.4).

    Decided as emptiness of `spec1 \\ spec2`. "Certain" is false exactly when
    an `Opaque` literal or a size cap left the difference undecided -- never
    when the answer is merely NIL.
    """
    try:
        type1 = parse_type(spec1, environment)
        type2 = parse_type(spec2, environment)
    except (LispTypeSpecError, _GiveUp):
        return False, False

    try:
        difference = type1.subtract(type2)
    except _GiveUp:
        return False, False

    if difference.is_definitely_empty():
        return True, True
    if difference.is_definitely_nonempty():
        return False, True
    return False, False
