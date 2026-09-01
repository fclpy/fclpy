"""The divide-then-round family: exact arithmetic, two values, and REM's sign.

All eight of FLOOR/CEILING/TRUNCATE/ROUND and their F- variants computed
``x / divisor`` -- Python **float** division -- before rounding. Two defects
followed, and the first one is why this module exists rather than leaving the
coverage to ansi-test:

1. **Precision above 2**53 was silently lost**, and it manifested as a *hang*
   rather than a wrong answer. `integer-binary-search`
   (ansi-test ``auxiliary/numbers-aux.lsp:46``) steps with
   ``(ceiling (+ lo hi) 2)``; once ``lo`` passed 2**53 the midpoint rounded
   back to ``lo``, ``(setq lo mid)`` became a no-op, and the loop ran until the
   600s watchdog killed it -- 1,335,702 iterations, 15% of the entire ANSI
   run's wall time, reached from ``numbers/sqrt.lsp``'s
   ``(find-largest-exactly-floatable-integer most-positive-fixnum)``.
   A hang is not a clean failure signal, so it is pinned here where it fails
   fast and says why.

2. **They returned one value** where CLHS 12.2 requires *quotient and
   remainder*. ansi-test does cover this (every helper opens with
   ``(eql (length vals) 2)``), so only the shape is asserted here.

REM shared the defect from the other side: it was Python's ``%``, which is
floor-based -- right for MOD, wrong for REM whenever the operands differ in
sign. Both are now the remainder of the corresponding rounding operation.
"""

import io

import pytest

from fclpy import lispenv
from fclpy.lispfunc.evaluation_core import eval as lisp_eval
from fclpy.lispreader import LispReader, LispStream
from fclpy.readtable import get_current_readtable


@pytest.fixture(autouse=True)
def env():
    lispenv.setup_standard_environment()


def ev(source):
    import fclpy.state as state
    stream = LispStream(io.StringIO(source))
    readtable = get_current_readtable()
    form = LispReader(readtable.get_macro_character, stream).read_1()
    return lisp_eval(form, state.current_environment)


def evs(source):
    return str(ev(source))


BIG = 2 ** 62  # comfortably past a double's 53 bits of mantissa


class TestExactAboveDoublePrecision:
    """A rational operand must not be routed through a float."""

    def test_ceiling_of_an_odd_sum_rounds_up(self):
        assert evs(f"(nth-value 0 (ceiling {2 * BIG + 1} 2))") == str(BIG + 1)

    def test_floor_of_an_odd_sum_rounds_down(self):
        assert evs(f"(nth-value 0 (floor {2 * BIG + 1} 2))") == str(BIG)

    @pytest.mark.parametrize('op,want', [
        ('truncate', BIG), ('round', BIG), ('floor', BIG), ('ceiling', BIG + 1),
    ])
    def test_each_operator_is_exact(self, op, want):
        assert evs(f"(nth-value 0 ({op} {2 * BIG + 1} 2))") == str(want)

    def test_the_midpoint_strictly_advances(self):
        """The property `integer-binary-search` needs to terminate: with
        lo < hi, the CEILING midpoint must be greater than lo. Float division
        rounded it back to lo, so the search made no progress at all."""
        lo, hi = BIG, BIG + 1
        assert evs(f"(> (ceiling (+ {lo} {hi}) 2) {lo})") == 'T'

    def test_a_binary_search_over_a_large_range_terminates(self):
        """The reduced shape of the loop that burned the 600s watchdog."""
        assert evs(
            f"(let ((lo 0) (hi {BIG}))"
            f"  (loop while (< lo hi)"
            f"        do (let ((mid (ceiling (+ lo hi) 2)))"
            f"             (if (< mid {BIG}) (setq lo mid)"
            f"                 (if (= mid hi) (return lo) (setq hi mid)))))"
            f"  lo)") == str(BIG - 1)


class TestTwoValues:
    """CLHS 12.2: quotient and remainder, with number = quotient*divisor + rem."""

    @pytest.mark.parametrize('op', [
        'floor', 'ceiling', 'truncate', 'round',
        'ffloor', 'fceiling', 'ftruncate', 'fround',
    ])
    def test_returns_exactly_two_values(self, op):
        assert evs(f"(length (multiple-value-list ({op} 7 2)))") == '2'

    @pytest.mark.parametrize('op', ['floor', 'ceiling', 'truncate', 'round'])
    def test_the_identity_holds(self, op):
        assert evs(f"(multiple-value-bind (q r) ({op} -7 2) (+ (* q 2) r))") == '-7'

    @pytest.mark.parametrize('op', ['ffloor', 'fceiling', 'ftruncate', 'fround'])
    def test_the_f_variants_return_a_float_quotient(self, op):
        assert evs(f"(floatp (nth-value 0 ({op} 7 2)))") == 'T'

    def test_round_goes_half_to_even(self):
        assert evs("(nth-value 0 (round 5 2))") == '2'
        assert evs("(nth-value 0 (round 7 2))") == '4'


class TestRemainderSigns:
    """REM follows TRUNCATE (sign of the dividend), MOD follows FLOOR."""

    @pytest.mark.parametrize('expr,want', [
        ("(rem -7 2)", '-1'), ("(rem 7 -2)", '1'),
        ("(rem -7 -2)", '-1'), ("(rem 7 2)", '1'),
    ])
    def test_rem(self, expr, want):
        assert evs(expr) == want

    @pytest.mark.parametrize('expr,want', [
        ("(mod -7 2)", '1'), ("(mod 7 -2)", '-1'),
        ("(mod -7 -2)", '-1'), ("(mod 7 2)", '1'),
    ])
    def test_mod(self, expr, want):
        assert evs(expr) == want

    def test_rem_and_mod_agree_with_the_rounding_operators(self):
        """They are those operators' remainders, not a third implementation."""
        assert evs("(eql (rem -7 2) (nth-value 1 (truncate -7 2)))") == 'T'
        assert evs("(eql (mod -7 2) (nth-value 1 (floor -7 2)))") == 'T'
