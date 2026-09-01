"""CLHS 2.3.1 -- **the one place a token is decided to be a number.**

The reader's step 10 (`lispreader.read_10`) and the `#B`/`#O`/`#X`/`#nR`
dispatch readers (`readtable._read_radix_number`) both have to answer the same
question -- *does this run of constituent characters denote an integer, a ratio
or a float, and in what radix* -- and they had two different partial answers.
`read_10` matched three hardcoded regexes (`\\d+`, and a float pattern) and
`_read_radix_number` scanned digits itself; between them they agreed on nothing
except plain decimal integers, which is why:

* **`(read-from-string "1/2")` answered the *symbol* `1/2`.** Neither had a
  ratio syntax at all, so every ratio anyone wrote or printed read back as a
  symbol -- `print.ratios.random` is one test and it failed on the very first
  ratio it generated.
* **`*READ-BASE*` was not consulted anywhere.** `\\d+` is base ten by
  construction, so `(let ((*read-base* 16)) (read-from-string "FF"))` answered
  the symbol `FF`. The variable was not merely unread, it was **unbound** and
  registered as a `cl_function` under a variable's name (plan.md C7).
* **`123.` answered the float `123.0`.** A trailing decimal point makes a token
  a *decimal integer* (CLHS 2.3.1); reading it as a float is a wrong value, not
  a missing feature.
* **`|123|` answered the integer 123.** Step 8 tracks which characters were
  escaped and then threw that away when it called step 10, and a token
  containing an escape is never a number (CLHS 2.3.1.1).
* **`#x951115BA/AC02A5F7` answered just the numerator**, because
  `_read_radix_number` stopped at the first character that was not a digit of
  its radix and left `/AC02A5F7` on the stream.

Two properties this module exists to hold:

**The radix applies to integers and ratios, never to floats.** CLHS 2.3.1
writes the float grammar in terms of *decimal* digits, so `1.5` is one and a
half in every base. It follows that integer and ratio syntax must be tried
*first*: in base 16 `1E5` is the integer 485, and only in a base where `E` is
not a digit is it the float 100000.0.

**"Not a number" is a return of None, never a guess.** The caller then reads
the token as a symbol, which is what the standard says happens to a token that
is not a number and not a potential number. A token that *is* numeric syntax
but cannot be given a value -- `1/0`, an exponent that overflows -- is a
reader error, because silently answering a symbol there would make `1/0`
evaluate to an unbound variable rather than signal.
"""

import re as _re
from fractions import Fraction

#: The digit characters, in value order, for any radix 2-36. Slicing this is
#: the one place a "digit of base N" set comes from.
DIGIT_CHARS = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'

#: CLHS 2.3.1's exponent markers. `D`/`F`/`L`/`S` name double/single/long/short
#: float; `E` means "whatever `*READ-DEFAULT-FLOAT-FORMAT*` says". All four
#: letters are also digits in a large enough radix, which is exactly why
#: integer syntax is tried before float syntax.
EXPONENT_MARKERS = 'DEFLS'

#: `[sign] decimal-digit+ decimal-point` -- a *decimal* integer whatever the
#: input radix is. Kept separate from the radix integer pattern below because
#: the trailing point is what forces base ten.
_DECIMAL_INTEGER = _re.compile(r'^[+-]?([0-9]+)\.$')

#: The two float productions of CLHS 2.3.1, as one alternation:
#: `[sign] decimal-digit* . decimal-digit+ [exponent]`
#: `[sign] decimal-digit+ [. decimal-digit*] exponent`
#: The exponent is optional in the first and required in the second, which is
#: what keeps a bare run of digits out of this pattern -- that is an integer.
_FLOAT = _re.compile(
    r'^[+-]?(?:'
    r'[0-9]*\.[0-9]+(?:[DEFLSdefls][+-]?[0-9]+)?'
    r'|'
    r'[0-9]+(?:\.[0-9]*)?[DEFLSdefls][+-]?[0-9]+'
    r')$')


class NumericTokenError(Exception):
    """`token` is numeric syntax that denotes no number (`1/0`, `1e99999`).

    Distinct from returning None: None means "read this as a symbol", and this
    means "signal a READER-ERROR". `lispreader` and `readtable` translate it.
    """


def check_radix(radix, what='radix'):
    """`radix` as an int in 2-36, or a `NumericTokenError`.

    CLHS constrains `*READ-BASE*` and `#nR`'s radix identically, and both used
    to be checked in neither place -- `#1R` and `(setq *read-base* 1)` were
    accepted and then produced whatever Python's `int(s, 1)` does (it raises).
    """
    if isinstance(radix, bool) or not isinstance(radix, int):
        raise NumericTokenError(f"{what} must be an integer between 2 and 36, not {radix!r}")
    if not 2 <= radix <= 36:
        raise NumericTokenError(f"{what} must be between 2 and 36, not {radix}")
    return radix


def _radix_integer_pattern(radix):
    """`^[+-]?<digit-of-radix>+$` for `radix`."""
    return _re.compile(r'^[+-]?[' + _re.escape(DIGIT_CHARS[:radix]) + r']+$',
                       _re.IGNORECASE)


def _radix_ratio_pattern(radix):
    """`^[+-]?<digits>/<digits>$` for `radix`.

    The denominator takes no sign of its own: CLHS 2.3.1's ratio production is
    `[sign] {digit}+ slash {digit}+`, so `1/-2` is not a ratio (it is a
    symbol), and accepting it here would silently create a ratio the printer
    then prints in a form that does not read back.
    """
    digits = r'[' + _re.escape(DIGIT_CHARS[:radix]) + r']+'
    return _re.compile(r'^([+-]?)(' + digits + r')/(' + digits + r')$',
                       _re.IGNORECASE)


def _float_value(token):
    """The float `token` denotes, with CLHS's exponent markers normalized.

    `D`/`F`/`L`/`S` all mean "exponent" to Python's `float()` only after being
    rewritten to `E`; every fclpy float is a Python float, so the marker
    selects no representation yet (`*READ-DEFAULT-FLOAT-FORMAT*` is recorded
    but not honoured -- see plan.md).
    """
    normalized = token.upper()
    for marker in EXPONENT_MARKERS:
        normalized = normalized.replace(marker, 'E')
    try:
        return float(normalized)
    except (ValueError, OverflowError) as exc:
        raise NumericTokenError(f"{token!r} does not denote a float: {exc}")


def parse_numeric_token(token, radix=10, escaped=False):
    """The number `token` denotes in input radix `radix`, or None.

    None means `token` is not numeric syntax and the caller should read it as a
    symbol. `escaped` is true when any character of the token was preceded by a
    single escape or came from inside a multiple escape; such a token is never
    a number (CLHS 2.3.1.1), which is what makes `|123|` a symbol.
    """
    if escaped or not token:
        return None
    check_radix(radix, '*READ-BASE*')

    # A trailing decimal point makes the token a decimal integer regardless of
    # the input radix -- CLHS 2.3.1's first `integer` production. Tried before
    # the radix integer because `.` is a digit in no radix, so the two patterns
    # cannot both match, but the ordering documents which rule applies.
    decimal = _DECIMAL_INTEGER.match(token)
    if decimal:
        value = int(decimal.group(1), 10)
        return -value if token[0] == '-' else value

    # Integer and ratio in the input radix, before float: in base 16 `1E5` is
    # an integer and only the smaller bases leave it to the float pattern.
    if _radix_integer_pattern(radix).match(token):
        sign = -1 if token[0] == '-' else 1
        digits = token.lstrip('+-')
        return sign * int(digits, radix)

    ratio = _radix_ratio_pattern(radix).match(token)
    if ratio:
        sign, numerator, denominator = ratio.groups()
        denominator = int(denominator, radix)
        if denominator == 0:
            raise NumericTokenError(f"{token!r} has a zero denominator")
        value = Fraction(int(numerator, radix), denominator)
        if sign == '-':
            value = -value
        # A ratio whose denominator divides out is an integer, and must *be*
        # one: `(read-from-string "4/2")` is 2, and `(typep (read-from-string
        # "4/2") 'ratio)` is false. Fraction already normalizes, so this only
        # has to unwrap the whole case.
        if value.denominator == 1:
            return value.numerator
        return value

    if _FLOAT.match(token):
        return _float_value(token)

    return None
