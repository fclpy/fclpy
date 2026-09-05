"""System information, time operations, and environment access.

**This module is the one home of the universal-time model (CLHS 25.1.4).**
There used to be two, and neither implemented the chapter: this module's
`DECODE-UNIVERSAL-TIME` took no `time-zone` argument at all, `core.py`'s took
one and ignored it, and which of the two ran was decided by import order --
this one won, so *every* test that passed a time zone signalled a Python
`TypeError` about positional arguments. `core.py`'s `ENCODE-UNIVERSAL-TIME`
went through `time.mktime`, which is expressed in the *local* zone and raises
outside the platform's `time_t` range, so `(encode-universal-time 0 0 0 1 1
1900 0)` -- the inverse of the chapter's own first example -- was an error
rather than 0.

Three properties the model holds, and the reason each is here:

- **The calendar is computed, never asked of the OS.** `_decode_calendar` and
  `_encode_calendar` are pure Gregorian arithmetic over
  `datetime.date.toordinal`, so a universal time in 1900 or in 5000 decodes as
  exactly as one in 2026. The OS is consulted for one thing only -- what the
  *local* zone and daylight-saving rule are -- and only when the caller
  omitted `time-zone`.
- **A time zone is a rational number of hours west of GMT**, not an integer of
  hours and not a number of seconds. `decode-universal-time.4` and
  `encode-universal-time.3` build one as `(/ <seconds> 3600)`, require the
  same value back out under EQL, and require the round trip to be exact -- so
  the offset is carried as a `Fraction` and the returned zone is the caller's
  own object.
- **`_local_offset_seconds` is the single definition of the local offset**, so
  DECODE and ENCODE cannot disagree about it. They are inverses by
  construction rather than by two matching sign conventions.
"""

import datetime
from fractions import Fraction

import fclpy.lisptype as lisptype
from fclpy.lispfunc import registry as _registry
from fclpy.system.filesystem import fs
from fclpy.system.kernel import kernel
from fclpy.system.shell import shell


# =====================================================================
# The universal time model (CLHS 25.1.4)
# =====================================================================

#: A universal time counts seconds from 1900-01-01 00:00:00 GMT (CLHS
#: 25.1.4.1); a Unix timestamp counts from 1970-01-01 00:00:00 GMT. This is
#: the one place the two epochs are related.
UNIX_EPOCH_UNIVERSAL_TIME = 2208988800

#: `datetime.date.toordinal()` of the universal-time epoch day.
_EPOCH_ORDINAL = datetime.date(1900, 1, 1).toordinal()

#: The Gregorian calendar repeats exactly every 400 years -- same weekday for
#: the same month and day, same leap years -- so shifting a date by a whole
#: number of these cycles moves it into a range the platform's `mktime` can
#: represent *without changing the answer* to "is daylight saving in effect on
#: this date". That is what lets the DST rule be applied to a date in 1900 or
#: 4000 at all. The bounds below are one cycle apart, so one pass of each
#: while-loop always lands inside them.
_GREGORIAN_CYCLE_YEARS = 400
_DST_PROBE_MIN_YEAR = 1972      # first leap year the Unix epoch fully contains
_DST_PROBE_MAX_YEAR = _DST_PROBE_MIN_YEAR + _GREGORIAN_CYCLE_YEARS - 1


def _canonical_zone(value):
    """A `Fraction` whose denominator reduced to 1 *is* an integer (CLHS 12.1.1.2).

    The same normalization `math_arithmetic._canonicalize_rational` performs,
    applied to a time zone: a whole-hour offset must answer EQL to an integer,
    or `(eql tz zone)` fails for the common case.
    """
    if isinstance(value, Fraction) and value.denominator == 1:
        return value.numerator
    return value


def _time_zone_offset_seconds(time_zone, operator):
    """A time-zone designator as an exact number of seconds west of GMT.

    CLHS glossary, *time zone*: a rational multiple of 1/3600 between -24 and
    24 inclusive. Anything else is a TYPE-ERROR rather than a silently
    truncated offset -- a wrong offset is a wrong *time*, which no caller can
    detect.
    """
    if isinstance(time_zone, bool) or not isinstance(time_zone, (int, Fraction)):
        raise lisptype.LispTypeError(
            f"{operator}: time zone must be a rational: {time_zone}",
            expected_type="RATIONAL", actual_value=time_zone)
    offset = Fraction(time_zone) * 3600
    if offset.denominator != 1 or not (-24 * 3600 <= offset <= 24 * 3600):
        raise lisptype.LispTypeError(
            f"{operator}: time zone must be a multiple of 1/3600 "
            f"between -24 and 24: {time_zone}",
            expected_type="(RATIONAL -24 24)", actual_value=time_zone)
    return offset.numerator


def _decode_calendar(local_seconds):
    """Seconds since the epoch *in some local zone* -> the calendar fields.

    Returns `(second, minute, hour, date, month, year, day)` where `day` is
    CLHS's day of the week: 0 is Monday, which is exactly what
    `datetime.date.weekday()` answers -- and 1900-01-01 really was a Monday,
    so `(decode-universal-time 0 0)` yields day 0.

    Python's `divmod` floors, so a negative `local_seconds` (a time east of
    GMT in the first hours of 1900) decodes into 1899 rather than wrapping.
    """
    days, rest = divmod(local_seconds, 86400)
    hour, rest = divmod(rest, 3600)
    minute, second = divmod(rest, 60)
    try:
        day = datetime.date.fromordinal(_EPOCH_ORDINAL + days)
    except (ValueError, OverflowError):
        raise lisptype.LispTypeError(
            f"universal time is outside the representable calendar: "
            f"{local_seconds}",
            expected_type="UNSIGNED-BYTE", actual_value=local_seconds)
    return (second, minute, hour, day.day, day.month, day.year, day.weekday())


def _encode_calendar(second, minute, hour, date, month, year):
    """The inverse of `_decode_calendar`: calendar fields -> local seconds."""
    try:
        ordinal = datetime.date(year, month, date).toordinal()
    except (ValueError, OverflowError, TypeError):
        raise lisptype.LispTypeError(
            f"ENCODE-UNIVERSAL-TIME: not a valid date: "
            f"{year}-{month}-{date}",
            expected_type="(INTEGER 1 9999)", actual_value=year)
    return ((ordinal - _EPOCH_ORDINAL) * 86400
            + hour * 3600 + minute * 60 + second)


def _daylight_saving_in_effect(second, minute, hour, date, month, year):
    """Is daylight saving in effect at this local date and time?

    Determined from the *calendar date*, not from a timestamp, because that is
    how a daylight-saving rule is written ("the second Sunday in March") and
    because a timestamp for 1900 or 4000 is not representable. The year is
    shifted by whole Gregorian cycles into the platform's range first, which
    preserves the weekday-and-leap-year structure the rule reads.

    A machine with no daylight-saving rule answers False without consulting
    the OS at all, so `_local_offset_seconds` collapses to `time.timezone`.
    """
    if not kernel.daylight():
        return False
    probe_year = year
    while probe_year < _DST_PROBE_MIN_YEAR:
        probe_year += _GREGORIAN_CYCLE_YEARS
    while probe_year > _DST_PROBE_MAX_YEAR:
        probe_year -= _GREGORIAN_CYCLE_YEARS
    try:
        stamp = kernel.mktime((probe_year, month, date, hour, minute, second,
                               0, 1, -1))
        return kernel.localtime(stamp).tm_isdst > 0
    except (OSError, OverflowError, ValueError):
        # The platform cannot say. Reporting "no daylight saving" is the
        # honest answer for a zone whose rule is unavailable, and it keeps
        # DECODE and ENCODE mutual inverses, which is what the round-trip
        # tests actually require.
        return False


def _local_offset_seconds(daylight_p):
    """The local zone's offset in seconds west of GMT.

    One definition, read by both DECODE and ENCODE, so they are inverses by
    construction. `time.timezone`/`time.altzone` are already seconds west of
    GMT -- the same sign convention CLHS uses for a time zone.
    """
    return kernel.altzone() if daylight_p else kernel.timezone()


def _local_zone_hours(daylight_p):
    """The offset `_local_offset_seconds` reports, as CLHS's hours west of GMT.

    CLHS DECODE-UNIVERSAL-TIME returns the *standard* zone even when daylight
    saving is in effect -- the extra hour is reported by `daylight-p`, not
    folded into `zone` -- so this is always asked with `daylight_p` false for
    the returned value.
    """
    return _canonical_zone(Fraction(_local_offset_seconds(daylight_p), 3600))


def _decode_in_local_zone(universal_time):
    """Decode with the *current* time zone and daylight-saving rule.

    Two passes, because the daylight-saving rule is keyed on the local date
    and the local date depends on whether daylight saving applies: decode with
    the standard offset to learn the date, ask the rule, then decode again
    with the offset the rule selects. Reversing that order would need the
    answer to compute the question.
    """
    provisional = _decode_calendar(universal_time - _local_offset_seconds(False))
    daylight_p = _daylight_saving_in_effect(*provisional[:6])
    if not daylight_p:
        return provisional, False
    fields = _decode_calendar(universal_time - _local_offset_seconds(True))
    return fields, True


@_registry.cl_function('DECODE-UNIVERSAL-TIME')
def decode_universal_time(universal_time, time_zone=lisptype.OMITTED):
    """DECODE-UNIVERSAL-TIME (CLHS 25.1.4.2) -- nine values.

    `time-zone` is genuinely optional rather than defaulted, and the
    distinction is observable: supplied, no daylight-saving adjustment is
    performed and `daylight-p` is NIL; omitted, the current zone and rule
    apply. NIL is not a time zone, so `OMITTED` is the sentinel -- the same
    reason `lisptype.OMITTED` exists everywhere else in this codebase.
    """
    if not isinstance(universal_time, int) or isinstance(universal_time, bool):
        raise lisptype.LispTypeError(
            f"DECODE-UNIVERSAL-TIME: not a universal time: {universal_time}",
            expected_type="UNSIGNED-BYTE", actual_value=universal_time)

    if time_zone is lisptype.OMITTED:
        fields, daylight_p = _decode_in_local_zone(universal_time)
        zone = _local_zone_hours(False)
    else:
        offset = _time_zone_offset_seconds(time_zone, 'DECODE-UNIVERSAL-TIME')
        fields = _decode_calendar(universal_time - offset)
        daylight_p = False
        # The caller's own object, so `(eql tz zone)` holds for a ratio as
        # well as for an integer.
        zone = time_zone

    second, minute, hour, date, month, year, day = fields
    return lisptype.MultipleValues(second, minute, hour, date, month, year,
                                   day, lisptype.lisp_bool(daylight_p), zone)


@_registry.cl_function('ENCODE-UNIVERSAL-TIME')
def encode_universal_time(second, minute, hour, date, month, year,
                          time_zone=lisptype.OMITTED):
    """ENCODE-UNIVERSAL-TIME (CLHS 25.1.4.3) -- the inverse of DECODE.

    With `time-zone` omitted the current zone applies, *including* its
    daylight-saving rule, which is what makes `(encode-universal-time
    (decode-universal-time t))` recover `t`.
    """
    for name, value in (('SECOND', second), ('MINUTE', minute),
                        ('HOUR', hour), ('DATE', date), ('MONTH', month),
                        ('YEAR', year)):
        if not isinstance(value, int) or isinstance(value, bool):
            raise lisptype.LispTypeError(
                f"ENCODE-UNIVERSAL-TIME: {name} must be an integer: {value}",
                expected_type="INTEGER", actual_value=value)

    # CLHS 25.1.4.3: a year between 0 and 99 names a year in the hundred-year
    # span beginning fifty years before the current one.
    if 0 <= year <= 99:
        current_year = _decode_in_local_zone(get_universal_time())[0][5]
        century = (current_year - 50) // 100 * 100
        year = century + year
        if year < current_year - 50:
            year += 100

    local_seconds = _encode_calendar(second, minute, hour, date, month, year)
    if time_zone is lisptype.OMITTED:
        daylight_p = _daylight_saving_in_effect(second, minute, hour,
                                                date, month, year)
        offset = _local_offset_seconds(daylight_p)
    else:
        offset = _time_zone_offset_seconds(time_zone, 'ENCODE-UNIVERSAL-TIME')
    return local_seconds + offset


@_registry.cl_function('GET-UNIVERSAL-TIME')
def get_universal_time():
    """GET-UNIVERSAL-TIME (CLHS 25.1.4.4) -- the current time, GMT-based."""
    return int(kernel.time()) + UNIX_EPOCH_UNIVERSAL_TIME


@_registry.cl_function('GET-DECODED-TIME')
def get_decoded_time():
    """GET-DECODED-TIME (CLHS 25.1.4.4) -- the current time, decoded.

    Defined as `(decode-universal-time (get-universal-time))`, and written
    that way: it returned *seven* values from a private conversion, so
    `get-universal-time.2` -- which asserts both are nine values long and
    agree field by field -- could not pass however DECODE behaved.
    """
    return decode_universal_time(get_universal_time())


#: `INTERNAL-TIME-UNITS-PER-SECOND` (CLHS 25.1.4.1). Milliseconds. A constant
#: *variable*, established by `lispenv`; the resolution of the internal-time
#: clocks below is expressed through it rather than by two matching literals.
INTERNAL_TIME_UNITS_PER_SECOND = 1000


@_registry.cl_function('GET-INTERNAL-REAL-TIME')
def get_internal_real_time():
    """GET-INTERNAL-REAL-TIME (CLHS 25.1.4.5).

    CLHS requires only that this count internal time units "relative to an
    arbitrary time base", and the tests require it to be *monotonic* over a
    tight loop. `time.monotonic` guarantees exactly that; `time.time` does
    not -- it can step backwards when the system clock is adjusted, and on
    Windows its resolution is coarse enough that an adjustment lands inside a
    single test run.
    """
    return int(kernel.monotonic() * INTERNAL_TIME_UNITS_PER_SECOND)


@_registry.cl_function('GET-INTERNAL-RUN-TIME')
def get_internal_run_time():
    """GET-INTERNAL-RUN-TIME (CLHS 25.1.4.5) -- computation time, monotonic."""
    return int(kernel.process_time() * INTERNAL_TIME_UNITS_PER_SECOND)


@_registry.cl_function('SLEEP')
def sleep(seconds):
    """SLEEP (CLHS 25.1.4.6) -- a non-negative *real*, ratios included.

    `time.sleep` takes a Python float, so a Lisp ratio has to be converted
    rather than passed through: `(sleep 1/100)` raised a Python `TypeError`
    about interpreting a `Fraction` as an integer, and
    `(sleep (/ 1000000000000000000000000000000))` still has to be a legal way
    to say "essentially no time at all". `float()` of a ratio that small
    underflows to 0.0, which is the correct duration to wait.
    """
    from .math_arithmetic import _ensure_real
    _ensure_real(seconds, 'SLEEP')
    if seconds < 0:
        raise lisptype.LispTypeError(
            f"SLEEP: seconds must be non-negative: {seconds}",
            expected_type="(REAL 0)", actual_value=seconds)
    kernel.sleep(float(seconds))
    return lisptype.NIL


@_registry.cl_special('TIME')
def time_special(form):
    """TIME (CLHS 25.1.3) is a *macro*, so its subform must not be evaluated
    before it is entered. Registered here so the dispatcher knows the symbol
    names a special operator; the semantics are
    `evaluation_loops_conditionals.eval_time`.
    """
    raise lisptype.LispNotImplementedError('TIME', 'special form handled by evaluator')


# --- System information ---
@_registry.cl_function('LISP-IMPLEMENTATION-TYPE')
def lisp_implementation_type():
    """Get Lisp implementation type."""
    return "FCLPY"


@_registry.cl_function('LISP-IMPLEMENTATION-VERSION')
def lisp_implementation_version():
    """Get Lisp implementation version."""
    return "0.1.0"


@_registry.cl_function('MACHINE-INSTANCE')
def machine_instance():
    """Get machine instance (hostname)."""
    return kernel.machine_instance()


@_registry.cl_function('MACHINE-TYPE')
def machine_type():
    """Get machine type (CPU architecture)."""
    return kernel.machine_type()


@_registry.cl_function('MACHINE-VERSION')
def machine_version():
    """Get machine version (platform string)."""
    return kernel.machine_version()


@_registry.cl_function('SOFTWARE-TYPE')
def software_type():
    """Get software type (operating system)."""
    return kernel.software_type()


@_registry.cl_function('SOFTWARE-VERSION')
def software_version():
    """Get software version (OS release)."""
    return kernel.software_version()


@_registry.cl_function('SHORT-SITE-NAME')
def short_site_name():
    """Get short site name."""
    return "Unknown"


@_registry.cl_function('LONG-SITE-NAME')
def long_site_name():
    """Get long site name."""
    return "Unknown Site"


# --- Environment and file access ---
@_registry.cl_function('USER-HOMEDIR-PATHNAME')
def user_homedir_pathname(host=None):
    """USER-HOMEDIR-PATHNAME (CLHS 19.1.2): a *pathname* for the user's home
    directory -- a directory pathname, so PATHNAME-NAME/TYPE/VERSION all
    answer NIL (user-homedir-pathname.3-.5). This used to return the raw
    Python home string, which PATHNAMEP rejected (user-homedir-pathname.2).

    The optional `host` (CLHS: "a host designator... the default is the
    value of *null-pathname*'s host-ish default"): only the local/unspecific
    host is supported here, so a truthy host argument has no home directory
    to offer and NIL is the conforming answer (user-homedir-pathname.7
    accepts either NIL or a pathname); a wrong argument count is the
    caller's PROGRAM-ERROR (user-homedir-pathname.error.1), as for any
    function."""
    if lisptype.is_truthy(host):
        return lisptype.NIL
    home = shell.home()
    if not home.endswith(('/', '\\')):
        home += fs.get_path_sep()
    from .pathnames import pathname as _coerce_pathname
    return _coerce_pathname(home)


def get_env(name):
    """Get environment variable."""
    return shell.get_env(name)


def exit(code=0):
    """Exit the Lisp system."""
    kernel.exit(code)


def quit(code=0):
    """Quit the Lisp system."""
    exit(code)


# --- Random number generation ---

class RandomState:
    """A random state object for Common Lisp random number generation.
    
    Wraps Python's random.Random to provide reproducible sequences.
    """
    
    def __init__(self, seed=None):
        """Create a new random state.
        
        Args:
            seed: Optional seed value. If None, uses system entropy.
                  If True, creates a new random seed.
                  If another RandomState, copies its state.
        """
        import random as rnd
        self._random = rnd.Random()
        if seed is None:
            # Use system entropy - None already does this in Python's Random
            pass
        elif seed is True or seed is lisptype.T:
            # Create a new random seed from the kernel's entropy source,
            # combined with a high-resolution clock reading
            entropy = kernel.entropy(16) + str(kernel.perf_counter()).encode()
            self._random.seed(entropy)
        elif isinstance(seed, RandomState):
            # Copy state from another RandomState
            self._random.setstate(seed._random.getstate())
        elif isinstance(seed, (int, float)):
            self._random.seed(seed)
        elif isinstance(seed, (tuple, list)):
            # Try to restore from state tuple or list
            # Convert lists to tuples recursively since Python's setstate expects nested tuples
            try:
                def to_tuple(obj):
                    """Recursively convert lists to tuples."""
                    if isinstance(obj, list):
                        return tuple(to_tuple(item) for item in obj)
                    return obj

                seed_tuple = to_tuple(seed)
                self._random.setstate(seed_tuple)
            except (TypeError, ValueError):
                pass
    
    def getstate(self):
        """Get the internal state for later restoration."""
        return self._random.getstate()
    
    def setstate(self, state):
        """Restore state from a previously saved state."""
        self._random.setstate(state)
    
    def randrange(self, limit):
        """Return random integer in [0, limit)."""
        return self._random.randrange(limit)
    
    def random(self):
        """Return random float in [0.0, 1.0)."""
        return self._random.random()
    
    def __repr__(self):
        return "#<RANDOM-STATE>"
    
    def __str__(self):
        return "#<RANDOM-STATE>"


def _random_state_symbol():
    return lisptype.COMMON_LISP_PACKAGE.intern_symbol('*RANDOM-STATE*')


def current_random_state():
    """Read the live value of `*RANDOM-STATE*` (CLHS 12.1.4.3, 25.1.1).

    Mirrors `printer.resolve_control`'s resolution order -- lexical/dynamic
    binding in the current environment chain, then the symbol's value cell
    -- for the same reason that fix was needed: a private Python module
    global is invisible to a Lisp `(let ((*random-state* ...)) ...)` or
    `(setq *random-state* ...)`, which is exactly what `RANDOM` and
    `MAKE-RANDOM-STATE` need to see. `*RANDOM-STATE*` is bound to a real
    `RandomState` at bootstrap (`lispenv.py`), so this should always find
    one; a missing binding is a bootstrap defect, not routine unbound-ness,
    hence the `PROGRAM-ERROR` rather than a silent fallback that would mask it.
    """
    import fclpy.state as state

    symbol = _random_state_symbol()
    env = getattr(state, 'current_environment', None)
    if env is not None and env.has_variable(symbol):
        value = env.find_variable(symbol)
        if isinstance(value, RandomState):
            return value
    value = getattr(symbol, 'value', None)
    if isinstance(value, RandomState):
        return value
    raise lisptype.LispProgramError(
        "*RANDOM-STATE* is not bound to a RANDOM-STATE object")


@_registry.cl_function('RANDOM')
def random(limit, state=None):
    """Generate random number up to limit.

    Args:
        limit: Upper bound (exclusive). Must be positive real number.
        state: Optional random state to use. If None, uses *RANDOM-STATE*.

    Returns:
        Random integer in [0, limit) if limit is integer.
        Random float in [0.0, limit) if limit is float.
    """
    from fractions import Fraction
    rs = state if isinstance(state, RandomState) else current_random_state()

    # Type check - must be a positive real number
    if not isinstance(limit, (int, float, Fraction)):
        raise lisptype.LispTypeError(
            f"RANDOM: Argument is not a REAL: {limit}",
            expected_type="(REAL (0 *))", actual_value=limit)

    if isinstance(limit, complex):
        raise lisptype.LispTypeError(
            f"RANDOM: Argument is not a REAL: {limit}",
            expected_type="(REAL (0 *))", actual_value=limit)

    if limit <= 0:
        raise lisptype.LispTypeError(
            f"RANDOM: Argument must be positive: {limit}",
            expected_type="(REAL (0 *))", actual_value=limit)

    if isinstance(limit, int):
        return rs.randrange(limit)
    else:  # float or Fraction
        return rs.random() * limit


@_registry.cl_function('MAKE-RANDOM-STATE')
def make_random_state(state=lisptype.OMITTED):
    """Make random state object (CLHS 12.1.4 MAKE-RANDOM-STATE).

    Args:
        state: Controls how to initialize:
            - NIL or omitted: Copy current *RANDOM-STATE*
            - T: Create fresh state from entropy
            - RandomState: Copy that state

    Returns:
        A new RandomState object.

    The argument is restricted to NIL, T, or a random-state (CLHS 12.1.4
    says a non-nil/non-t/non-random-state designator is a TYPE-ERROR, not
    a seed). The previous implementation handed every other value to
    Python's `RandomState` as a seed, so `(make-random-state 0)` and
    `(make-random-state 1.5)` silently answered a state object instead of
    signalling, and MAKE-RANDOM-STATE.ERROR.4 (`check-type-error`) saw
    them as successes.
    """
    if state is lisptype.OMITTED or state is None or state is lisptype.NIL:
        return RandomState(current_random_state())
    if state is True or state is lisptype.T:
        return RandomState(True)
    if isinstance(state, RandomState):
        return RandomState(state)
    raise lisptype.LispTypeError(
        f"MAKE-RANDOM-STATE: invalid state argument: {state!r}",
        expected_type="(OR (MEMBER NIL T) RANDOM-STATE)",
        actual_value=state)


@_registry.cl_function('RANDOM-STATE-P')
def random_state_p(object):
    """Test if object is random state."""
    return lisptype.lisp_bool(isinstance(object, RandomState))


@_registry.cl_function('%MAKE-RANDOM-STATE-FROM-DATA')
def make_random_state_from_data(data):
    """Rebuild a random state from its readable printed representation.

    CLHS 22.1.3.10: the syntax is implementation-dependent, but reading the
    printed form must construct a copy "as if the copy had been made by
    make-random-state". The form this implementation prints is

        #.(FCLPY-INTERNAL::%MAKE-RANDOM-STATE-FROM-DATA '(version state gauss))

    and it deliberately does *not* go through MAKE-RANDOM-STATE: that
    function's argument is a random-state designator (NIL, T, or a
    random-state) and MAKE-RANDOM-STATE.ERROR.4 requires every other object
    -- a data vector included -- to signal a TYPE-ERROR, so a seed argument
    there would trade one green test for another.

    `data` is the object the reader produced from the quoted vector: a
    version integer, the generator's internal state as a sequence of
    integers, and the saved gaussian tail (NIL when there is none) -- the
    exact tuple Python's `random.Random.getstate` returns and `setstate`
    consumes.
    """
    if isinstance(data, lisptype.lispCons) or _is_nil(data):
        from .sequence_protocol import list_elements
        items = [] if _is_nil(data) else list_elements(data, dotted='error')
    elif isinstance(data, (list, tuple)):
        items = list(data)
    else:
        from .arrays import array_elements
        items = list(array_elements(data))

    if len(items) < 2:
        raise lisptype.LispTypeError(
            "%MAKE-RANDOM-STATE-FROM-DATA: malformed state data: {data!r}",
            expected_type="(CONS (MEMBER 3) (CONS SEQUENCE T))",
            actual_value=data)
    version, internal = items[0], items[1]
    gauss = items[2] if len(items) > 2 else None

    if isinstance(internal, (list, tuple)):
        internal_values = list(internal)
    elif isinstance(internal, lisptype.lispCons) or _is_nil(internal):
        from .sequence_protocol import list_elements
        internal_values = [] if _is_nil(internal) else list_elements(
            internal, dotted='error')
    else:
        from .arrays import array_elements
        internal_values = list(array_elements(internal))

    try:
        state_tuple = (
            int(version),
            tuple(int(v) for v in internal_values),
            None if _is_nil(gauss) else gauss,
        )
        rs = RandomState()
        rs.setstate(state_tuple)
    except (TypeError, ValueError) as exc:
        raise lisptype.LispTypeError(
            f"%MAKE-RANDOM-STATE-FROM-DATA: not a valid random state: {exc}",
            expected_type="(CONS INTEGER (CONS SEQUENCE T))",
            actual_value=data)
    return rs


def _is_nil(value):
    """True for any of NIL's Python spellings (None, the singleton, symbol)."""
    if value is None or value is lisptype.NIL:
        return True
    return (isinstance(value, lisptype.LispSymbol) and value.name == 'NIL')


@_registry.cl_function('RATIONAL-SAFELY')
def rational_safely(x):
    """Rational a floating point number, limiting the exponent.
    
    This is a utility function for testing that converts a float to a rational
    while limiting the exponent to avoid very large rationals that some
    implementations (like CLISP) might struggle with.
    
    CLHS: This is not a standard function, but an ansi-test aux function.
    """
    if not isinstance(x, float):
        raise lisptype.LispTypeError(
            f"RATIONAL-SAFELY: argument is not a FLOAT: {x!r}",
            expected_type="FLOAT", actual_value=x)
    
    # Use integer-decode-float to get significand, exponent, sign
    from .math_advanced import integer_decode_float
    significand, exponent, sign = integer_decode_float(x)
    
    # Limit exponent to [-1000, 1000]
    limit = 1000
    radix = 2  # Python floats are binary
    
    if exponent < -limit:
        result = significand * (radix ** (-limit)) * sign
    elif exponent > limit:
        result = significand * (radix ** limit) * sign
    else:
        # Use standard rational conversion
        from fractions import Fraction
        result = float(Fraction(x).limit_denominator())
    
    # Return as a rational (Fraction) if possible, else float
    from fractions import Fraction
    try:
        return Fraction(x).limit_denominator()
    except (OverflowError, ValueError):
        return float(x)


__all__ = [
    'get_universal_time',
    'decode_universal_time',
    'encode_universal_time',
    'get_decoded_time',
    'get_internal_real_time',
    'get_internal_run_time',
    'INTERNAL_TIME_UNITS_PER_SECOND',
    'UNIX_EPOCH_UNIVERSAL_TIME',
    'sleep',
    'lisp_implementation_type',
    'lisp_implementation_version',
    'machine_instance',
    'machine_type',
    'machine_version',
    'software_type',
    'software_version',
    'short_site_name',
    'long_site_name',
    'user_homedir_pathname',
    'get_env',
    'exit',
    'quit',
    'random',
    'make_random_state',
    'random_state_p',
    'RandomState',
    'current_random_state',
]
