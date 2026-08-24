"""Regression tests for the LOOP form's value and the CLHS 25 environment chapter.

Grouped in one file because they were found together: the LOOP defect surfaced
from `environment/get-internal-time.lsp`, whose monotonicity tests wrap a
`loop ... do ...` in `(compile nil '(lambda () ...))` and require NIL.
"""

import pytest

from fclpy import lispenv
from fclpy.lispfunc.evaluation_core import ConditionException
from fclpy.lispfunc import eval_string
import fclpy.lisptype as lisptype


@pytest.fixture(scope='module', autouse=True)
def environment():
    lispenv.setup_standard_environment()


def ev(source):
    return eval_string(source)


def signals(source, condition_type):
    """Assert `source` signals `condition_type`.

    A condition raised inside `eval` reaches Python as a `ConditionException`
    carrying it, which is the whole point of that wrapper -- a Lisp handler
    matches the condition, not the Python exception -- so a test that expects
    the bare Lisp error class would pass only for raise sites that bypass
    signaling.
    """
    with pytest.raises((condition_type, ConditionException)) as raised:
        ev(source)
    payload = raised.value
    if isinstance(payload, ConditionException):
        inner = getattr(payload, 'condition', None)
        assert isinstance(inner, condition_type) or type(inner).__name__.endswith(
            condition_type.__name__.replace('Lisp', '')), (
                f"{source} signalled {inner!r}, not a {condition_type.__name__}")


# ---------------------------------------------------------------------------
# CLHS 6.1.1.4 -- what the LOOP form's value is
# ---------------------------------------------------------------------------

@pytest.mark.parametrize('source', [
    # `do` clauses are evaluated for effect; their values are discarded. Each of
    # these used to answer the last form's value: `(1)`, `(9)`, `3`, `5`.
    '(loop for x = 1 repeat 3 do (list x))',
    '(loop repeat 3 do (list 9))',
    '(loop for x in (list 1 2 3) do (list x))',
    '(loop for x in (list 1 2 3) do (list x) do (list x))',
    # A `finally` clause is likewise for effect; only `finally (return ...)`
    # supplies a value.
    '(loop for x in (list 1 2 3) do (list x) finally (list 7))',
    '(loop for x from 1 to 3 finally (list 7))',
])
def test_loop_with_only_side_effecting_clauses_is_nil(source):
    assert ev(source) is lisptype.NIL


@pytest.mark.parametrize('source,expected', [
    # ... while every clause that *names* a value still supplies it.
    ('(loop for x in (list 1 2 3) finally (return 42))', 42),
    ('(loop for x in (list 1 2 3) when (= x 2) return (* x 10))', 20),
    ('(loop for x in (list 1 2 3) do (when (= x 2) (return 99)))', 99),
    ('(loop for x in (list 1 2 3) sum x)', 6),
    ('(loop for x in (list 1 2 3) count (oddp x))', 2),
    ('(loop for i from 1 to 3 maximize i)', 3),
    ('(loop for i from 1 to 3 minimize i)', 1),
    ('(loop for x in (list 1 2 3) thereis (and (= x 2) 5))', 5),
    # A RETURN clause is `do (return ...)` (CLHS 6.1.5.3), so it wins over an
    # accumulation clause rather than falling through to it.
    ('(loop for x in (list 1 2) collect x return 3)', 3),
])
def test_loop_value_naming_clauses_still_supply_it(source, expected):
    assert ev(source) == expected


def test_loop_collect_returns_a_list():
    assert ev('(equal (loop for x in (list 1 2 3) collect x) (list 1 2 3))') is lisptype.T


def test_loop_always_and_never_are_booleans():
    assert ev('(loop for x in (list 1 2 3) always (numberp x))') is lisptype.T
    assert ev('(loop for x in (list 1 2 3) never (stringp x))') is lisptype.T
    assert ev('(loop for x in (list 1 2 3) always (stringp x))') is lisptype.NIL


def test_loop_finally_return_overrides_an_accumulation():
    assert ev('(loop for x in (list 1 2 3) collect x finally (return :done))') \
        == lisptype.intern_keyword('DONE')


# ---------------------------------------------------------------------------
# CLHS 25.1.4 -- the universal time model
# ---------------------------------------------------------------------------

# The reference values CLHS and ansi-test both give. Day 0 is Monday, and
# 1900-01-01 really was a Monday.
@pytest.mark.parametrize('source,expected', [
    ('(multiple-value-list (decode-universal-time 0 0))',
     [0, 0, 0, 1, 1, 1900, 0, lisptype.NIL, 0]),
    # A zone of -1 is one hour *east* of GMT, so local time is 01:00.
    ('(multiple-value-list (decode-universal-time 0 -1))',
     [0, 0, 1, 1, 1, 1900, 0, lisptype.NIL, -1]),
    ('(multiple-value-list (decode-universal-time (* 365 3600 24) 0))',
     [0, 0, 0, 1, 1, 1901, 1, lisptype.NIL, 0]),
    ('(multiple-value-list (decode-universal-time (* 4 365 3600 24) 0))',
     [0, 0, 0, 1, 1, 1904, 4, lisptype.NIL, 0]),
    # 1904 is a leap year, so 1905-01-01 is 5*365+1 days in, and was a Sunday.
    ('(multiple-value-list (decode-universal-time (+ (* 24 3600) (* 5 365 3600 24)) 0))',
     [0, 0, 0, 1, 1, 1905, 6, lisptype.NIL, 0]),
])
def test_decode_universal_time_reference_values(source, expected):
    from fclpy.lispfunc.sequence_protocol import seq_elements
    assert list(seq_elements(ev(source))) == expected


def test_encode_universal_time_is_the_inverse_of_decode():
    """Round trip with an explicit zone, which must be exact.

    `core.py`'s copy went through `time.mktime`, which is expressed in the
    *local* zone and raises outside the platform's `time_t` range, so this was
    an error rather than 0.
    """
    assert ev('(encode-universal-time 0 0 0 1 1 1900 0)') == 0
    assert ev("""(let ((count 0))
                   (dolist (u (list 0 1 86399 86400 1000000000 99999999999) count)
                     (dolist (tz (list 0 -1 5 -12 24 -24))
                       (let ((d (multiple-value-list (decode-universal-time u tz))))
                         (unless (= u (apply (function encode-universal-time)
                                             (append (subseq d 0 6) (list tz))))
                           (incf count))))))""") == 0


def test_time_zone_may_be_a_ratio_and_comes_back_eql():
    """CLHS's time zone is a rational multiple of 1/3600, not an integer.

    `decode-universal-time.4` builds one as `(/ <seconds> 3600)` and requires
    `(eql tz zone)` plus an exact round trip, which is why the offset is
    carried as an exact rational rather than as whole hours.
    """
    assert ev('(eql 7/2 (nth-value 8 (decode-universal-time 300000000 7/2)))') \
        is lisptype.T
    assert ev("""(let* ((tz 4201/3600)
                        (d (multiple-value-list (decode-universal-time 300000000 tz))))
                   (= 300000000 (apply (function encode-universal-time)
                                       (append (subseq d 0 6) (list tz)))))""") \
        is lisptype.T


def test_decode_universal_time_rejects_a_non_rational_zone():
    signals('(decode-universal-time 0 "noon")', lisptype.LispTypeError)
    signals('(decode-universal-time 0 (quote a))', lisptype.LispTypeError)


def test_get_decoded_time_agrees_with_decode_universal_time():
    """GET-DECODED-TIME *is* `(decode-universal-time (get-universal-time))`.

    It used to answer seven values from a private conversion, so
    `get-universal-time.2` -- which asserts both are nine values long and
    agree field by field -- could not pass however DECODE behaved.
    """
    assert ev('(length (multiple-value-list (get-decoded-time)))') == 9
    assert ev("""(let ((u (get-universal-time)))
                   (equal (subseq (multiple-value-list (decode-universal-time u)) 0 7)
                          (subseq (multiple-value-list (get-decoded-time)) 0 7)))""") \
        is lisptype.T


def test_sleep_accepts_any_non_negative_real():
    """A ratio is a REAL, and `time.sleep` cannot take one directly."""
    for source in ('(sleep 0)', '(sleep 0.0)', '(sleep 1/100)',
                   '(sleep (/ 1000000000000000000000000000000))'):
        assert ev(source) is lisptype.NIL


def test_sleep_rejects_a_negative_or_non_real_argument():
    for source in ('(sleep -1)', '(sleep (quote a))', '(sleep "1")'):
        signals(source, lisptype.LispTypeError)


def test_internal_real_time_is_monotonic():
    """CLHS only requires an arbitrary time base, so a monotonic clock is the
    right source. `time.time()` can step backwards."""
    assert ev("""(let ((prev (get-internal-real-time)))
                   (loop repeat 2000
                         do (let ((next (get-internal-real-time)))
                              (assert (>= next prev))
                              (setf prev next))))""") is lisptype.NIL


# ---------------------------------------------------------------------------
# CLHS 3.1.2.1 -- constant forms
# ---------------------------------------------------------------------------

@pytest.mark.parametrize('source', [
    # Self-evaluating: anything that is neither a symbol nor a cons. The
    # previous implementation enumerated int/float/str/bool/keyword/NIL and so
    # answered NIL for every one of these.
    '(constantp 1/2)', '(constantp #\\a)', '(constantp #c(1 2))',
    '(constantp "abc")', '(constantp (vector 1 2))',
    '(constantp (make-hash-table))', '(constantp 1.5)',
    # Constant variables.
    '(constantp t)', '(constantp nil)', '(constantp :foo)',
    '(constantp (quote pi))',
    '(constantp (quote internal-time-units-per-second))',
    '(constantp (quote most-positive-fixnum))',
    # A QUOTE form.
    "(constantp (quote (quote (((foo))))))",
])
def test_constantp_is_true_for_constant_forms(source):
    assert ev(source) is lisptype.T


@pytest.mark.parametrize('source', [
    '(constantp (gensym))',
    '(constantp (quote (incf x)))',
    '(constantp (quote a-name-nothing-defined))',
])
def test_constantp_is_false_otherwise(source):
    assert ev(source) is lisptype.NIL


def test_defconstant_makes_its_name_constant():
    """DEFCONSTANT used to write a private table nothing ever read."""
    ev('(defconstant a-test-constant 17)')
    assert ev('(symbol-value (quote a-test-constant))') == 17
    assert ev('(constantp (quote a-test-constant))') is lisptype.T


# ---------------------------------------------------------------------------
# CLHS 25.1.2 -- the environment inquiry and debugging operators
# ---------------------------------------------------------------------------

def test_time_reports_on_trace_output_and_passes_values_through():
    """The report used to go to Python's `sys.stderr`, which is not a Lisp
    stream, so every test that captures it saw the empty string."""
    assert ev('(plusp (length (with-output-to-string (*trace-output*) (time nil))))') \
        is lisptype.T
    # The timed form's own value is unaffected, and the report does not leak on
    # to *STANDARD-OUTPUT*.
    assert ev('(with-output-to-string (*standard-output*)'
              '  (with-output-to-string (*trace-output*) (time nil)))') == ''
    # All of the form's values pass through, not just the primary one.
    assert ev('(equal (multiple-value-list (time (values 1 2 3))) (list 1 2 3))') \
        is lisptype.T
    assert ev('(multiple-value-list (time (values)))') is lisptype.NIL
    assert ev('(let ((x (cons (quote a) (quote b)))) (eq (time x) x))') is lisptype.T


def test_time_requires_exactly_one_form():
    signals('(time)', lisptype.LispProgramError)


def test_room_prints_on_standard_output():
    for source in ('(room)', '(room nil)', '(room t)', '(room :default)'):
        captured = ev(f'(with-output-to-string (*standard-output*) {source})')
        assert len(captured) > 0, source


def test_describe_honours_the_stream_designator():
    """No stream means `*STANDARD-OUTPUT*`, T means `*TERMINAL-IO*`, and an
    explicit stream means that stream and nothing else -- which is exactly what
    `environment/describe.lsp` measures."""
    assert ev('(plusp (length (with-output-to-string (*standard-output*) (describe 17))))') \
        is lisptype.T
    assert ev('(plusp (length (with-output-to-string (s) (describe 17 s))))') \
        is lisptype.T
    # DESCRIBE returns no values.
    assert ev('(multiple-value-list (with-output-to-string (s) (describe 17 s)))') \
        is not None
    assert ev('(with-output-to-string (*standard-output*) (describe 17 (make-string-output-stream)))') \
        == ''


def test_describe_object_is_a_generic_function():
    """A program describes its own classes with a DEFMETHOD, so DESCRIBE-OBJECT
    has to be a generic function rather than a `cl_function` no DEFMETHOD can
    reach (plan.md Finding L)."""
    ev('(defclass a-described-class () ((s :initarg :s)))')
    ev("""(defmethod describe-object ((obj a-described-class) stream)
            (format stream "MARKER ~A END" (slot-value obj (quote s))))""")
    captured = ev("""(with-output-to-string (*standard-output*)
                       (describe (make-instance (quote a-described-class) :s 42)))""")
    assert 'MARKER 42 END' in str(captured)


def test_apropos_list_finds_symbols_in_every_package():
    """With no package argument the search covers every package.

    It could not: `state.packages` holds only the packages a program creates --
    COMMON-LISP, COMMON-LISP-USER and KEYWORD are module-level constants that
    `find_package` special-cases -- so `(apropos-list "CAR")` missed CL:CAR
    while `(apropos-list "CAR" "CL")` found it.
    """
    assert ev("(equal (list (quote car)) (intersection (list (quote car)) (apropos-list \"CAR\")))") \
        is lisptype.T
    # A string designator: string, character or symbol.
    assert ev('(notany (function null) (list (apropos-list "CAR") (apropos-list #\\c) (apropos-list :|CAR|)))') \
        is lisptype.T


def test_apropos_list_returns_a_lisp_list():
    """A Python list is a simple general *vector* here, so returning one would
    make `(member sym (apropos-list ...))` fail against something that prints
    convincingly as a list."""
    assert ev('(listp (apropos-list "CAR"))') is lisptype.T
    assert ev('(consp (apropos-list "CAR"))') is lisptype.T


def test_apropos_prints_matches_and_nothing_when_there_are_none():
    captured = ev('(with-output-to-string (*standard-output*) (apropos "CAR"))')
    assert 'CAR' in str(captured).upper()
    assert ev('(with-output-to-string (*standard-output*) (apropos "QQZZXNOSUCHNAME"))') == ''


def test_list_all_packages_includes_the_standard_packages():
    assert ev("""(let ((names (mapcar (function package-name) (list-all-packages))))
                   (and (member "COMMON-LISP" names :test (function string=))
                        (member "COMMON-LISP-USER" names :test (function string=))
                        (member "KEYWORD" names :test (function string=))
                        t))""") is lisptype.T


def test_trace_records_function_names_and_reports_calls():
    """TRACE and UNTRACE take *unevaluated* function names, so they are macros;
    they used to be `cl_function`s, which evaluated the name as a variable."""
    ev('(defun a-traced-function (x) (car x))')
    ev('(untrace)')
    assert ev('(trace)') is lisptype.NIL
    ev('(trace a-traced-function)')
    assert ev("(equal (trace) (list (quote a-traced-function)))") is lisptype.T
    captured = ev('(with-output-to-string (*trace-output*) (a-traced-function (list 1 2)))')
    assert len(str(captured)) > 0
    ev('(untrace)')
    assert ev('(trace)') is lisptype.NIL
    assert ev('(with-output-to-string (*trace-output*) (a-traced-function (list 1 2)))') == ''


def test_trace_accepts_a_setf_function_name():
    """A function name is a symbol or `(SETF symbol)` (CLHS 3.1.2.1.2.2), and
    `(trace)` answers the specs as written rather than the internal storage
    symbol."""
    ev('(defun (setf a-traced-place) (v a) (setf (car a) v))')
    ev('(untrace)')
    ev('(trace (setf a-traced-place))')
    assert ev("(equal (trace) (list (list (quote setf) (quote a-traced-place))))") \
        is lisptype.T
    ev('(untrace)')


def test_tracing_does_not_change_how_arguments_are_checked():
    """A traced function must behave exactly as it did.

    The wrapper is `*args, **kwargs`, so without `functools.wraps` --
    which `inspect.signature` follows -- `LambdaListShape` would read the
    wrapper's signature instead of the function's and tracing would change the
    function's own arity and keyword checking.
    """
    ev('(defun a-checked-function (a &key (b 5)) (list a b))')
    ev('(untrace)')
    ev('(trace a-checked-function)')
    try:
        assert ev('(equal (a-checked-function 1) (list 1 5))') is lisptype.T
        assert ev('(equal (a-checked-function 1 :b 2) (list 1 2))') is lisptype.T
        signals('(a-checked-function 1 :nope 2)', lisptype.LispProgramError)
    finally:
        ev('(untrace)')


# ---------------------------------------------------------------------------
# CLHS 2.4.1 / 5.1.3 -- a binding's init form is a single-value context
# ---------------------------------------------------------------------------

@pytest.mark.parametrize('source,expected', [
    # FLOOR has always returned two values, so this was latent for as long as
    # `BindingFrame` has existed: the binding held the `MultipleValues`
    # *wrapper* rather than 3.
    ('(let ((x (floor 7 2))) x)', 3),
    ('(let* ((x (floor 7 2))) x)', 3),
    ('(let ((h (make-hash-table))) (setf (gethash 1 h) 5)'
     '  (let ((x (gethash 1 h))) x))', 5),
    ('(do ((x (floor 7 2) (floor 7 2)) (i 0 (1+ i))) ((= i 1) x))', 3),
    ('(dotimes (i 1) (return (let ((x (truncate 9 2))) x)))', 4),
])
def test_a_binding_holds_only_the_primary_value(source, expected):
    assert ev(source) == expected


def test_zero_values_bind_as_nil():
    """CLHS 2.4.1: a form returning *no* values yields NIL where one is wanted."""
    assert ev('(let ((x (values))) x)') is lisptype.NIL


def test_multiple_value_contexts_are_unaffected():
    """The reduction must not reach a genuine multiple-value context."""
    assert ev('(equal (multiple-value-list (floor 7 2)) (list 3 1))') is lisptype.T
    assert ev('(multiple-value-bind (a b) (floor 7 2) (equal (list a b) (list 3 1)))') \
        is lisptype.T
    assert ev('(multiple-value-bind (v p) (gethash 1 (make-hash-table))'
              '  (list v p))') is not None


def test_rt_add_entry_shape_works():
    """The exact shape that aborted the whole ansi-test bootstrap.

    RT's `add-entry` is `(let* ((pred (gethash (name entry) *entries-table*)))
    (cond (pred (setf (cadr pred) entry) ...)))`. With GETHASH correctly
    returning two values, `pred` was bound to the wrapper, so `(setf (cadr
    pred) ...)` signalled "CADR: invalid structure" -- at *load* time, through
    the Lisp LOAD, which propagates. The suite then ran **0 tests**, and
    `scripts/run_ansi.py` never loads `init.lsp`, so no targeted run could
    see it.
    """
    assert ev("""(let ((h (make-hash-table)) (p (list 1 2 3)))
                   (setf (gethash 'k h) p)
                   (let ((pred (gethash 'k h)))
                     (setf (cadr pred) 99)
                     (equal p (list 1 99 3))))""") is lisptype.T


# ---------------------------------------------------------------------------
# CLHS 6.1.1.4 / 6.1.5.3 -- which block a RETURN leaves
# ---------------------------------------------------------------------------

def test_return_clause_returns_from_the_loops_own_block():
    """A NAMED loop's `return` clause returns from *that* loop.

    `loop13.lsp` pins down that the `return` clause and a `(return ...)` form
    written in a `do` clause are different things once the loop is NAMED. When
    the clause was compiled to `RETURN` (i.e. `RETURN-FROM NIL`), the transfer
    sailed past the loop -- which is watching for its own name -- and was
    caught by whatever enclosing NIL block happened to be running. Under
    ansi-test that is RT's own `do-entries` DOLIST, so this one form silently
    *ended the test run*, losing every test registered after it.
    """
    assert ev("(loop named foo return 'a)") \
        == lisptype.COMMON_LISP_USER_PACKAGE.intern_symbol('A')
    # ... while a RETURN *form* in a `do` clause still means RETURN-FROM NIL
    # and escapes the named loop.
    assert ev('(block nil (loop named foo do (return :good)) :bad)') \
        == lisptype.intern_keyword('GOOD')
    assert ev('(block nil (return (loop named foo return :good)) :bad)') \
        == lisptype.intern_keyword('GOOD')


def test_return_clause_does_not_leak_out_of_a_named_loop():
    """The escape was observable as an enclosing iteration ending early."""
    assert ev("""(let ((r nil))
                   (dolist (x (list 1 2 3))
                     (push (loop named foo return :v) r)
                     (push :after r))
                   (= (length r) 6))""") is lisptype.T


def test_named_loop_keeps_conditional_return():
    assert ev('(loop named g for x in (list 1 2 3) when (= x 2) return (* x 10))') == 20


# ---------------------------------------------------------------------------
# CLHS 5.3 -- EQUAL walks the cdr spine iteratively
# ---------------------------------------------------------------------------

def test_equal_handles_lists_longer_than_the_python_stack():
    """A list's length is unbounded in a way its nesting depth is not.

    Recursing on the cdr cost one Python frame per *element*, so two
    1000-element lists exhausted the default limit and EQUAL answered a
    `RecursionError` -- a Python exception as a Lisp value.
    """
    assert ev('(let ((a (make-list 5000 :initial-element :a)))'
              '  (equal a (copy-list a)))') is lisptype.T
    assert ev('(let ((a (make-list 5000 :initial-element :a)))'
              '  (equal a (cons :b (copy-list a))))') is lisptype.NIL


@pytest.mark.parametrize('source,expected', [
    ('(equal (list 1 2 3) (list 1 2 3))', True),
    ('(equal (list 1 2 3) (list 1 2 4))', False),
    ('(equal (cons 1 2) (cons 1 2))', True),
    ('(equal (list (list 1 (list 2))) (list (list 1 (list 2))))', True),
    ('(equal (list 1 2) (list 1 2 3))', False),
    ('(equal "abc" "abc")', True),
    ('(equal "abc" "abd")', False),
    ('(equal 7/2 7/2)', True),
])
def test_equal_still_agrees_on_the_ordinary_cases(source, expected):
    assert ev(source) is (lisptype.T if expected else lisptype.NIL)
