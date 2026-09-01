import io
import logging

import pytest


def pytest_configure(config):
    logging.basicConfig(level=logging.INFO)


# ---------------------------------------------------------------------------
# Reading through the reader the implementation actually uses.
#
# `fclpy/reader.py` is a separate ~480-line reader that no module under
# `fclpy/` imports; the live path is `tokenizer.py` -> `lispreader.py` ->
# `readtable.py` (CLAUDE.md's architecture map). Four test files were written
# against the dead one, so ~178 tests measured code that never runs while the
# real reader went untested at the unit level -- and the two disagree on
# conformance: the dead reader splits "123abc" into the integer 123, where the
# live reader correctly answers the symbol |123ABC|.
#
# These live here, in the file that already exists for shared test
# infrastructure, so the four files share one definition instead of copying the
# setup four times.
# ---------------------------------------------------------------------------

def _live_reader(text):
    """A `LispReader` over `text`, reading the way the interpreter does.

    The environment is established first because the reader resolves
    `*READTABLE*` and `*PACKAGE*` through the symbols' value cells, and
    `reset_lisp_state` below clears `state` before every test.
    """
    import fclpy.lispreader as lispreader
    import fclpy.state as state
    from fclpy import lispenv
    from fclpy.readtable import get_current_readtable
    if state.current_environment is None or not state.functions_loaded:
        lispenv.setup_standard_environment()
    return lispreader.LispReader(get_current_readtable(),
                                 lispreader.LispStream(io.StringIO(text)))


def read(text):
    """The first object in `text`.

    Reports failure the way the live reader does, which is the ANSI
    distinction (CLHS 23.1): `EOFError` when the input ends in the middle of an
    object, `lispreader.ReaderErrorSignal` when it is malformed.
    """
    return _live_reader(text).read_1()


def read_all(text):
    """Every object in `text`. Propagates a failure part-way through."""
    reader = _live_reader(text)
    out = []
    while True:
        obj = reader.read_1()      # None at a clean end of input
        if obj is None:
            return out
        out.append(obj)


def read_in_package(text, package):
    """`read(text)` with `*PACKAGE*` bound to `package`, so that unqualified
    symbols intern there -- what the dead reader's `Reader(package=...)`
    constructor argument meant."""
    from fclpy.lispfunc.binding import dynamic_value, set_dynamic_value
    from fclpy.lisptype import LispSymbol
    import fclpy.state as state
    from fclpy import lispenv
    if state.current_environment is None or not state.functions_loaded:
        lispenv.setup_standard_environment()
    sym = LispSymbol('*PACKAGE*')
    previous_mirror = state.current_package
    saved = dynamic_value(sym, None)
    try:
        set_dynamic_value(sym, package)
        state.current_package = package
        return read(text)
    finally:
        set_dynamic_value(sym, saved)
        state.current_package = previous_mirror

class Results:
    def __init__(self):
        self.passed = 0
        self.failed = 0
        self.errors = []

    def test(self, name, condition, error_msg=None):
        if condition:
            self.passed += 1
        else:
            self.failed += 1
            if error_msg:
                self.errors.append(f"{name}: {error_msg}")

    def summary(self):
        return self.failed == 0


@pytest.fixture
def results():
    """Provide a lightweight Results object for existing test functions expecting it."""
    return Results()


@pytest.fixture(autouse=True)
def reset_lisp_state():
    """Reset global Lisp state before each test to ensure isolation.
    
    This prevents state corruption when test files reset their environments,
    ensuring each test starts with a fresh, properly initialized state.
    """
    import fclpy.state as state
    # Reset state BEFORE each test
    state.functions_loaded = False
    state.current_environment = None
    yield
    # Cleanup after test
    state.functions_loaded = False
    state.current_environment = None
