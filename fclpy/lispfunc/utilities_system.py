"""System information, time operations, and environment access."""

import time
import socket
import platform
import os
import fclpy.lisptype as lisptype
from fclpy.lispfunc import registry as _registry


# --- Time functions ---
@_registry.cl_function('GET-UNIVERSAL-TIME')
def get_universal_time():
    """Get current time as universal time (seconds since 1900-01-01)."""
    import datetime
    epoch = datetime.datetime(1900, 1, 1)
    return int((datetime.datetime.utcnow() - epoch).total_seconds())


@_registry.cl_function('DECODE-UNIVERSAL-TIME')
def decode_universal_time(universal_time=None):
    """Decode universal time into components (second, minute, hour, day, month, year, day-of-week)."""
    import datetime
    if universal_time is None:
        universal_time = get_universal_time()
    epoch = datetime.datetime(1900, 1, 1)
    dt = epoch + datetime.timedelta(seconds=universal_time)
    return (dt.second, dt.minute, dt.hour, dt.day, dt.month, dt.year, None)


@_registry.cl_function('GET-DECODED-TIME')
def get_decoded_time():
    """Get current time in decoded form."""
    return decode_universal_time(get_universal_time())


@_registry.cl_function('TIME')
def time_fn():
    """Return current time in seconds since epoch."""
    import time as _time
    return _time.time()


def sleep(seconds):
    """Sleep for given number of seconds."""
    import time
    time.sleep(seconds)
    return None


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
    return socket.gethostname()


@_registry.cl_function('MACHINE-TYPE')
def machine_type():
    """Get machine type (CPU architecture)."""
    return platform.machine()


@_registry.cl_function('MACHINE-VERSION')
def machine_version():
    """Get machine version (platform string)."""
    return platform.platform()


@_registry.cl_function('SOFTWARE-TYPE')
def software_type():
    """Get software type (operating system)."""
    return platform.system()


@_registry.cl_function('SOFTWARE-VERSION')
def software_version():
    """Get software version (OS release)."""
    return platform.release()


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
def user_homedir_pathname():
    """Get user home directory."""
    return os.path.expanduser("~")


def get_env(name):
    """Get environment variable."""
    return os.environ.get(name)


def exit(code=0):
    """Exit the Lisp system."""
    import sys
    sys.exit(code)


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
            # Create a new random seed using OS entropy + high-res counter
            import os
            import time
            # Use combination of OS random bytes and high-resolution time
            entropy = os.urandom(16) + str(time.perf_counter_ns()).encode()
            self._random.seed(entropy)
        elif isinstance(seed, RandomState):
            # Copy state from another RandomState
            self._random.setstate(seed._random.getstate())
        elif isinstance(seed, (int, float)):
            self._random.seed(seed)
        elif isinstance(seed, tuple):
            # Try to restore from state tuple
            try:
                self._random.setstate(seed)
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


# Global default random state
_DEFAULT_RANDOM_STATE = RandomState()


@_registry.cl_function('RANDOM')
def random(limit, state=None):
    """Generate random number up to limit.
    
    Args:
        limit: Upper bound (exclusive). Must be positive integer or float.
        state: Optional random state to use. If None, uses *RANDOM-STATE*.
    
    Returns:
        Random integer in [0, limit) if limit is integer.
        Random float in [0.0, limit) if limit is float.
    """
    rs = state if isinstance(state, RandomState) else _DEFAULT_RANDOM_STATE
    
    if isinstance(limit, int):
        if limit <= 0:
            raise lisptype.LispError("RANDOM: limit must be positive")
        return rs.randrange(limit)
    elif isinstance(limit, float):
        if limit <= 0:
            raise lisptype.LispError("RANDOM: limit must be positive")
        return rs.random() * limit
    else:
        raise lisptype.LispNotImplementedError("RANDOM: invalid limit type")


@_registry.cl_function('MAKE-RANDOM-STATE')
def make_random_state(state=None):
    """Make random state object.
    
    Args:
        state: Controls how to initialize:
            - NIL or omitted: Copy current *RANDOM-STATE*
            - T: Create fresh state from entropy
            - RandomState: Copy that state
    
    Returns:
        A new RandomState object.
    """
    if state is None or state is lisptype.NIL:
        # Copy the default random state
        return RandomState(_DEFAULT_RANDOM_STATE)
    elif state is True or state is lisptype.T:
        # Create a truly random new state
        return RandomState(True)
    elif isinstance(state, RandomState):
        # Copy the provided state
        return RandomState(state)
    else:
        raise lisptype.LispError("MAKE-RANDOM-STATE: invalid state argument")


@_registry.cl_function('RANDOM-STATE-P')
def random_state_p(object):
    """Test if object is random state."""
    return lisptype.lisp_bool(isinstance(object, RandomState))


def get_random_state():
    """Get the current *RANDOM-STATE* value."""
    return _DEFAULT_RANDOM_STATE


def set_random_state(state):
    """Set the *RANDOM-STATE* value."""
    global _DEFAULT_RANDOM_STATE
    if isinstance(state, RandomState):
        _DEFAULT_RANDOM_STATE = state
    else:
        raise lisptype.LispError("*RANDOM-STATE* must be a RandomState object")


__all__ = [
    'get_universal_time',
    'decode_universal_time',
    'get_decoded_time',
    'time_fn',
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
    'get_random_state',
    'set_random_state',
]
