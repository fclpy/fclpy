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
@_registry.cl_function('RANDOM')
def random(limit, state=None):
    """Generate random number up to limit."""
    import random as rnd
    if isinstance(limit, int):
        return rnd.randrange(limit)
    elif isinstance(limit, float):
        return rnd.random() * limit
    else:
        raise lisptype.LispNotImplementedError("RANDOM: invalid limit type")


@_registry.cl_function('MAKE-RANDOM-STATE')
def make_random_state(state=None):
    """Make random state object."""
    import random as rnd
    new_state = rnd.getstate()
    if state is not None:
        rnd.setstate(state)
    return new_state


@_registry.cl_function('RANDOM-STATE-P')
def random_state_p(object):
    """Test if object is random state."""
    return lisptype.lisp_bool(isinstance(object, tuple) and len(object) >= 2)


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
]
