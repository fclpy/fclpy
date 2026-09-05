"""
Kernel interface and default implementation.
"""

import time
import sys
import os
import platform
import socket


class KernelInterface:
    def __init__(self):
        pass

    def sleep(self, seconds):
        raise NotImplementedError()

    def exit(self, code=0):
        raise NotImplementedError()

    def hard_exit(self, code=0):
        raise NotImplementedError()

    def os_name(self):
        raise NotImplementedError()

    def machine_instance(self):
        raise NotImplementedError()

    def machine_type(self):
        raise NotImplementedError()

    def machine_version(self):
        raise NotImplementedError()

    def software_type(self):
        raise NotImplementedError()

    def software_version(self):
        raise NotImplementedError()

    def time(self):
        raise NotImplementedError()

    def monotonic(self):
        raise NotImplementedError()

    def perf_counter(self):
        raise NotImplementedError()

    def process_time(self):
        raise NotImplementedError()

    def localtime(self, secs=None):
        raise NotImplementedError()

    def mktime(self, t):
        raise NotImplementedError()

    def strftime(self, format, t=None):
        raise NotImplementedError()

    def daylight(self):
        raise NotImplementedError()

    def timezone(self):
        raise NotImplementedError()

    def altzone(self):
        raise NotImplementedError()

    def entropy(self, n=16):
        raise NotImplementedError()

    def python_implementation(self):
        raise NotImplementedError()

    def python_version(self):
        raise NotImplementedError()

    def float_max(self):
        raise NotImplementedError()

    def float_min(self):
        raise NotImplementedError()

    def float_mant_dig(self):
        raise NotImplementedError()


class DefaultKernel(KernelInterface):
    def __init__(self):
        super().__init__()

    def sleep(self, seconds):
        time.sleep(seconds)

    def exit(self, code=0):
        sys.exit(code)

    def hard_exit(self, code=0):
        os._exit(code)

    def os_name(self):
        return os.name

    def machine_instance(self):
        return socket.gethostname()

    def machine_type(self):
        return platform.machine()

    def machine_version(self):
        return platform.platform()

    def software_type(self):
        return platform.system()

    def software_version(self):
        return platform.release()

    def time(self):
        return time.time()

    def monotonic(self):
        return time.monotonic()

    def perf_counter(self):
        return time.perf_counter()

    def process_time(self):
        return time.process_time()

    def localtime(self, secs=None):
        return time.localtime(secs)

    def mktime(self, t):
        return time.mktime(t)

    def strftime(self, format, t=None):
        return time.strftime(format, t)

    def daylight(self):
        return time.daylight

    def timezone(self):
        return time.timezone

    def altzone(self):
        return time.altzone

    def entropy(self, n=16):
        return os.urandom(n)

    def python_implementation(self):
        return sys.implementation.name

    def python_version(self):
        return sys.version_info[:3]

    def float_max(self):
        return sys.float_info.max

    def float_min(self):
        return sys.float_info.min

    def float_mant_dig(self):
        return sys.float_info.mant_dig


kernel = DefaultKernel()
