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


class DefaultKernel(KernelInterface):
    def __init__(self):
        super().__init__()

    def sleep(self, seconds):
        time.sleep(seconds)

    def exit(self, code=0):
        sys.exit(code)

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


kernel = DefaultKernel()
