"""
Shell interface and default implementation.
"""

import os
import sys

class ShellInterface:

    def __init__(self):
        pass

    def home(self):
        raise NotImplementedError()

    def get_env(self, key):
        raise NotImplementedError()

    def get_stdin(self):
        raise NotImplementedError()

    def get_stdout(self):
        raise NotImplementedError()

    def get_stderr(self):
        raise NotImplementedError()


class DefaultShell(ShellInterface):
    def __init__(self):
        super().__init__()

    def home(self):
        return os.path.expanduser("~")
    
    def get_env(self, key):
        return os.getenv(key)

    def get_stdin(self):
        return sys.stdin

    def get_stdout(self):
        return sys.stdout

    def get_stderr(self):
        return sys.stderr


shell = DefaultShell()
