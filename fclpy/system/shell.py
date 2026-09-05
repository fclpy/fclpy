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

    def get_startup_cwd(self):
        raise NotImplementedError()

    def input(self, prompt=''):
        raise NotImplementedError()

    def print(self, *args, sep=' ', end='\n', file=None, flush=True):
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

    def get_startup_cwd(self):
        return os.getcwd()

    def input(self, prompt=''):
        return input(prompt)

    def print(self, *args, sep=' ', end='\n', file=None, flush=True):
        print(*args, sep=sep, end=end,
              file=self.get_stdout() if file is None else file, flush=flush)


shell = DefaultShell()
