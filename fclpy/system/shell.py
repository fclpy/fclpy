"""
Shell interface and default implementation.
"""

import os


class ShellInterface:
    def home(self):
        pass


class DefaultShell(ShellInterface):
    def __init__(self):
        super().__init__()

    def home(self):
        return os.path.expanduser("~")


shell = DefaultShell()
