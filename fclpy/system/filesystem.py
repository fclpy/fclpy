"""
File system interface and default implementation.
"""

import os


class FileSystemInterface:
    def __init__(self):
        raise NotImplementedError()

    def exists(self, path):
        raise NotImplementedError()

    def isdir(self, path):
        raise NotImplementedError()

    def rmdir(self, path):
        raise NotImplementedError()

    def delete(self, path):
        raise NotImplementedError()

    def file_size(self, path):
        raise NotImplementedError()

    def ensure_dir(self, path):
        raise NotImplementedError()

    def rename(self, old_path, new_path):
        raise NotImplementedError()

    def open(self, path, mode, encoding=None, newline=None):
        raise NotImplementedError()

    def read(self, fh, n=None):
        raise NotImplementedError()

    def write(self, fh, data):
        raise NotImplementedError()


class DefaultFileSystem(FileSystemInterface):
    def __init__(self):
        super().__init__()

    def exists(self, path):
        return os.path.exists(path)

    def isdir(self, path):
        return os.path.isdir(path)

    def rmdir(self, path):
        return os.rmdir(path)

    def delete(self, path):
        return os.remove(path)

    def file_size(self, file_id):
        return os.fstat(file_id).st_size

    def ensure_dir(self, path):
        return os.makedirs(path, exist_ok=True)

    def rename(self, old_path, new_path):
        return os.rename(old_path, new_path)

    def open(self, path, mode, encoding=None, newline=None):
        kwargs = {}
        if encoding is not None:
            kwargs['encoding'] = encoding
        if newline is not None:
            kwargs['newline'] = newline
        return open(path, mode, **kwargs)

    def read(self, fh, n=None):
        if n is None:
            return fh.read()
        return fh.read(n)

    def write(self, fh, data):
        return fh.write(data)


fs = DefaultFileSystem()
