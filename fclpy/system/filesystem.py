"""
File system interface and default implementation.
"""

from glob import glob
import os

# Whence constants for `seek`, re-exported so callers never need `os`.
SEEK_SET = os.SEEK_SET
SEEK_CUR = os.SEEK_CUR
SEEK_END = os.SEEK_END


class FileSystemInterface:
    def __init__(self):
        pass

    def exists(self, path):
        raise NotImplementedError()

    def isdir(self, path):
        raise NotImplementedError()

    def isfile(self, path):
        raise NotImplementedError()

    def rmdir(self, path):
        raise NotImplementedError()

    def delete(self, path):
        raise NotImplementedError()

    def file_size(self, file_id):
        raise NotImplementedError()

    def ensure_dir(self, path):
        raise NotImplementedError()

    def rename(self, old_path, new_path):
        raise NotImplementedError()

    def replace(self, old_path, new_path):
        raise NotImplementedError()

    def splitext(self, path):
        raise NotImplementedError()

    def open(self, path, mode, encoding=None, newline=None):
        raise NotImplementedError()

    def read(self, fh, n=None):
        raise NotImplementedError()

    def write(self, fh, data):
        raise NotImplementedError()

    def get_file_id(self, fh):
        raise NotImplementedError()

    def close(self, fh):
        raise NotImplementedError()

    def flush(self, fh):
        raise NotImplementedError()

    def pos(self, fh):
        raise NotImplementedError()

    def readline(self, fh):
        raise NotImplementedError()

    def glob(self, pattern):
        raise NotImplementedError()

    def get_dirs(self, path):
        raise NotImplementedError()

    def abspath(self, path):
        raise NotImplementedError()
    
    def isabs(self, path):
        raise NotImplementedError()

    def normpath(self, path):
        raise NotImplementedError()

    def join(self, *paths):
        raise NotImplementedError()

    def dirname(self, path):
        raise NotImplementedError()

    def realpath(self, path):
        raise NotImplementedError()

    def getmtime(self, path):
        raise NotImplementedError()
    
    def get_path_sep(self):
        raise NotImplementedError()
    
    def seek(self, fh, offset, whence=os.SEEK_SET):
        raise NotImplementedError()


class DefaultFileSystem(FileSystemInterface):
    def __init__(self):
        super().__init__()

    def exists(self, path):
        return os.path.exists(path)

    def isdir(self, path):
        return os.path.isdir(path)

    def isfile(self, path):
        return os.path.isfile(path)

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

    def replace(self, old_path, new_path):
        return os.replace(old_path, new_path)

    def splitext(self, path):
        return os.path.splitext(path)

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

    def get_file_id(self, fh):
        return fh.fileno()

    def close(self, fh):
        return fh.close()

    def flush(self, fh):
        return fh.flush()

    def pos(self, fh):
        return fh.tell()
    
    def readline(self, fh):
        return fh.readline()

    def glob(self, pattern):
        return glob(pattern)

    def get_dirs(self, path):
        return os.listdir(path)

    def abspath(self, path):
        return os.path.abspath(path)


    def join(self, *paths):
        return os.path.join(*paths)

    def dirname(self, path):
        return os.path.dirname(path)

    def realpath(self, path):
        return os.path.realpath(path)

    def getmtime(self, path):
        return os.path.getmtime(path)

    def get_path_sep(self):
        return os.path.sep

    def isabs(self, path):
        return os.path.isabs(path)

    def normpath(self, path):
        return os.path.normpath(path)
    
    def seek(self, fh, offset, whence=os.SEEK_SET):
        return fh.seek(offset, whence)

    

fs = DefaultFileSystem()
