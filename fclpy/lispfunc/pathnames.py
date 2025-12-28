"""Pathname handling for Phase 5 Task 6."""

import os
import pathlib
import fclpy.lisptype as lisptype
from . import registry as _registry


class Pathname:
    """A Pathname object representing a file path with components."""
    
    def __init__(self, path_str):
        """Initialize a pathname from a string.
        
        Args:
            path_str: Path string (can include directory, filename, extension)
        """
        self.original = str(path_str)
        
        # Parse path using pathlib
        path_obj = pathlib.Path(path_str)
        
        # Store components
        self.directory = str(path_obj.parent) if path_obj.parent != pathlib.Path('.') else None
        self.filename = path_obj.stem  # Name without extension
        self.extension = path_obj.suffix[1:] if path_obj.suffix else None  # Remove leading dot
        self.name = path_obj.name  # Full filename with extension
        
        # Full path components
        self.parts = list(path_obj.parts)
    
    def __str__(self):
        """Return string representation."""
        return self.original
    
    def __repr__(self):
        """Return repr."""
        return f"Pathname({self.original!r})"
    
    def to_list(self):
        """Convert to list representation for Lisp."""
        return [
            ('directory', self.directory),
            ('filename', self.filename),
            ('extension', self.extension),
            ('name', self.name)
        ]


@_registry.cl_function('PATHNAME')
def make_pathname(pathspec=None, host=None, device=None, directory=None,
                  name=None, type=None, version=None):
    """Make a pathname object.
    
    Args:
        pathspec: Path string (primary way to create)
        host: Host (not used)
        device: Device (not used)
        directory: Directory component (string or list)
        name: Filename component
        type: Extension (called 'type' in CL)
        version: Version (not supported)
    
    Returns:
        Pathname object
    """
    if pathspec is not None:
        # Create from pathspec string
        return Pathname(pathspec)
    
    # Create from components
    parts = []
    if directory:
        if isinstance(directory, (list, tuple)):
            parts.extend(directory)
        else:
            parts.append(directory)
    
    if name:
        parts.append(name)
    
    # Add extension/type if provided
    if type:
        parts[-1] = f"{name}.{type}"
    
    path_str = os.path.join(*parts) if parts else "."
    return Pathname(path_str)


@_registry.cl_function('PATHNAMEP')
def pathnamep(obj):
    """Test if object is a pathname.
    
    Args:
        obj: Object to test
    
    Returns:
        T if pathname, NIL otherwise
    """
    return lisptype.lisp_bool(isinstance(obj, Pathname))


@_registry.cl_function('PATHNAME-DIRECTORY')
def pathname_directory(pathname):
    """Get directory component of pathname.
    
    Args:
        pathname: Pathname object or string
    
    Returns:
        Directory string or NIL
    """
    if isinstance(pathname, str):
        pathname = Pathname(pathname)
    elif not isinstance(pathname, Pathname):
        raise TypeError(f"Expected Pathname, got {type(pathname)}")
    
    if pathname.directory:
        return pathname.directory
    return lisptype.NIL


@_registry.cl_function('PATHNAME-NAME')
def pathname_name(pathname):
    """Get filename component of pathname.
    
    Args:
        pathname: Pathname object or string
    
    Returns:
        Filename string or NIL
    """
    if isinstance(pathname, str):
        pathname = Pathname(pathname)
    elif not isinstance(pathname, Pathname):
        raise TypeError(f"Expected Pathname, got {type(pathname)}")
    
    if pathname.filename:
        return pathname.filename
    return lisptype.NIL


@_registry.cl_function('PATHNAME-TYPE')
def pathname_type(pathname):
    """Get extension (type) component of pathname.
    
    Args:
        pathname: Pathname object or string
    
    Returns:
        Extension string or NIL
    """
    if isinstance(pathname, str):
        pathname = Pathname(pathname)
    elif not isinstance(pathname, Pathname):
        raise TypeError(f"Expected Pathname, got {type(pathname)}")
    
    if pathname.extension:
        return pathname.extension
    return lisptype.NIL


@_registry.cl_function('PATHNAME-HOST')
def pathname_host(pathname):
    """Get host component of pathname.
    
    Args:
        pathname: Pathname object or string
    
    Returns:
        Host (not supported, always NIL)
    """
    return lisptype.NIL


@_registry.cl_function('PATHNAME-DEVICE')
def pathname_device(pathname):
    """Get device component of pathname.
    
    Args:
        pathname: Pathname object or string
    
    Returns:
        Device (not supported, always NIL)
    """
    return lisptype.NIL


@_registry.cl_function('PATHNAME-VERSION')
def pathname_version(pathname):
    """Get version component of pathname.
    
    Args:
        pathname: Pathname object or string
    
    Returns:
        Version (not supported, always NIL)
    """
    return lisptype.NIL


@_registry.cl_function('NAMESTRING')
def namestring(pathname):
    """Convert pathname to string.
    
    Args:
        pathname: Pathname object or string
    
    Returns:
        Path string
    """
    if isinstance(pathname, Pathname):
        return pathname.original
    return str(pathname)


@_registry.cl_function('FILE-NAMESTRING')
def file_namestring(pathname):
    """Get just the filename part of pathname.
    
    Args:
        pathname: Pathname object or string
    
    Returns:
        Filename with extension (if any)
    """
    if isinstance(pathname, str):
        pathname = Pathname(pathname)
    elif not isinstance(pathname, Pathname):
        raise TypeError(f"Expected Pathname, got {type(pathname)}")
    
    return pathname.name


@_registry.cl_function('DIRECTORY-NAMESTRING')
def directory_namestring(pathname):
    """Get just the directory part of pathname.
    
    Args:
        pathname: Pathname object or string
    
    Returns:
        Directory string
    """
    if isinstance(pathname, str):
        pathname = Pathname(pathname)
    elif not isinstance(pathname, Pathname):
        raise TypeError(f"Expected Pathname, got {type(pathname)}")
    
    if pathname.directory:
        return pathname.directory
    return lisptype.NIL


@_registry.cl_function('PATHNAME-WITHOUT-NAME-TYPE')
def pathname_without_name_type(pathname):
    """Get pathname with only directory component.
    
    Args:
        pathname: Pathname object or string
    
    Returns:
        Pathname with only directory
    """
    if isinstance(pathname, str):
        pathname = Pathname(pathname)
    elif not isinstance(pathname, Pathname):
        raise TypeError(f"Expected Pathname, got {type(pathname)}")
    
    if pathname.directory:
        return Pathname(pathname.directory)
    return Pathname(".")


@_registry.cl_function('MAKE-PATHNAME')
def make_pathname_function(host=None, device=None, directory=None, name=None, 
                          type=None, version=None, defaults=None, case=None):
    """Make a pathname from components.
    
    Args:
        host: Host (not used)
        device: Device (not used)
        directory: Directory component
        name: Name component
        type: Type/extension component
        version: Version (not used)
        defaults: Default pathname to merge with
        case: Case conversion (not used)
    
    Returns:
        Pathname object
    """
    parts = []
    
    if directory:
        if isinstance(directory, (list, tuple)):
            parts.extend(directory)
        else:
            parts.append(directory)
    
    if name:
        name_part = name
        if type:
            name_part = f"{name}.{type}"
        parts.append(name_part)
    
    path_str = os.path.join(*parts) if parts else "."
    return Pathname(path_str)


@_registry.cl_function('MERGE-PATHNAMES')
def merge_pathnames(pathname, defaults=None):
    """Merge pathname with defaults.
    
    Args:
        pathname: Pathname to merge
        defaults: Default pathname to use for missing components
    
    Returns:
        Merged pathname
    """
    if isinstance(pathname, str):
        pathname = Pathname(pathname)
    elif not isinstance(pathname, Pathname):
        raise TypeError(f"Expected Pathname, got {type(pathname)}")
    
    if defaults is None:
        return pathname
    
    if isinstance(defaults, str):
        defaults = Pathname(defaults)
    elif not isinstance(defaults, Pathname):
        raise TypeError(f"Expected Pathname, got {type(defaults)}")
    
    # Use pathname components, fill in from defaults
    directory = pathname.directory or defaults.directory
    name = pathname.name or defaults.name
    
    parts = []
    if directory:
        parts.append(directory)
    if name:
        parts.append(name)
    
    path_str = os.path.join(*parts) if parts else "."
    return Pathname(path_str)


@_registry.cl_function('FILE-WRITE-DATE')
def file_write_date(pathname):
    """Get file modification time (as integer timestamp).
    
    Args:
        pathname: File pathname
    
    Returns:
        Integer timestamp or NIL if file doesn't exist
    """
    if isinstance(pathname, Pathname):
        pathname = pathname.original
    else:
        pathname = str(pathname)
    
    if os.path.exists(pathname):
        import time
        mtime = os.path.getmtime(pathname)
        return int(mtime)
    return lisptype.NIL


@_registry.cl_function('PROBE-FILE')
def probe_file(pathname):
    """Check if file exists.
    
    Args:
        pathname: File pathname
    
    Returns:
        Pathname if exists, NIL otherwise
    """
    if isinstance(pathname, Pathname):
        path_str = pathname.original
    else:
        path_str = str(pathname)
    
    if os.path.exists(path_str) and os.path.isfile(path_str):
        return Pathname(path_str)
    return lisptype.NIL


@_registry.cl_function('DIRECTORY')
def directory(pathname):
    """List directory contents.
    
    Args:
        pathname: Directory pathname (can include wildcards)
    
    Returns:
        List of pathnames matching pattern
    """
    if isinstance(pathname, Pathname):
        path_str = pathname.original
    else:
        path_str = str(pathname)
    
    import glob
    
    # Handle wildcards
    if '*' in path_str or '?' in path_str:
        matches = glob.glob(path_str)
    else:
        # List directory contents
        if os.path.isdir(path_str):
            matches = [os.path.join(path_str, name) for name in os.listdir(path_str)]
        else:
            matches = []
    
    return [Pathname(match) for match in matches]


@_registry.cl_function('TRUENAME')
def truename(pathname):
    """Get canonical absolute pathname.
    
    Args:
        pathname: File pathname
    
    Returns:
        Pathname with absolute canonical path
    """
    if isinstance(pathname, Pathname):
        path_str = pathname.original
    else:
        path_str = str(pathname)
    
    # Check if file exists first
    if not os.path.exists(path_str):
        raise FileNotFoundError(f"File not found: {path_str}")
    
    try:
        real_path = os.path.realpath(path_str)
        return Pathname(real_path)
    except (OSError, ValueError):
        raise FileNotFoundError(f"Cannot resolve pathname: {path_str}")


@_registry.cl_function('ABSOLUTE-PATHNAME-P')
def absolute_pathname_p(pathname):
    """Test if pathname is absolute.
    
    Args:
        pathname: Pathname object or string
    
    Returns:
        T if absolute, NIL otherwise
    """
    if isinstance(pathname, Pathname):
        path_str = pathname.original
    else:
        path_str = str(pathname)
    
    return lisptype.lisp_bool(os.path.isabs(path_str))


@_registry.cl_function('RELATIVE-PATHNAME-P')
def relative_pathname_p(pathname):
    """Test if pathname is relative.
    
    Args:
        pathname: Pathname object or string
    
    Returns:
        T if relative, NIL otherwise
    """
    if isinstance(pathname, Pathname):
        path_str = pathname.original
    else:
        path_str = str(pathname)
    
    return lisptype.lisp_bool(not os.path.isabs(path_str))
