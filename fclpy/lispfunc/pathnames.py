"""Pathname handling for Phase 5 Task 6."""

import os
import pathlib
import fclpy.lisptype as lisptype
from . import registry as _registry


def _ensure_string(obj):
    """Convert LispString to Python string if needed."""
    if isinstance(obj, lisptype.LispString):
        return str(obj)
    return obj


class Pathname:
    """A Pathname object representing a file path with components."""
    
    def __init__(self, path_str):
        """Initialize a pathname from a string.
        
        Args:
            path_str: Path string (can include directory, filename, extension)
        """
        path_str = _ensure_string(path_str)
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
    
    def __fspath__(self):
        """Return the file system path representation.
        
        This implements the os.PathLike protocol, allowing Pathname objects
        to be used directly with Python's os functions (os.path.exists, 
        os.remove, open(), etc.)
        """
        return self.original
    
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
    if isinstance(pathname, (str, lisptype.LispString)):
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
    if isinstance(pathname, (str, lisptype.LispString)):
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
    if isinstance(pathname, (str, lisptype.LispString)):
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
    if isinstance(pathname, (str, lisptype.LispString)):
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
    if isinstance(pathname, (str, lisptype.LispString)):
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
    if isinstance(pathname, (str, lisptype.LispString)):
        pathname = Pathname(pathname)
    elif not isinstance(pathname, Pathname):
        raise TypeError(f"Expected Pathname, got {type(pathname)}")
    
    if pathname.directory:
        return Pathname(pathname.directory)
    return Pathname(".")


@_registry.cl_function('MAKE-PATHNAME')
def make_pathname_function(*args, host=None, device=None, directory=None, name=None, 
                          type=None, version=None, defaults=None, case=None):
    """Make a pathname from components.
    
    This function handles both Python keyword arguments and Lisp-style 
    keyword arguments passed as positional arguments.
    
    Args:
        args: Lisp-style keyword arguments (e.g., :name, nil, :type, nil)
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
    import fclpy.lisptype as lisptype
    
    # Sentinel to track which values were explicitly provided
    _NOT_PROVIDED = object()
    name_explicit = _NOT_PROVIDED
    type_explicit = _NOT_PROVIDED
    
    # Parse Lisp-style keyword arguments if present
    i = 0
    while i < len(args):
        arg = args[i]
        # Check for keyword argument - can be lispKeyword or LispSymbol starting with ':'
        is_keyword = False
        key = None
        if isinstance(arg, lisptype.lispKeyword):
            # lispKeyword stores name without the colon
            is_keyword = True
            key = arg.name.lower()
        elif isinstance(arg, lisptype.LispSymbol) and arg.name.startswith(':'):
            is_keyword = True
            key = arg.name[1:].lower()
        
        if is_keyword and key:
            value = args[i + 1] if i + 1 < len(args) else None
            # Track if NIL was explicitly passed for name/type
            is_nil = value is lisptype.NIL or value is None
            
            if key == 'host':
                host = None if is_nil else value
            elif key == 'device':
                device = None if is_nil else value
            elif key == 'directory':
                directory = None if is_nil else value
            elif key == 'name':
                # Mark as explicitly set (even if to None)
                name_explicit = None if is_nil else value
            elif key == 'type':
                # Mark as explicitly set (even if to None)
                type_explicit = None if is_nil else value
            elif key == 'version':
                version = None if is_nil else value
            elif key == 'defaults':
                defaults = None if is_nil else value
            elif key == 'case':
                case = None if is_nil else value
            i += 2
        else:
            i += 1
    
    # Start with defaults if provided
    dir_parts = []
    result_name = None
    result_type = None
    
    if defaults:
        if isinstance(defaults, Pathname):
            # Extract directory from defaults
            if defaults.directory:
                dir_parts = list(defaults.directory) if isinstance(defaults.directory, (list, tuple)) else [defaults.directory]
            result_name = defaults.filename  # Use filename (without extension)
            result_type = defaults.extension  # Use extension for type
    
    # Override with explicit values
    if directory is not None:
        if isinstance(directory, (list, tuple)):
            dir_parts = list(directory)
        elif isinstance(directory, lisptype.lispCons):
            # Convert Lisp list to Python list
            dir_parts = []
            current = directory
            while isinstance(current, lisptype.lispCons):
                item = current.car
                if isinstance(item, lisptype.LispSymbol):
                    if item.name == ':RELATIVE':
                        pass  # Skip relative marker for now
                    elif item.name == ':WILD':
                        dir_parts.append('*')
                    else:
                        dir_parts.append(item.name.lower())
                elif isinstance(item, str):
                    dir_parts.append(item)
                current = current.cdr
        elif directory is not lisptype.NIL:
            dir_parts = [str(directory)]
    
    # Check if name was explicitly set (use sentinel to distinguish from defaults)
    if name_explicit is not _NOT_PROVIDED:
        # Explicitly set via Lisp-style :name arg - use the explicit value (may be None for NIL)
        if name_explicit is None:
            result_name = None
        elif isinstance(name_explicit, lisptype.LispSymbol):
            if name_explicit.name == ':WILD':
                result_name = '*'
            else:
                result_name = name_explicit.name.lower()
        else:
            result_name = str(name_explicit)
    elif name is not None and name is not lisptype.NIL:
        # Fall back to Python keyword arg with actual value
        if isinstance(name, lisptype.LispSymbol):
            if name.name == ':WILD':
                result_name = '*'
            else:
                result_name = name.name.lower()
        else:
            result_name = str(name)
    # If name was passed as Python kwarg with None/NIL, clear result_name
    # (This handles call like make_pathname_function(name=None, defaults=...))
    # We need to check if 'name' was explicitly passed - but Python doesn't tell us that
    # So we rely on the Lisp-style args being parsed first
    
    # Check if type was explicitly set
    if type_explicit is not _NOT_PROVIDED:
        if type_explicit is None:
            result_type = None
        elif isinstance(type_explicit, lisptype.LispSymbol):
            result_type = type_explicit.name.lower()
        else:
            result_type = str(type_explicit)
    elif type is not None and type is not lisptype.NIL:
        if isinstance(type, lisptype.LispSymbol):
            result_type = type.name.lower()
        else:
            result_type = str(type)
    
    # Build path
    parts = dir_parts[:]
    if result_name:
        if result_type:
            parts.append(f"{result_name}.{result_type}")
        else:
            parts.append(result_name)
    
    path_str = os.path.join(*parts) if parts else "."
    return Pathname(path_str)

@_registry.cl_function('MERGE-PATHNAMES')
def merge_pathnames(pathname, defaults=None):
    """Merge pathname with defaults.
    
    In Common Lisp, MERGE-PATHNAMES fills in missing pathname components
    from defaults. For relative pathnames, the directory is appended to
    the defaults' directory.
    
    Args:
        pathname: Pathname to merge
        defaults: Default pathname to use for missing components
    
    Returns:
        Merged pathname
    """
    if isinstance(pathname, (str, lisptype.LispString)):
        pathname = Pathname(pathname)
    elif not isinstance(pathname, Pathname):
        raise TypeError(f"Expected Pathname, got {type(pathname)}")
    
    if defaults is None:
        return pathname
    
    if isinstance(defaults, (str, lisptype.LispString)):
        defaults = Pathname(defaults)
    elif not isinstance(defaults, Pathname):
        raise TypeError(f"Expected Pathname, got {type(defaults)}")
    
    # Get the pathname's original path for analysis
    pathname_str = pathname.original
    defaults_str = defaults.original
    
    # Check if pathname is just a relative path (no absolute component)
    if not os.path.isabs(pathname_str):
        # For relative paths, join with the defaults base directory
        # Determine the base directory from defaults:
        # 1. If defaults ends with / or \, it's explicitly a directory
        # 2. If defaults is an existing directory, use it as-is
        # 3. If defaults is an existing file, use its parent directory
        # 4. Otherwise, if defaults has a file extension, treat as file (use parent)
        # 5. Otherwise, treat as directory
        if defaults_str.endswith('/') or defaults_str.endswith('\\'):
            base = defaults_str.rstrip('/\\')
        elif os.path.isdir(defaults_str):
            base = defaults_str
        elif os.path.isfile(defaults_str):
            # defaults is a file - use its parent directory
            base = os.path.dirname(defaults_str)
        elif '.' in os.path.basename(defaults_str):
            # Has extension, likely a file - use parent directory
            base = os.path.dirname(defaults_str)
        else:
            # No extension, treat as directory
            base = defaults_str
        
        # Join the relative pathname to the base
        result = os.path.join(base, pathname_str)
        return Pathname(os.path.normpath(result))
    
    # For absolute paths, just use the pathname as-is
    return pathname


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
    """The pathnames matching `pathname` (CLHS 20.2).

    Returns a Lisp **list**, not a Python one: a Python list is this
    implementation's *vector* (plan.md Finding M), so this used to hand back
    something `(listp ...)` denied and `APPEND` could not walk. It went
    unnoticed while APPEND read its arguments through `seq_elements`, which
    accepts any sequence -- ansi-test's own `init.lsp` opens with
    `(append (directory ...) (directory ...) (directory ...))`, so the harness
    bootstrap was the first thing to break once APPEND required lists.
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
    
    from .sequence_protocol import make_lisp_list
    return make_lisp_list(Pathname(match) for match in matches)


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


@_registry.cl_function('HOST-NAMESTRING')
def host_namestring(pathname):
    """Return host portion of pathname.
    
    Args:
        pathname: Pathname object or string
    
    Returns:
        Empty string (host not supported)
    """
    return ""


@_registry.cl_function('ENOUGH-NAMESTRING')
def enough_namestring(pathname, defaults=None):
    """Get enough of pathname to distinguish it from defaults.
    
    Args:
        pathname: Pathname to convert
        defaults: Default pathname (optional)
    
    Returns:
        Namestring sufficient to identify pathname
    """
    if isinstance(pathname, Pathname):
        return pathname.original
    return str(pathname)


@_registry.cl_function('PARSE-NAMESTRING')
def parse_namestring(thing, host=None, defaults=None, **kwargs):
    """Parse a namestring into a pathname.
    
    Args:
        thing: String to parse
        host: Host (ignored)
        defaults: Default pathname (ignored)
    
    Returns:
        Pathname object
    """
    return Pathname(str(thing))


@_registry.cl_function('WILD-PATHNAME-P')
def wild_pathname_p(pathname, field_key=None):
    """Test if pathname has wildcards.
    
    Args:
        pathname: Pathname to test
        field_key: Optional field to check (ignored, checks whole path)
    
    Returns:
        T if contains wildcards, NIL otherwise
    """
    if isinstance(pathname, Pathname):
        path_str = pathname.original
    else:
        path_str = str(pathname)
    
    return lisptype.lisp_bool('*' in path_str or '?' in path_str)


@_registry.cl_function('PATHNAME-MATCH-P')
def pathname_match_p(pathname, wildname):
    """Test if pathname matches a wildcard pattern.
    
    Args:
        pathname: Pathname to test
        wildname: Wildcard pattern
    
    Returns:
        T if matches, NIL otherwise
    """
    import fnmatch
    if isinstance(pathname, Pathname):
        path_str = pathname.original
    else:
        path_str = str(pathname)
    
    if isinstance(wildname, Pathname):
        wild_str = wildname.original
    else:
        wild_str = str(wildname)
    
    return lisptype.lisp_bool(fnmatch.fnmatch(path_str, wild_str))


@_registry.cl_function('TRANSLATE-PATHNAME')
def translate_pathname(source, from_wildname, to_wildname):
    """Translate pathname from one pattern to another.
    
    Args:
        source: Source pathname
        from_wildname: Source pattern
        to_wildname: Target pattern
    
    Returns:
        Translated pathname (simplified - just returns source)
    """
    if isinstance(source, Pathname):
        return source
    return Pathname(str(source))


@_registry.cl_function('LOGICAL-PATHNAME')
def logical_pathname(pathspec):
    """Convert to logical pathname.
    
    Args:
        pathspec: Path specification
    
    Returns:
        Pathname (logical pathnames not fully supported)
    """
    if isinstance(pathspec, Pathname):
        return pathspec
    return Pathname(str(pathspec))


@_registry.cl_function('TRANSLATE-LOGICAL-PATHNAME')
def translate_logical_pathname(pathname, **kwargs):
    """Translate logical pathname to physical pathname.
    
    Args:
        pathname: Logical pathname
    
    Returns:
        Physical pathname (same as input, logical pathnames not supported)
    """
    if isinstance(pathname, Pathname):
        return pathname
    return Pathname(str(pathname))


@_registry.cl_function('LOAD-LOGICAL-PATHNAME-TRANSLATIONS')
def load_logical_pathname_translations(host):
    """Load logical pathname translations for a host.
    
    Args:
        host: Host name
    
    Returns:
        T (no-op, logical pathnames not fully supported)
    """
    return lisptype.T


@_registry.cl_function('LOGICAL-PATHNAME-TRANSLATIONS')
def logical_pathname_translations(host):
    """Get logical pathname translations for a host.
    
    Args:
        host: Host name
    
    Returns:
        NIL (no translations defined)
    """
    return lisptype.NIL
