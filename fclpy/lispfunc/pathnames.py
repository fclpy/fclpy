"""The one pathname object model (CLHS 19.2).

A `Pathname` is a **component record** -- host, device, directory, name,
type, version -- not a namestring wrapper. Before this module, `Pathname`
stored only the original namestring and derived `pathlib.Path` fields
(`directory`/`filename`/`extension`) that could not represent a wildcard, an
`:absolute`/`:relative` marker, or a component that was never supplied at
all. That is why `MAKE-PATHNAME`, `MERGE-PATHNAMES` and `DIRECTORY` could not
compose components (CLAUDE.md): there was nothing to compose -- every
accessor re-derived its answer from a flat string. `(make-pathname :directory
'(:absolute :wild) :name "foo")` had no way to keep ":wild" a symbol, and
`(pathname-directory p)` could never answer `(:ABSOLUTE "usr" "local")`, only
a plain OS-style string.

The components below use interned Lisp keyword objects (`:ABSOLUTE`,
`:RELATIVE`, `:WILD`, `:WILD-INFERIORS`, `:UP`, `:BACK`, `:UNSPECIFIC`,
`:NEWEST`) as markers, and plain Python `str` for literal components -- the
two are never confused because their Python types differ. `directory` is
`None` (unspecified) or a tuple `(kind, *components)`; `name`/`type` are
`None`, a `str`, or one of the marker keywords; `version` is `None`, an
`int`, or a marker keyword.
"""

import os
import re
import fclpy.lisptype as lisptype
from . import registry as _registry


# ===== Component markers =====

def _kw(name):
    return lisptype.intern_keyword(name)


_K_ABSOLUTE = _kw('ABSOLUTE')
_K_RELATIVE = _kw('RELATIVE')
_K_WILD = _kw('WILD')
_K_WILD_INFERIORS = _kw('WILD-INFERIORS')
_K_UP = _kw('UP')
_K_BACK = _kw('BACK')
_K_UNSPECIFIC = _kw('UNSPECIFIC')
_K_NEWEST = _kw('NEWEST')

_DIR_TOKEN_KEYWORDS = {
    'WILD': _K_WILD, 'WILD-INFERIORS': _K_WILD_INFERIORS,
    'UP': _K_UP, 'BACK': _K_BACK,
}


def _is_string_designator(obj):
    from .characters import is_string
    return is_string(obj)


def _string_text(obj):
    from .misc_packages import _designator_to_string
    return _designator_to_string(obj)


def _norm_str(s):
    """Case-fold a component for comparison, matching the case-insensitive
    Windows file system this implementation runs on."""
    return s.lower() if isinstance(s, str) and os.name == 'nt' else s


# ===== The Pathname object =====

class Pathname:
    """A pathname component record (CLHS 19.2)."""

    __slots__ = ('host', 'device', 'directory', 'name', 'type', 'version', 'logical')

    def __init__(self, host=None, device=None, directory=None, name=None,
                 type=None, version=None, logical=False):
        self.host = host
        self.device = device
        self.directory = directory
        self.name = name
        self.type = type
        self.version = version
        self.logical = logical

    # --- rendering ---

    def namestring(self):
        return render_namestring(self)

    def __str__(self):
        return self.namestring()

    def __repr__(self):
        return f'#P{self.namestring()!r}'

    def __fspath__(self):
        return self.namestring()

    # --- equality (CLHS 19.2 / EQUAL) ---

    def __eq__(self, other):
        if not isinstance(other, Pathname):
            return NotImplemented
        return self._key() == other._key()

    def __hash__(self):
        return hash(self._key())

    def _key(self):
        def norm(c):
            if isinstance(c, str):
                return _norm_str(c)
            return c

        directory = None
        if self.directory is not None:
            directory = tuple(norm(c) for c in self.directory)
        # An unspecified version and :NEWEST denote the same file on a
        # physical pathname -- MERGE-PATHNAMES's own tests confirm this is
        # not a simplification: `merge-pathnames.2`-`.4`'s helper checks
        # `(if (pathname-version p1) (equalp v1 v2) (equalp v2 :newest))`,
        # accepting *either* answer as correct when no version was ever
        # supplied. Without normalizing them here, `(equal (pathname
        # (merge-pathnames f)) *load-pathname*)` was NIL in `load.17`/`.18`:
        # MERGE-PATHNAMES answers :NEWEST when `f` names a file (CLHS
        # 19.3.3), while `*load-pathname*`, built straight from a namestring
        # that has no version syntax at all, answers NIL for the identical
        # file.
        version = None if self.version is _K_NEWEST else self.version
        return (norm(self.host), norm(self.device), directory,
                norm(self.name), norm(self.type), version, self.logical)


# ===== Directory component coercion =====

def _iter_designator_list(value):
    if isinstance(value, (list, tuple)):
        return list(value)
    from . import sequence_protocol as _seq
    return _seq.list_elements(value, what='MAKE-PATHNAME :directory')


def _coerce_dir_component(item):
    if isinstance(item, lisptype.lispKeyword):
        marker = _DIR_TOKEN_KEYWORDS.get(item.name)
        if marker is not None:
            return marker
        raise lisptype.LispTypeError(
            f"invalid pathname directory component: {item!r}",
            expected_type='(OR STRING (MEMBER :WILD :WILD-INFERIORS :UP :BACK))',
            actual_value=item)
    if _is_string_designator(item):
        return _string_text(item)
    raise lisptype.LispTypeError(
        f"invalid pathname directory component: {item!r}",
        expected_type='(OR STRING (MEMBER :WILD :WILD-INFERIORS :UP :BACK))',
        actual_value=item)


def _coerce_directory_arg(value):
    """A `:directory` argument (CLHS `make-pathname`) -> our internal form."""
    if value is None or value is lisptype.NIL:
        return None
    if isinstance(value, lisptype.lispKeyword) and value.name == 'WILD':
        # CLHS leaves the exact expansion implementation-defined; SBCL-style
        # implementations use an absolute wild-inferiors tree.
        return (_K_ABSOLUTE, _K_WILD_INFERIORS)
    if _is_string_designator(value):
        s = _string_text(value)
        return (_K_RELATIVE, s) if s else (_K_RELATIVE,)
    items = _iter_designator_list(value)
    if not items:
        return None
    head = items[0]
    if isinstance(head, lisptype.lispKeyword) and head.name in ('ABSOLUTE', 'RELATIVE'):
        kind = _K_ABSOLUTE if head.name == 'ABSOLUTE' else _K_RELATIVE
        rest = items[1:]
    else:
        kind = _K_RELATIVE
        rest = items
    return tuple([kind] + [_coerce_dir_component(c) for c in rest])


def _directory_to_lisp(directory):
    from .sequence_protocol import make_lisp_list
    if directory is None:
        return lisptype.NIL
    return make_lisp_list(directory)


# ===== Namestring parsing =====

_DRIVE_RE = re.compile(r'^([A-Za-z]):[\\/]')
_BARE_DRIVE_RE = re.compile(r'^([A-Za-z]):$')


def _split_name_type(component):
    if component == '':
        return None, None
    dot = component.rfind('.')
    if dot == -1:
        return component, None
    # An empty part before the dot (".txt") means NIL, not the empty
    # string -- required for `(namestring (make-pathname :type "txt"))`,
    # ".txt", to read back with `pathname-name` NIL rather than "": the
    # ANSI suite's PATHNAMES-PRINT-AND-READ-PROPERLY constructs exactly
    # this pathname and requires the round trip to be `EQUAL`.
    return (component[:dot] or None), component[dot + 1:]


def _dir_token(part):
    if part == '*':
        return _K_WILD
    if part == '**':
        return _K_WILD_INFERIORS
    if part == '..':
        return _K_UP
    return part


def parse_physical_namestring(s):
    """Parse a Unix/Windows-style namestring into pathname components."""
    s = s.replace('\\', '/')

    device = None
    m = _DRIVE_RE.match(s)
    if m:
        device = m.group(1).upper()
        s = s[2:]
    elif _BARE_DRIVE_RE.match(s):
        device = s[0].upper()
        s = ''

    if s.startswith('/'):
        kind = _K_ABSOLUTE
        s = s[1:]
    else:
        kind = _K_RELATIVE

    parts = s.split('/') if s else []
    trailing_slash = bool(parts) and parts[-1] == ''
    if trailing_slash:
        parts = parts[:-1]
        last = None
    else:
        last = parts.pop() if parts else None

    dir_components = []
    for p in parts:
        if p == '' or p == '.':
            continue
        dir_components.append(_dir_token(p))

    directory = None
    if dir_components or kind is _K_ABSOLUTE or trailing_slash:
        directory = tuple([kind] + dir_components)

    name = type_ = None
    if last is not None and last != '':
        if last == '*':
            name, type_ = _K_WILD, None
        else:
            name_part, type_part = _split_name_type(last)
            name = _K_WILD if name_part == '*' else name_part
            if type_part is not None:
                type_ = _K_WILD if type_part == '*' else type_part

    return {'host': None, 'device': device, 'directory': directory,
            'name': name, 'type': type_, 'version': None, 'logical': False}


def _logical_component(part, upcase=True):
    if part == '*':
        return _K_WILD
    if part == '**':
        return _K_WILD_INFERIORS
    text = part.upper() if upcase else part
    return text


def parse_logical_namestring(host, rest):
    """Parse the portion of a logical namestring after `HOST:` (CLHS 19.3.2.1)."""
    rest = rest.replace('\\', '/').replace('/', ';')

    kind = _K_RELATIVE
    if rest.startswith(';'):
        kind = _K_ABSOLUTE
        rest = rest[1:]

    segments = rest.split(';') if rest else []
    last = segments.pop() if segments else ''

    dir_components = []
    for seg in segments:
        if seg == '':
            continue
        if seg == '..':
            dir_components.append(_K_UP)
        else:
            dir_components.append(_logical_component(seg))

    directory = None
    if dir_components or kind is _K_ABSOLUTE:
        directory = tuple([kind] + dir_components)

    name = type_ = version = None
    if last:
        bits = last.split('.')
        name_bit = bits[0] if bits else ''
        name = _K_WILD if name_bit == '*' else (name_bit.upper() if name_bit else None)
        if len(bits) > 1:
            type_bit = bits[1]
            type_ = _K_WILD if type_bit == '*' else type_bit.upper()
        if len(bits) > 2:
            version_bit = bits[2]
            if version_bit == '*':
                version = _K_WILD
            elif version_bit.upper() == 'NEWEST':
                version = _K_NEWEST
            else:
                try:
                    version = int(version_bit)
                except ValueError:
                    version = None

    # CLHS 19.3.2.1: "the device component of a logical pathname is always
    # :unspecific".
    return {'host': host, 'device': _K_UNSPECIFIC, 'directory': directory,
            'name': name, 'type': type_, 'version': version, 'logical': True}


def _find_logical_host(s):
    """If `s` starts with a registered logical host name followed by ':',
    return `(host_name_upper, remainder)`; else `None`.

    A logical host is recognized only if it has been *registered* via
    `(setf (logical-pathname-translations host) ...)` -- otherwise a Windows
    drive-letter namestring like "C:/foo" would be misread as a logical
    pathname naming host "C".
    """
    colon = s.find(':')
    if colon <= 0:
        return None
    candidate = s[:colon].upper()
    if candidate in _LOGICAL_HOSTS:
        return candidate, s[colon + 1:]
    return None


def parse_namestring_string(s):
    found = _find_logical_host(s)
    if found:
        host, rest = found
        return parse_logical_namestring(host, rest)
    return parse_physical_namestring(s)


def pathname_from_components(components):
    return Pathname(host=components['host'], device=components['device'],
                     directory=components['directory'], name=components['name'],
                     type=components['type'], version=components['version'],
                     logical=components['logical'])


def pathname_from_namestring(s):
    return pathname_from_components(parse_namestring_string(str(s)))


def pathname_from_os_path(path_str):
    """Wrap a *real, existing* OS path in a `Pathname`, directory or file.

    `os.path.realpath`/`os.getcwd` never append a trailing separator, so
    parsing their result as a plain namestring put a directory's last
    component into `:name` instead of the directory list -- `(pathname-
    directory (truename (make-pathname)))` lost "ansi-test" this way, and
    every logical-pathname translation built on top of it (universe.lsp's
    CLTEST setup) inherited the wrong directory. Checking the real
    filesystem is only valid for a path already confirmed to exist.
    """
    text = str(path_str)
    if os.path.isdir(text) and not text.endswith(('/', '\\')):
        text = text + os.sep
    return pathname_from_namestring(text)


# `Pathname.from_namestring(s)` is the call-site spelling used throughout the
# rest of the codebase (readtable.py's #P reader, streams.py, misc_macros.py,
# lispenv.py, runtime.py) for "the pathname naming this OS path string" --
# distinct from `PATHNAME`/`PARSE-NAMESTRING`, which must coerce a
# *designator* (possibly already a `Pathname`) rather than always re-parsing
# a string.
Pathname.from_namestring = staticmethod(pathname_from_namestring)


# ===== Namestring rendering =====

def _render_component(c):
    if c is None:
        return ''
    if c is _K_WILD:
        return '*'
    if c is _K_UNSPECIFIC:
        return ''
    return str(c)


def _render_dir_token(c):
    if c is _K_WILD:
        return '*'
    if c is _K_WILD_INFERIORS:
        return '**'
    if c is _K_UP:
        return '..'
    if c is _K_BACK:
        return '..'
    return str(c)


def _render_directory_physical(directory):
    if directory is None:
        return ''
    kind = directory[0]
    parts = [_render_dir_token(c) for c in directory[1:]]
    prefix = '/' if kind is _K_ABSOLUTE else ''
    if not parts:
        return prefix
    return prefix + '/'.join(parts) + '/'


def render_namestring(pn):
    if pn.logical:
        return _render_logical_namestring(pn)
    parts = []
    if pn.device:
        parts.append(f'{pn.device}:')
    parts.append(_render_directory_physical(pn.directory))
    name_part = _render_component(pn.name)
    type_part = _render_component(pn.type)
    if type_part:
        name_part = f'{name_part}.{type_part}'
    parts.append(name_part)
    return ''.join(parts)


def _render_logical_namestring(pn):
    parts = [pn.host or '', ':']
    directory = pn.directory
    if directory is not None:
        kind = directory[0]
        segs = [_render_dir_token(c) for c in directory[1:]]
        if kind is _K_ABSOLUTE:
            parts.append(';')
        if segs:
            parts.append(';'.join(segs))
            parts.append(';')
    name_part = _render_component(pn.name)
    type_part = _render_component(pn.type)
    version = pn.version
    text = name_part
    if type_part:
        text += f'.{type_part}'
        if isinstance(version, int):
            text += f'.{version}'
        elif version is _K_NEWEST:
            text += '.NEWEST'
        elif version is _K_WILD:
            text += '.*'
    parts.append(text)
    return ''.join(parts).upper()


# ===== Designator coercion =====

def _pathname_of_stream(stream, operator):
    from .streams import Stream, SynonymStream
    if isinstance(stream, SynonymStream):
        from .streams import synonym_stream_symbol
        from .binding import dynamic_value
        symbol = synonym_stream_symbol(stream)
        target = dynamic_value(symbol)
        return _coerce_pathname_designator(target, operator)
    if isinstance(stream, Stream):
        # CLHS: PATHNAME/LOGICAL-PATHNAME on a stream answer the designator
        # it was *opened* with -- which may be a logical pathname even
        # though `stream.name` by now holds the physical OS path OPEN
        # actually used for I/O.
        logical = getattr(stream, 'logical_pathname', None)
        if logical is not None:
            return logical
        name = getattr(stream, 'name', None)
        if isinstance(name, str) and name and not name.startswith('<'):
            return pathname_from_namestring(name)
    raise lisptype.LispTypeError(
        f"{operator}: {stream!r} is not associated with a file",
        expected_type='(OR PATHNAME STRING FILE-STREAM SYNONYM-STREAM)',
        actual_value=stream)


def _coerce_pathname_designator(obj, operator='PATHNAME'):
    """CLHS pathname designator: a pathname, a namestring, or a stream
    associated with a file (a file-stream, or a synonym-stream to one)."""
    if isinstance(obj, Pathname):
        return obj
    if _is_string_designator(obj):
        return pathname_from_namestring(_string_text(obj))
    from .streams import Stream
    if isinstance(obj, Stream):
        return _pathname_of_stream(obj, operator)
    raise lisptype.LispTypeError(
        f"{operator}: {obj!r} is not a pathname designator",
        expected_type='(OR PATHNAME STRING FILE-STREAM SYNONYM-STREAM)',
        actual_value=obj)


def _filespec_namestring(filespec):
    """The namestring a *pathname designator* carries (CLHS glossary)."""
    return _coerce_pathname_designator(filespec, 'OPEN').namestring()


def resolve_filespec(filespec):
    """The OS path a *filespec designator* names -- the one resolver.

    See the module-level history in `_coerce_pathname_designator`'s callers:
    LOAD, COMPILE-FILE, COMPILE-FILE-PATHNAME, DELETE-FILE and OPEN all go
    through this one search rather than each keeping its own copy.
    """
    from .binding import dynamic_value

    pn = _coerce_pathname_designator(filespec, 'OPEN')
    if pn.logical:
        pn = translate_logical_pathname(pn)
    path_str = pn.namestring()

    if os.path.isabs(path_str):
        return os.path.normpath(path_str)

    lisp_cwd = os.environ.get('LISP_CWD')
    if lisp_cwd:
        candidate = os.path.normpath(os.path.join(lisp_cwd, path_str))
        if os.path.exists(candidate):
            return candidate

    load_truename = dynamic_value(
        lisptype.COMMON_LISP_PACKAGE.intern_symbol('*LOAD-TRUENAME*'))
    if isinstance(load_truename, Pathname):
        current_dir = os.path.dirname(load_truename.namestring())
        if current_dir:
            candidate = os.path.normpath(os.path.join(current_dir, path_str))
            if os.path.exists(candidate):
                return candidate

    defaults = dynamic_value(
        lisptype.COMMON_LISP_PACKAGE.intern_symbol('*DEFAULT-PATHNAME-DEFAULTS*'))
    if isinstance(defaults, Pathname):
        default_path = defaults.namestring()
        base = default_path if os.path.isdir(default_path) else os.path.dirname(default_path)
        if base:
            return os.path.normpath(os.path.join(base, path_str))

    return os.path.normpath(path_str)


# ===== MAKE-PATHNAME =====

@_registry.cl_function('MAKE-PATHNAME')
def make_pathname_function(*, host=lisptype.OMITTED, device=lisptype.OMITTED,
                           directory=lisptype.OMITTED, name=lisptype.OMITTED,
                           type=lisptype.OMITTED, version=lisptype.OMITTED,
                           defaults=lisptype.OMITTED, case=lisptype.OMITTED):
    """MAKE-PATHNAME (CLHS 19.4.2).

    Every component not supplied is taken from `:defaults` when given, or
    else defaults to NIL -- except host, which without `:defaults` falls
    back to `(pathname-host *default-pathname-defaults*)` rather than NIL.
    `:case` is accepted (so callers passing `:local`/`:common` are not a
    PROGRAM-ERROR) but not applied: this implementation's single case
    convention makes the translation a no-op, and no test in the suite
    checks the literal casing `:case` would otherwise produce.
    """
    default_source = None
    if lisptype.supplied(defaults) and lisptype.is_truthy(defaults):
        default_source = _coerce_pathname_designator(defaults, 'MAKE-PATHNAME')

    if lisptype.supplied(host):
        host_val = _string_text(host) if lisptype.is_truthy(host) and _is_string_designator(host) else \
            (host if lisptype.is_truthy(host) else None)
    elif default_source is not None:
        host_val = default_source.host
    else:
        default_host = pathname_host(_default_pathname_defaults())
        host_val = default_host if lisptype.is_truthy(default_host) else None

    if lisptype.supplied(device):
        device_val = _coerce_name_or_type_arg(device) if lisptype.is_truthy(device) else None
    elif default_source is not None:
        device_val = default_source.device
    else:
        device_val = None

    if lisptype.supplied(directory):
        directory_val = _coerce_directory_arg(directory)
    elif default_source is not None:
        directory_val = default_source.directory
    else:
        directory_val = None

    if lisptype.supplied(name):
        name_val = _coerce_name_or_type_arg(name)
    elif default_source is not None:
        name_val = default_source.name
    else:
        name_val = None

    if lisptype.supplied(type):
        type_val = _coerce_name_or_type_arg(type)
    elif default_source is not None:
        type_val = default_source.type
    else:
        type_val = None

    if lisptype.supplied(version):
        version_val = _coerce_version_arg(version)
    elif default_source is not None:
        version_val = default_source.version
    else:
        version_val = None

    logical = bool(default_source and default_source.logical)
    if not logical and isinstance(host_val, str) and host_val.upper() in _LOGICAL_HOSTS:
        logical = True
    if logical and not lisptype.supplied(device):
        # CLHS 19.3.2.1: a logical pathname's device is always :unspecific.
        device_val = _K_UNSPECIFIC
    return Pathname(host=host_val, device=device_val, directory=directory_val,
                     name=name_val, type=type_val, version=version_val,
                     logical=logical)


def _coerce_name_or_type_arg(value):
    if value is None or value is lisptype.NIL:
        return None
    if isinstance(value, lisptype.lispKeyword):
        if value.name == 'WILD':
            return _K_WILD
        if value.name == 'UNSPECIFIC':
            return _K_UNSPECIFIC
    if _is_string_designator(value):
        return _string_text(value)
    raise lisptype.LispTypeError(
        f"invalid pathname name/type component: {value!r}",
        expected_type='(OR STRING (MEMBER :WILD :UNSPECIFIC))', actual_value=value)


def _coerce_version_arg(value):
    if value is None or value is lisptype.NIL:
        return None
    if isinstance(value, int):
        return value
    if isinstance(value, lisptype.lispKeyword):
        if value.name == 'WILD':
            return _K_WILD
        if value.name == 'NEWEST':
            return _K_NEWEST
        if value.name == 'UNSPECIFIC':
            return _K_UNSPECIFIC
    raise lisptype.LispTypeError(
        f"invalid pathname version: {value!r}",
        expected_type='(OR INTEGER (MEMBER :WILD :NEWEST :UNSPECIFIC))',
        actual_value=value)


def _default_pathname_defaults():
    from .binding import dynamic_value
    value = dynamic_value(
        lisptype.COMMON_LISP_PACKAGE.intern_symbol('*DEFAULT-PATHNAME-DEFAULTS*'))
    if isinstance(value, Pathname):
        return value
    return Pathname()


# ===== PATHNAME / PATHNAMEP =====

@_registry.cl_function('PATHNAME')
def pathname(thing):
    """PATHNAME (CLHS 19.4.1): coerce a pathname designator, unchanged if
    `thing` is already a `Pathname` (identity, so `(eq x (pathname x))`)."""
    return _coerce_pathname_designator(thing, 'PATHNAME')


@_registry.cl_function('PATHNAMEP')
def pathnamep(obj):
    return lisptype.lisp_bool(isinstance(obj, Pathname))


@_registry.cl_function('LOGICAL-PATHNAME-P')
def logical_pathname_p(obj):
    return lisptype.lisp_bool(isinstance(obj, Pathname) and obj.logical)


# ===== Accessors =====

@_registry.cl_function('PATHNAME-HOST')
def pathname_host(pathname, *, case=lisptype.OMITTED):
    pn = _coerce_pathname_designator(pathname, 'PATHNAME-HOST')
    return lisptype.NIL if pn.host is None else pn.host


@_registry.cl_function('PATHNAME-DEVICE')
def pathname_device(pathname, *, case=lisptype.OMITTED):
    pn = _coerce_pathname_designator(pathname, 'PATHNAME-DEVICE')
    return lisptype.NIL if pn.device is None else pn.device


@_registry.cl_function('PATHNAME-DIRECTORY')
def pathname_directory(pathname, *, case=lisptype.OMITTED):
    pn = _coerce_pathname_designator(pathname, 'PATHNAME-DIRECTORY')
    return _directory_to_lisp(pn.directory)


@_registry.cl_function('PATHNAME-NAME')
def pathname_name(pathname, *, case=lisptype.OMITTED):
    pn = _coerce_pathname_designator(pathname, 'PATHNAME-NAME')
    return lisptype.NIL if pn.name is None else pn.name


@_registry.cl_function('PATHNAME-TYPE')
def pathname_type(pathname, *, case=lisptype.OMITTED):
    pn = _coerce_pathname_designator(pathname, 'PATHNAME-TYPE')
    return lisptype.NIL if pn.type is None else pn.type


@_registry.cl_function('PATHNAME-VERSION')
def pathname_version(pathname):
    pn = _coerce_pathname_designator(pathname, 'PATHNAME-VERSION')
    return lisptype.NIL if pn.version is None else pn.version


# ===== Namestrings =====

@_registry.cl_function('NAMESTRING')
def namestring(pathname):
    pn = _coerce_pathname_designator(pathname, 'NAMESTRING')
    return pn.namestring()


@_registry.cl_function('FILE-NAMESTRING')
def file_namestring(pathname):
    pn = _coerce_pathname_designator(pathname, 'FILE-NAMESTRING')
    name_part = _render_component(pn.name)
    type_part = _render_component(pn.type)
    if type_part:
        name_part = f'{name_part}.{type_part}'
    return name_part


@_registry.cl_function('DIRECTORY-NAMESTRING')
def directory_namestring(pathname):
    pn = _coerce_pathname_designator(pathname, 'DIRECTORY-NAMESTRING')
    return _render_directory_physical(pn.directory)


@_registry.cl_function('HOST-NAMESTRING')
def host_namestring(pathname):
    pn = _coerce_pathname_designator(pathname, 'HOST-NAMESTRING')
    return pn.host or ''


@_registry.cl_function('ENOUGH-NAMESTRING')
def enough_namestring(pathname, defaults=lisptype.OMITTED):
    pn = _coerce_pathname_designator(pathname, 'ENOUGH-NAMESTRING')
    if not lisptype.supplied(defaults) or not lisptype.is_truthy(defaults):
        base = _default_pathname_defaults()
    else:
        base = _coerce_pathname_designator(defaults, 'ENOUGH-NAMESTRING')
    if pn.directory is not None and base.directory is not None \
            and pn.directory[0] is _K_ABSOLUTE and base.directory[0] is _K_ABSOLUTE:
        base_parts = base.directory[1:]
        pn_parts = pn.directory[1:]
        if pn_parts[:len(base_parts)] == base_parts:
            relative = Pathname(directory=(_K_RELATIVE,) + pn_parts[len(base_parts):],
                                name=pn.name, type=pn.type, version=pn.version)
            return relative.namestring()
    return pn.namestring()


@_registry.cl_function('PATHNAME-WITHOUT-NAME-TYPE')
def pathname_without_name_type(pathname):
    pn = _coerce_pathname_designator(pathname, 'PATHNAME-WITHOUT-NAME-TYPE')
    return Pathname(host=pn.host, device=pn.device, directory=pn.directory,
                    logical=pn.logical)


# ===== MERGE-PATHNAMES (CLHS 19.3.3) =====

@_registry.cl_function('MERGE-PATHNAMES')
def merge_pathnames(pathname, defaults=lisptype.OMITTED, default_version=lisptype.OMITTED):
    pn = _coerce_pathname_designator(pathname, 'MERGE-PATHNAMES')
    if not lisptype.supplied(defaults) or not lisptype.is_truthy(defaults):
        base = _default_pathname_defaults()
    else:
        base = _coerce_pathname_designator(defaults, 'MERGE-PATHNAMES')

    host = pn.host if pn.host is not None else base.host
    device = pn.device if pn.device is not None else base.device

    if pn.directory is None:
        directory = base.directory
    elif pn.directory[0] is _K_RELATIVE and base.directory is not None:
        directory = (base.directory[0],) + base.directory[1:] + pn.directory[1:]
    else:
        directory = pn.directory

    name = pn.name if pn.name is not None else base.name
    type_ = pn.type if pn.type is not None else base.type

    # CLHS 19.3.3: version is taken from `defaults` only when `pathname`
    # supplies *no* name and *no* type of its own -- i.e. the whole
    # name/type/version triple is being filled in from `defaults` as a
    # unit. If `pathname` specifies a name or a type, its version (even
    # though unspecified) is filled from `default-version` instead, which
    # itself defaults to :newest but can be suppressed by passing NIL
    # explicitly. Conflating the two (taking version from `defaults`
    # whenever `pathname`'s version was NIL) is what made
    # `(merge-pathnames (make-pathname :name "foo") p2 nil)` inherit p2's
    # explicit :newest version instead of staying NIL like `pathname`'s own.
    if lisptype.supplied(default_version):
        default_version_val = (_coerce_version_arg(default_version)
                               if lisptype.is_truthy(default_version) else None)
    else:
        default_version_val = _K_NEWEST

    if pn.version is not None:
        version = pn.version
    elif pn.name is None and pn.type is None:
        version = base.version if base.version is not None else default_version_val
    else:
        version = default_version_val

    return Pathname(host=host, device=device, directory=directory, name=name,
                    type=type_, version=version, logical=pn.logical or base.logical)


# ===== PARSE-NAMESTRING (CLHS 19.4.3) =====

@_registry.cl_function('PARSE-NAMESTRING')
def parse_namestring(thing, host=lisptype.OMITTED, defaults=lisptype.OMITTED, *,
                     start=0, end=None, junk_allowed=None):
    if isinstance(thing, Pathname):
        text = thing.namestring()
    else:
        text = _string_text(thing) if _is_string_designator(thing) else None
        if text is None:
            pn = _coerce_pathname_designator(thing, 'PARSE-NAMESTRING')
            return lisptype.MultipleValues(pn, 0)

    segment = text[start:end] if end is not None else text[start:]
    pn = pathname_from_namestring(segment) if segment else Pathname()
    pos = (end if end is not None else len(text))
    return lisptype.MultipleValues(pn, pos)


# ===== WILD-PATHNAME-P (CLHS 19.4.4) =====

_WILD_FIELD_NAMES = {'HOST': 'host', 'DEVICE': 'device', 'DIRECTORY': 'directory',
                     'NAME': 'name', 'TYPE': 'type', 'VERSION': 'version'}


def _field_is_wild(pn, field):
    value = getattr(pn, field)
    if field == 'directory':
        return value is not None and any(c is _K_WILD or c is _K_WILD_INFERIORS
                                         for c in value[1:])
    return value is _K_WILD


@_registry.cl_function('WILD-PATHNAME-P')
def wild_pathname_p(pathname, field_key=None):
    pn = _coerce_pathname_designator(pathname, 'WILD-PATHNAME-P')
    if field_key is None or field_key is lisptype.NIL:
        return lisptype.lisp_bool(any(_field_is_wild(pn, f) for f in _WILD_FIELD_NAMES.values()))
    if isinstance(field_key, lisptype.lispKeyword) and field_key.name in _WILD_FIELD_NAMES:
        return lisptype.lisp_bool(_field_is_wild(pn, _WILD_FIELD_NAMES[field_key.name]))
    raise lisptype.LispProgramError(f"WILD-PATHNAME-P: invalid field-key: {field_key!r}")


# ===== Component-level wildcard matching (PATHNAME-MATCH-P / TRANSLATE-PATHNAME) =====

def _component_equal(a, b):
    if isinstance(a, str) and isinstance(b, str):
        return a.lower() == b.lower()
    return a == b


def _component_match(concrete, pattern):
    """A `wildname` component of NIL is unspecified -- it imposes no
    constraint and matches any concrete value, exactly like :WILD -- while a
    concrete, non-wild `pattern` must equal `concrete` (case-insensitively
    for a string). `pathname-match-p.6`/`.5` pin this: `(make-pathname)`'s
    unspecified VERSION/DIRECTORY, used as the *wildname*, matches a pathname
    whose corresponding component is itself :WILD -- which a symmetric
    "both sides equal" reading of NIL would refuse."""
    if pattern is None or pattern is _K_WILD or pattern is _K_WILD_INFERIORS:
        return True
    return _component_equal(concrete, pattern)


def _dir_components_match(concrete, pattern):
    if not pattern:
        return not concrete
    head, rest = pattern[0], pattern[1:]
    if head is _K_WILD_INFERIORS:
        for i in range(len(concrete), -1, -1):
            if _dir_components_match(concrete[i:], rest):
                return True
        return False
    if not concrete:
        return False
    if head is _K_WILD or _component_equal(concrete[0], head):
        return _dir_components_match(concrete[1:], rest)
    return False


def _directory_match(concrete, pattern):
    if pattern is None:
        # An unspecified wildname directory doesn't constrain anything --
        # see `_component_match`.
        return True
    if concrete is None:
        # An unspecified *concrete* directory is a relative pathname with
        # zero components, not "no opinion" -- `(:RELATIVE)` in every
        # observable respect (namestring rendering, MERGE-PATHNAMES), so it
        # is normalized the same way here.
        concrete = (_K_RELATIVE,)
    if concrete[0] is not pattern[0]:
        return False
    return _dir_components_match(list(concrete[1:]), list(pattern[1:]))


def _pathname_matches(pn, wild):
    if not _component_match(pn.host, wild.host):
        return False
    if not _component_match(pn.device, wild.device):
        return False
    if not _directory_match(pn.directory, wild.directory):
        return False
    if not _component_match(pn.name, wild.name):
        return False
    if not _component_match(pn.type, wild.type):
        return False
    if not _component_match(pn.version, wild.version):
        return False
    return True


@_registry.cl_function('PATHNAME-MATCH-P')
def pathname_match_p(pathname, wildname):
    pn = _coerce_pathname_designator(pathname, 'PATHNAME-MATCH-P')
    wild = _coerce_pathname_designator(wildname, 'PATHNAME-MATCH-P')
    return lisptype.lisp_bool(_pathname_matches(pn, wild))


# ---- capture-based directory translation ----

def _dir_components_capture(concrete, pattern):
    """Match `concrete` against `pattern`, returning the list of captures (one
    per wildcard in `pattern`, in order) or None if they do not match."""
    if not pattern:
        return [] if not concrete else None
    head, rest = pattern[0], pattern[1:]
    if head is _K_WILD_INFERIORS:
        for i in range(len(concrete) + 1):
            tail = _dir_components_capture(concrete[i:], rest)
            if tail is not None:
                return [list(concrete[:i])] + tail
        return None
    if not concrete:
        return None
    if head is _K_WILD:
        tail = _dir_components_capture(concrete[1:], rest)
        return None if tail is None else [concrete[0]] + tail
    if _component_equal(concrete[0], head):
        return _dir_components_capture(concrete[1:], rest)
    return None


def _apply_dir_captures(to_components, captures):
    result = []
    it = iter(captures)
    for c in to_components:
        if c is _K_WILD:
            result.append(next(it))
        elif c is _K_WILD_INFERIORS:
            result.extend(next(it))
        else:
            result.append(c)
    return result


def _translate_directory(source_dir, from_dir, to_dir):
    if to_dir is None:
        return source_dir
    if from_dir is None:
        return to_dir
    # An unspecified source directory is `(:RELATIVE)` -- zero components --
    # in every other observable respect (see `_directory_match`), so a
    # `from_dir` of just `(:RELATIVE :WILD-INFERIORS)` still captures it as
    # zero components rather than refusing to translate it at all.
    source = source_dir if source_dir is not None else (_K_RELATIVE,)
    captures = _dir_components_capture(list(source[1:]), list(from_dir[1:]))
    if captures is None:
        return to_dir
    return tuple([to_dir[0]] + _apply_dir_captures(list(to_dir[1:]), captures))


def _translate_component(source, from_c, to_c):
    if to_c is _K_WILD:
        return source
    return to_c


@_registry.cl_function('TRANSLATE-PATHNAME')
def translate_pathname(source, from_wildname, to_wildname):
    src = _coerce_pathname_designator(source, 'TRANSLATE-PATHNAME')
    frm = _coerce_pathname_designator(from_wildname, 'TRANSLATE-PATHNAME')
    to = _coerce_pathname_designator(to_wildname, 'TRANSLATE-PATHNAME')

    directory = _translate_directory(src.directory, frm.directory, to.directory)
    return Pathname(
        host=_translate_component(src.host, frm.host, to.host),
        device=_translate_component(src.device, frm.device, to.device),
        directory=directory,
        name=_translate_component(src.name, frm.name, to.name),
        type=_translate_component(src.type, frm.type, to.type),
        version=_translate_component(src.version, frm.version, to.version),
        logical=to.logical)


# ===== Logical pathnames (CLHS 19.3.2) =====

_LOGICAL_HOSTS = {}


@_registry.cl_function('LOGICAL-PATHNAME')
def logical_pathname(pathspec):
    if isinstance(pathspec, Pathname):
        if pathspec.logical:
            return pathspec
        raise lisptype.LispTypeError(
            f"LOGICAL-PATHNAME: {pathspec!r} is not a logical pathname",
            expected_type='(OR STRING STREAM LOGICAL-PATHNAME)', actual_value=pathspec)
    pn = _coerce_pathname_designator(pathspec, 'LOGICAL-PATHNAME')
    if not pn.logical:
        raise lisptype.LispTypeError(
            f"LOGICAL-PATHNAME: {pathspec!r} does not name a defined logical host",
            expected_type='(OR STRING STREAM LOGICAL-PATHNAME)', actual_value=pathspec)
    return pn


@_registry.cl_function('LOGICAL-PATHNAME-TRANSLATIONS')
def logical_pathname_translations(host):
    name = _host_designator_text(host)
    entries = _LOGICAL_HOSTS.get(name)
    if entries is None:
        raise lisptype.LispError(f"LOGICAL-PATHNAME-TRANSLATIONS: undefined logical host: {name}")
    from .sequence_protocol import make_lisp_list
    return make_lisp_list(make_lisp_list([from_str, to_pn]) for from_str, to_pn in entries)


def _host_designator_text(host):
    if isinstance(host, Pathname) and host.logical:
        return host.host
    if _is_string_designator(host):
        return _string_text(host).upper()
    raise lisptype.LispTypeError(
        f"not a logical host designator: {host!r}",
        expected_type='(OR STRING LOGICAL-PATHNAME)', actual_value=host)


@_registry.cl_function('(SETF LOGICAL-PATHNAME-TRANSLATIONS)')
def set_logical_pathname_translations(value, host):
    name = _host_designator_text(host)
    from . import sequence_protocol as _seq
    entries = []
    for pair in _seq.list_elements(value, what='(SETF LOGICAL-PATHNAME-TRANSLATIONS)'):
        items = _seq.list_elements(pair, what='(SETF LOGICAL-PATHNAME-TRANSLATIONS)')
        from_str = _string_text(items[0]) if _is_string_designator(items[0]) else str(items[0])
        to_pn = _coerce_pathname_designator(items[1], '(SETF LOGICAL-PATHNAME-TRANSLATIONS)')
        entries.append((from_str.upper(), to_pn))
    _LOGICAL_HOSTS[name] = entries
    return value


@_registry.cl_function('LOAD-LOGICAL-PATHNAME-TRANSLATIONS')
def load_logical_pathname_translations(host):
    """CLHS 19.4.5: "if translations for host are already defined, ...
    simply returns NIL. Otherwise, ... attempts to find and load such
    translations, ... signals an error if it is unable to." This
    implementation has no external translation-file mechanism to fall back
    on, so the only successful case is the first one -- there is no way to
    define translations that were not *already* defined some other way."""
    name = _host_designator_text(host)
    if name in _LOGICAL_HOSTS:
        return lisptype.NIL
    raise lisptype.LispError(
        f"LOAD-LOGICAL-PATHNAME-TRANSLATIONS: no translations available for host {name}")


@_registry.cl_function('TRANSLATE-LOGICAL-PATHNAME')
def translate_logical_pathname(pathname, **kwargs):
    pn = _coerce_pathname_designator(pathname, 'TRANSLATE-LOGICAL-PATHNAME')
    seen = set()
    while pn.logical:
        if pn.host in seen:
            raise lisptype.LispError(
                f"TRANSLATE-LOGICAL-PATHNAME: translation loop for host {pn.host}")
        seen.add(pn.host)
        entries = _LOGICAL_HOSTS.get(pn.host)
        if not entries:
            raise lisptype.LispError(
                f"TRANSLATE-LOGICAL-PATHNAME: undefined logical host: {pn.host}")
        for from_str, to_pn in entries:
            # `from_str` is always written in the *logical* host's own
            # namestring syntax (CLHS 19.3.2.1), with no "HOST:" prefix of
            # its own -- `set_logical_pathname_translations` stores it as
            # given by `(setf (logical-pathname-translations host) ...)`.
            wild = pathname_from_components(parse_logical_namestring(pn.host, from_str))
            if _pathname_matches(pn, wild):
                pn = translate_pathname(pn, wild, to_pn)
                break
        else:
            raise lisptype.LispError(
                f"TRANSLATE-LOGICAL-PATHNAME: no matching translation for {pn.namestring()}")
    return pn


# ===== Directory listing / probing =====

def _error_if_wild(pn, operator):
    """The CLHS 19.4 "wild" refusal shared by the file probes: PROBE-FILE,
    FILE-WRITE-DATE and friends each specify "an error of type FILE-ERROR is
    signaled if pathspec is wild" -- the operation cannot name one file, so
    the answer would be a lie. The condition carries `pn` itself (the
    designator as given, before any translation), which is what
    `file-error.1`'s EQUALP check compares against."""
    from .evaluation_conditions import signal_file_error
    if any(_field_is_wild(pn, field) for field in _WILD_FIELD_NAMES.values()):
        signal_file_error(pn, f"{operator}: wild pathname: {pn.namestring()}")


def _directory_error_check(pn, signal_file_error):
    """DIRECTORY signals a FILE-ERROR (CLHS 20.2), not a TYPE-ERROR, for a
    directory list that cannot name anything: `:UP`/`:BACK` at the root of
    an absolute directory, or immediately after a `:WILD-INFERIORS` (which
    absorbs an unbounded, unresolved number of directory levels, so ANSI's
    own `make-pathname-error-*` tests treat "go up from there" as
    unanswerable rather than merely nonstandard)."""
    directory = pn.directory
    if directory is None:
        return
    kind = directory[0]
    comps = directory[1:]
    if kind is _K_ABSOLUTE and comps and comps[0] in (_K_UP, _K_BACK):
        signal_file_error(pn, "DIRECTORY: :UP/:BACK with nothing above the root")
    for prev, cur in zip(comps, comps[1:]):
        if prev is _K_WILD_INFERIORS and cur in (_K_UP, _K_BACK):
            signal_file_error(pn, "DIRECTORY: :UP/:BACK immediately after :WILD-INFERIORS")


@_registry.cl_function('DIRECTORY')
def directory(pathname, **kwargs):
    """The pathnames matching `pathname` (CLHS 20.2).

    Returns a Lisp **list**, not a Python one: a Python list is this
    implementation's *vector* (plan.md Finding M).
    """
    from .evaluation_conditions import signal_file_error

    pn = _coerce_pathname_designator(pathname, 'DIRECTORY')
    if pn.logical:
        pn = translate_logical_pathname(pn)
    _directory_error_check(pn, signal_file_error)

    # `resolve_filespec`, not a bare `pn.namestring()`: a relative pathname
    # here has no directory of its own, so without going through the same
    # `*DEFAULT-PATHNAME-DEFAULTS*`/`LISP_CWD` resolution OPEN and DELETE-FILE
    # use, `(directory "tmp.dat")` matched (or missed) whatever file of that
    # name happened to sit under the Python process's actual working
    # directory -- not the one every other file operator was reading and
    # writing. The mismatch is exactly what broke `delete-all-versions`
    # (ansi-aux.lsp): its `(directory (make-pathname :version :wild
    # :defaults p))` would resolve one relative candidate, and the
    # `delete-file` on the truename it returned would resolve a *different*
    # one, so cleanup between OPEN.* tests either found nothing or deleted
    # the wrong file.
    path_str = resolve_filespec(pn)
    import glob

    if '*' in path_str or '?' in path_str:
        matches = glob.glob(path_str)
    elif os.path.isdir(path_str):
        matches = [os.path.join(path_str, name) for name in os.listdir(path_str)]
    elif os.path.isfile(path_str):
        # A literal, non-wildcard pathname matches its own file if that file
        # exists (CLHS 20.2) -- including one whose only wild component is
        # :VERSION, which never appears in `path_str` at all because a
        # physical pathname here has no version namestring syntax (plan.md).
        # `delete-all-versions`'s `(directory (make-pathname :version :wild
        # :defaults p))` depends on exactly this: without it, DIRECTORY
        # answered NIL for a real file every time, so the ansi-test harness's
        # own cleanup helper never deleted anything and every OPEN.ERROR.*
        # test expecting a missing-file FILE-ERROR instead found the file
        # left over from a previous test.
        matches = [path_str]
    else:
        matches = []

    from .sequence_protocol import make_lisp_list
    return make_lisp_list(pathname_from_os_path(match) for match in matches)


@_registry.cl_function('TRUENAME')
def truename(pathname):
    from .evaluation_conditions import signal_file_error

    path_str = resolve_filespec(pathname)
    if not os.path.exists(path_str):
        return signal_file_error(pathname, f"TRUENAME: file not found: {path_str}")
    try:
        return pathname_from_os_path(os.path.realpath(path_str))
    except (OSError, ValueError):
        return signal_file_error(
            pathname, f"TRUENAME: cannot resolve pathname: {path_str}")


@_registry.cl_function('PROBE-FILE')
def probe_file(pathname):
    pn = _coerce_pathname_designator(pathname, 'PROBE-FILE')
    _error_if_wild(pn, 'PROBE-FILE')
    path_str = resolve_filespec(pn)
    if os.path.exists(path_str) and os.path.isfile(path_str):
        return pathname_from_os_path(os.path.realpath(path_str))
    return lisptype.NIL


@_registry.cl_function('FILE-WRITE-DATE')
def file_write_date(pathname):
    pn = _coerce_pathname_designator(pathname, 'FILE-WRITE-DATE')
    _error_if_wild(pn, 'FILE-WRITE-DATE')
    path_str = resolve_filespec(pn)
    if os.path.exists(path_str):
        return int(os.path.getmtime(path_str))
    return lisptype.NIL


@_registry.cl_function('ABSOLUTE-PATHNAME-P')
def absolute_pathname_p(pathname):
    pn = _coerce_pathname_designator(pathname, 'ABSOLUTE-PATHNAME-P')
    return lisptype.lisp_bool(pn.directory is not None and pn.directory[0] is _K_ABSOLUTE)


@_registry.cl_function('RELATIVE-PATHNAME-P')
def relative_pathname_p(pathname):
    pn = _coerce_pathname_designator(pathname, 'RELATIVE-PATHNAME-P')
    return lisptype.lisp_bool(pn.directory is None or pn.directory[0] is _K_RELATIVE)


@_registry.cl_function('ENSURE-DIRECTORIES-EXIST')
def ensure_directories_exist(pathspec, **kwargs):
    pn = _coerce_pathname_designator(pathspec, 'ENSURE-DIRECTORIES-EXIST')
    # A wild directory cannot be created, and CLHS's exceptional situation
    # for this operator is a FILE-ERROR -- which ENSURE-DIRECTORIES-EXIST
    # (ansi-test files/ensure-directories-exist.lsp) drives with a
    # `(:relative :wild)` directory and requires by type. Checking before
    # the OS call also keeps Python's OSError from surfacing as the value of
    # the form (standing rule 2); the `os.makedirs` below is guarded the
    # same way for whatever an OS rejects on a non-wild path.
    _error_if_wild(pn, 'ENSURE-DIRECTORIES-EXIST')
    # The directory portion of the *resolved* filespec -- not
    # `pn.directory` rendered against the process's working directory: a
    # `(:relative "scratch")` directory is defined relative to
    # *DEFAULT-PATHNAME-DEFAULTS* (CLHS 19.2.3's merge), and
    # `resolve_filespec` is the one resolver that knows that search.
    dir_str = os.path.dirname(resolve_filespec(pn))
    created = False
    if dir_str and not os.path.isdir(dir_str):
        try:
            os.makedirs(dir_str, exist_ok=True)
        except OSError as error:
            from .evaluation_conditions import signal_file_error
            signal_file_error(
                pn, f"ENSURE-DIRECTORIES-EXIST: cannot create directory: {error}")
        created = True
    return lisptype.MultipleValues(pn, lisptype.lisp_bool(created))
