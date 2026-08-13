"""I/O write operations - stream output, printing, pathnames, and file operations."""

import fclpy.lisptype as lisptype
from . import registry as _registry
from .streams import open_file as open_fn, close_stream as close_fn


# === The printer, and where output goes ===
#
# The Lisp printer lives in `fclpy/printer.py` -- one implementation, reading
# the printer control variables from the live dynamic environment.
#
# What used to be here was a `PrinterSettings` object holding those variables as
# Python globals, a set of `@cl_function('*PRINT-...*')` accessors onto it, and
# a `_print_with_limits` printer that honoured them. None of it was reachable.
# No binding form can assign a Python global, so `(let ((*print-base* 2)) ...)`
# could not affect the settings object; nothing called `_print_with_limits`, so
# every CL entry point below printed via `lisptype.lisp_str`/`lisp_repr`, which
# are `str()`/`repr()`; and registering a *function* named `*PRINT-BASE*` is
# what made a reference to `*print-base*` evaluate to a Python function object,
# since the function registry is where the evaluator looks after failing to
# find a variable. The variables are now bound with their ANSI initial values
# in `lispenv.setup_standard_environment`, from `printer.PRINTER_VARIABLES`.
from fclpy import printer as _printer
from fclpy.printer import write_object as _write_object


def stream_element_type(stream):
    """Get stream element type (simple fallback)."""
    return 'CHARACTER'


def stream_external_format(stream):
    """Get stream external format (simple fallback)."""
    return 'UTF-8'


def resolve_output_stream(designator):
    """Resolve an output stream designator (CLHS 21.1.3).

    NIL -- which is what a missing optional stream argument is -- designates the
    current value of `*STANDARD-OUTPUT*`, and T designates `*TERMINAL-IO*`.

    Every output function has to come through here. They used to default to
    Python's `print()`, so output went to the process's stdout regardless of
    what `*STANDARD-OUTPUT*` was bound to. That one defect is why the printer
    was unmeasurable: `(with-output-to-string (*standard-output*) (prin1 x))` is
    the shape every `def-print-test` in `printer/` uses to capture output, and
    it returned the empty string for all of them no matter what the printer did.
    """
    import fclpy.state as state

    if designator is True or designator is lisptype.T:
        name = '*TERMINAL-IO*'
    elif designator is None or designator is lisptype.NIL:
        name = '*STANDARD-OUTPUT*'
    else:
        return designator

    env = getattr(state, 'current_environment', None)
    symbol = lisptype.COMMON_LISP_PACKAGE.intern_symbol(name)
    if env is not None and env.has_variable(symbol):
        return env.find_variable(symbol)
    return getattr(symbol, 'value', None)


def write_text(text, stream=None):
    """Write `text` to an output stream designator -- the one place text is written.

    `PRIN1` and `PRINC` each carried their own four-branch copy of this
    dispatch (stdout / fclpy `Stream` / Python file-like / bare `except`), which
    is how they drifted apart from each other and from FORMAT about where
    unspecified output goes.
    """
    target = resolve_output_stream(stream)
    if target is None:
        # Reachable only before the standard environment exists, i.e. during
        # bootstrap. A real gap rather than the normal path, so it is not
        # quietly equivalent to writing to a stream.
        import sys
        sys.stdout.write(text)
        return
    from .streams import Stream
    if isinstance(target, Stream):
        target.write_sequence(text)
    elif isinstance(target, lisptype.LispString):
        # FORMAT accepts a string with a fill pointer as its destination and
        # appends to it (CLHS 22.3.1). Reaching here used to raise an
        # AttributeError that a bare `except` turned into printing at stdout,
        # so the output silently went somewhere other than the string.
        if target.fill_pointer is None:
            raise lisptype.LispTypeError(
                "FORMAT to a string requires a fill pointer: "
                f"{_write_object(target, escape=True)}")
        target._data[target.fill_pointer:] = list(text)
        target.fill_pointer += len(text)
    else:
        writer = getattr(target, 'write', None)
        if writer is None:
            raise lisptype.LispTypeError(f"Not an output stream: {target!r}")
        writer(text)
    if text:
        # Record the column for FRESH-LINE. Only a non-empty write moves it.
        try:
            setattr(target, _AT_LINE_START, text.endswith('\n'))
        except AttributeError:
            # A stream that refuses attributes (e.g. a raw file object with
            # __slots__) simply has no recorded column; `_at_line_start` then
            # reports "at a line start", so FRESH-LINE emits nothing rather
            # than a spurious newline.
            pass


# Re-export pathname functions from pathnames module for backward compatibility
# Note: make_pathname (registered as 'PATHNAME') and make_pathname_function
# (registered as 'MAKE-PATHNAME') are different functions!
from .pathnames import (
    make_pathname,  # PATHNAME function - converts string to Pathname
    make_pathname_function,  # MAKE-PATHNAME function - constructs pathname from components
    pathnamep,
    pathname_host,
    pathname_device,
    pathname_directory,
    pathname_name,
    pathname_type,
    pathname_version,
    namestring,
    directory_namestring,
    file_namestring,
    host_namestring,
    enough_namestring,
    parse_namestring,
    merge_pathnames,
    wild_pathname_p,
    pathname_match_p,
    translate_pathname,
    logical_pathname,
    translate_logical_pathname,
    truename,
    probe_file,
)

# Alias for backward compatibility - some code may use 'pathname' instead of 'make_pathname'
pathname = make_pathname


@_registry.cl_function('CLEAR-OUTPUT')
def clear_output(stream=None):
    """Clear output from stream."""
    return None


@_registry.cl_function('OUTPUT-STREAM-P')
def output_stream_p(stream):
    """Test if stream is output stream."""
    return lisptype.T  # Simplified


@_registry.cl_function('OPEN-STREAM-P')
def open_stream_p(stream):
    """Test if stream is open."""
    return lisptype.T  # Simplified


# I/O write operations
@_registry.cl_function('WRITE-CHAR')
def write_char(character, stream=None):
    """Write a character to a stream (CLHS 21.2)."""
    text = character.char if isinstance(character, lisptype.Character) else str(character)
    write_text(text, stream)
    return character


@_registry.cl_function('WRITE-STRING')
def write_string(string, stream=None, start=0, end=None):
    """Write a string's characters to a stream, without escapes (CLHS 21.2)."""
    text = str(string)
    if end is None or end is lisptype.NIL:
        end = len(text)
    write_text(text[start:end], stream)
    return string


@_registry.cl_function('WRITE-LINE')
def write_line(string, stream=None, start=0, end=None):
    """WRITE-STRING followed by a newline (CLHS 21.2)."""
    text = str(string)
    if end is None or end is lisptype.NIL:
        end = len(text)
    write_text(text[start:end] + '\n', stream)
    return string


@_registry.cl_function('WRITE-BYTE')
def write_byte(byte, stream):
    """Write byte to stream."""
    # Simplified implementation
    return byte


def _print_keywords(kwargs):
    """Translate WRITE's keyword arguments into printer overrides.

    WRITE and WRITE-TO-STRING accept a keyword for each printer control
    variable (CLHS 22.3.1), each overriding that variable for the call. They
    used to be collected into `**kwargs` and dropped on the floor, so
    `(write-to-string x :base 2)` ignored the base and `(write-to-string '(a)
    :escape nil)` still escaped. `printer.WRITE_KEYWORD_VARIABLES` is the one
    table of which keyword maps to which variable.
    """
    normalized = {key.lower().replace('-', '_'): value
                  for key, value in kwargs.items()}
    # `:allow-other-keys` is accepted by every function that takes keyword
    # arguments, and a true value makes unrecognized keywords legal to pass and
    # ignore (CLHS 3.4.1.4) -- `(write 5 :allow-other-keys t :foo 'bar)` prints
    # "5" rather than signalling.
    permissive = _printer._true(normalized.pop('allow_other_keys', None))

    overrides = {}
    for name, value in normalized.items():
        if name not in _printer.WRITE_KEYWORD_VARIABLES:
            if permissive:
                continue
            raise lisptype.LispProgramError(
                f"WRITE: unknown keyword argument :{name.upper().replace('_', '-')}")
        overrides[name] = value
    return overrides


@_registry.cl_function('WRITE')
def write(object, stream=None, **kwargs):
    """Print an object to a stream, honouring the printer keyword arguments.

    CLHS 22.3.1. WRITE is the general entry point: `PRIN1` and `PRINC` are it
    with `*PRINT-ESCAPE*` forced true and false respectively.
    """
    write_text(_write_object(object, **_print_keywords(kwargs)), stream)
    return object


@_registry.cl_function('PRIN1-TO-STRING')
def prin1_to_string(object):
    """The escaped printed representation, as a string (CLHS 22.3.1)."""
    return lisptype.LispString(_printer.prin1_to_string(object))


@_registry.cl_function('PRINC-TO-STRING')
def princ_to_string(object):
    """The unescaped printed representation, as a string (CLHS 22.3.1)."""
    return lisptype.LispString(_printer.princ_to_string(object))


@_registry.cl_function('WRITE-TO-STRING')
def write_to_string(object, **kwargs):
    """WRITE to a string instead of a stream (CLHS 22.3.1).

    Defaults to escaped output like `PRIN1`, not to `PRINC` -- it is WRITE, and
    WRITE honours `*PRINT-ESCAPE*`, whose initial value is true.
    """
    return lisptype.LispString(_write_object(object, **_print_keywords(kwargs)))


@_registry.cl_function('PRINT')
def print_fn(object, stream=None):
    """Newline, then the object escaped, then a space (CLHS 22.3.1).

    The order matters and was reversed: PRINT is defined as a `TERPRI`, then a
    `PRIN1`, then a space -- not `PRIN1` followed by a newline.
    """
    write_text('\n' + _printer.prin1_to_string(object) + ' ', stream)
    return object


@_registry.cl_function('PRIN1')
def prin1(object, stream=None):
    """Print an object escaped, with no surrounding whitespace (CLHS 22.3.1)."""
    write_text(_printer.prin1_to_string(object), stream)
    return object


@_registry.cl_function('PRINC')
def princ(object, stream=None):
    """Print an object with escaping off (CLHS 22.3.1).

    Not a separate representation from `PRIN1`: the same printer with
    `*PRINT-ESCAPE*` bound to NIL (CLHS 22.1.3.2).
    """
    write_text(_printer.princ_to_string(object), stream)
    return object


@_registry.cl_function('TERPRI')
def terpri(stream=None):
    """Output a newline (CLHS 21.2)."""
    write_text('\n', stream)
    return lisptype.NIL


@_registry.cl_function('FRESH-LINE')
def fresh_line(stream=None):
    """Output a newline only if not already at the start of a line (CLHS 21.2).

    Returns T if it output one, NIL otherwise. It used to output one
    unconditionally and return NIL, so `~&` could not be distinguished from
    `~%`. Column tracking is per stream, so a stream that does not report its
    position is assumed to need the newline.
    """
    target = resolve_output_stream(stream)
    if _at_line_start(target):
        return lisptype.NIL
    write_text('\n', stream)
    return lisptype.T


#: Attribute `write_text` stamps on a stream to record whether its last write
#: ended a line. FRESH-LINE needs to know the column, and neither fclpy's
#: `Stream` nor a Python file object reports one; tracking it at the single
#: point where text is written is cheaper and more accurate than trying to
#: recover it afterwards, and it works for file streams too, not just the
#: string streams whose buffer can be re-read.
_AT_LINE_START = '_fclpy_at_line_start'


def _at_line_start(target):
    """True when `target`'s next character would begin a line.

    A string output stream's buffer is authoritative and is preferred, since
    text can reach it by paths other than `write_text`. Otherwise the flag
    `write_text` recorded is used. A stream that has never been written to is at
    the start of a line.
    """
    if target is None:
        return False
    from .streams import StringOutputStream
    if isinstance(target, StringOutputStream):
        text = target.peek_string()
        return text == '' or text.endswith('\n')
    return getattr(target, _AT_LINE_START, True)

@_registry.cl_function('FINISH-OUTPUT')
def finish_output(stream=None):
    """Finish output to stream."""
    return None


@_registry.cl_function('FORCE-OUTPUT')
def force_output(stream=None):
    """Force output to stream."""
    return None


@_registry.cl_function('MAKE-STRING-OUTPUT-STREAM')
def make_string_output_stream(**kwargs):
    """Make string output stream - delegates to streams.py."""
    from .streams import make_string_output_stream as _make_sos
    element_type = kwargs.get('element_type', 'character')
    return _make_sos(element_type)


@_registry.cl_function('GET-OUTPUT-STREAM-STRING')
def get_output_stream_string(stream):
    """Get string from output stream - delegates to streams.py."""
    from .streams import get_output_stream_string as _get_oss
    return _get_oss(stream)


@_registry.cl_function('MAKE-BROADCAST-STREAM')
def make_broadcast_stream(*streams):
    """Make broadcast stream."""
    return streams[0] if streams else None


@_registry.cl_function('MAKE-CONCATENATED-STREAM')
def make_concatenated_stream(*streams):
    """Make concatenated stream."""
    return streams[0] if streams else None


@_registry.cl_function('MAKE-ECHO-STREAM')
def make_echo_stream(input_stream, output_stream):
    """Make echo stream."""
    return output_stream


@_registry.cl_function('MAKE-SYNONYM-STREAM')
def make_synonym_stream(symbol):
    """Make synonym stream."""
    return str(symbol)


@_registry.cl_function('MAKE-TWO-WAY-STREAM')
def make_two_way_stream(input_stream, output_stream):
    """Make two-way stream."""
    return output_stream


# Pretty printing operations
@_registry.cl_function('COPY-PPRINT-DISPATCH')
def copy_pprint_dispatch(table=None):
    """Copy pretty print dispatch table."""
    return {}  # Simplified


@_registry.cl_function('PPRINT')
def pprint(object, stream=None):
    """Pretty print object."""
    print(object)
    return None


@_registry.cl_function('PPRINT-DISPATCH')
def pprint_dispatch(object, table=None):
    """Get pretty print dispatch function (stub)."""
    return print, lisptype.NIL  # Simplified


@_registry.cl_function('PPRINT-EXIT-IF-LIST-EXHAUSTED')
def pprint_exit_if_list_exhausted():
    """Exit if list exhausted (stub)."""
    return None


@_registry.cl_function('PPRINT-INDENT')
def pprint_indent(relative_to, n, stream=None):
    """Set pretty print indent (stub)."""
    return None


@_registry.cl_function('PPRINT-LINEAR')
def pprint_linear(stream, object, prefix=None, per_line_prefix=None, suffix=None):
    """Linear pretty print (stub)."""
    print(object)
    return None


@_registry.cl_function('PPRINT-LOGICAL-BLOCK')
def pprint_logical_block(stream, object, prefix=None, per_line_prefix=None, suffix=None):
    """Logical block pretty print (stub)."""
    print(object)
    return None


@_registry.cl_function('PPRINT-NEWLINE')
def pprint_newline(kind, stream=None):
    """Pretty print newline (stub)."""
    print()
    return None


@_registry.cl_function('PPRINT-POP')
def pprint_pop():
    """Pretty print pop (stub)."""
    return None


@_registry.cl_function('PPRINT-TAB')
def pprint_tab(kind, colnum, colinc, stream=None):
    """Pretty print tab (stub)."""
    return None


@_registry.cl_function('PPRINT-TABULAR')
def pprint_tabular(stream, object, prefix=None, per_line_prefix=None, suffix=None):
    """Tabular pretty print (stub)."""
    print(object)
    return None


@_registry.cl_function('PPRINT-FILL')
def pprint_fill(stream, list_obj, colon_p=None, at_sign_p=None):
    """Pretty print fill (stub)."""
    print(list_obj)
    return None


@_registry.cl_function('SET-PPRINT-DISPATCH')
def set_pprint_dispatch(type_specifier, function, priority=0, table=None):
    """Set pretty print dispatch."""
    return None


# Format operations

class _FormatCursor:
    """Mutable argument cursor for FORMAT.

    This is the structural fix for FORMAT's argument-consumption model:
    directives that give a nested control string access to the *same*
    argument stream (~<...~>, ~(...~), ~[...~]) share one cursor instance,
    so that arguments consumed inside the nested directive (including via
    ~:*, ~*, ~:P) are visible to whatever follows the directive in the
    outer control string. Previously each nested call sliced `args` and
    started a fresh index at 0, which silently discarded consumption.

    Directives that give a nested control string its *own* independent
    argument scope per CLHS (~{...~} iterating over a list argument's
    elements; ~? with a separate format-args list) construct a fresh
    cursor instead of sharing this one - that is correct, not a bug.
    """
    __slots__ = ('args', 'idx')

    def __init__(self, args, idx=0):
        self.args = list(args) if args else []
        self.idx = idx

    def next(self):
        if self.idx < len(self.args):
            val = self.args[self.idx]
            self.idx += 1
            return val
        return None

    def prev(self):
        """The argument last consumed, without consuming another (~:P)."""
        if 0 < self.idx <= len(self.args):
            return self.args[self.idx - 1]
        return None

    def remaining(self):
        return self.args[self.idx:]

    def remaining_count(self):
        return len(self.args) - self.idx


class _FormatEscape(Exception):
    """Raised by `~^` to terminate the enclosing iteration or control string.

    CLHS 22.3.9.2 makes `~^` a *control transfer*, not a character: it
    abandons the rest of the control string it appears in. The previous
    implementation returned an in-band `'\\u0000'` marker and let callers
    `str.replace` it out, which is the same defect class as standing rule 4
    -- it cannot represent "stop here" distinctly from "emit a NUL", it
    silently corrupted any output that legitimately contained NUL, and the
    marker had to be re-detected by every construct that might contain a
    `~^`.

    `partial` carries the text produced *before* the escape, which CLHS
    requires be kept: `~{~A~^, ~}` over `(1 2 3)` must emit `1, 2, 3` --
    the final pass's `1`-equivalent survives, only the trailing `, ` is
    abandoned. Each `_format_process_cursor` frame prepends its own
    accumulated output as the exception unwinds, so the text is assembled
    in the same order it would have been concatenated.

    `terminate_outer` is set by `~:^`, which terminates the iteration one
    level out rather than the innermost one.
    """

    def __init__(self, partial='', terminate_outer=False):
        super().__init__('FORMAT ~^ escape')
        self.partial = partial
        self.terminate_outer = terminate_outer


def _format_args_list(value):
    """Coerce a FORMAT argument that is *required to be a list* into a Python list.

    `~{...~}` and `~?` take a list argument and iterate over its elements.
    Every such site previously tested `isinstance(value, (list, tuple))` and
    fell through to `[value]`, but a Lisp list reaching FORMAT is a
    `lispCons`, never a Python list -- so the test was false for exactly the
    argument shape the directive exists to handle, and the whole list was
    treated as a single opaque element. `(format nil "~{~A ~}" '(1 2 3))`
    returned `"(1 2 3) "` instead of `"1 2 3 "`.

    Cons traversal is delegated to the sequence protocol's `_seq_to_list`
    rather than open-coded a third time (standing rule 3); this function
    only adds the FORMAT-specific edges: NIL is the empty argument list, and
    a non-list argument stays wrapped rather than silently becoming empty.
    """
    if value is None or value is lisptype.NIL:
        return []
    if isinstance(value, (list, tuple)):
        return list(value)
    if hasattr(value, 'car') and hasattr(value, 'cdr'):
        from .sequences_search import _seq_to_list
        return _seq_to_list(value)
    return [value]


def _lisp_number(value, default=0):
    """Read a `~^` prefix parameter as an integer.

    A parameter is either a literal from the control string (already an
    int) or whatever `~V` pulled off the argument list, which may be a Lisp
    integer object. Anything non-numeric falls back to `default` rather
    than raising, because `~^`'s parameters only select between "terminate"
    and "keep going" -- a malformed one must not abort the whole FORMAT.
    """
    if value is None:
        return default
    if isinstance(value, bool):
        return int(value)
    if isinstance(value, int):
        return value
    try:
        return int(value)
    except (TypeError, ValueError):
        return default


def _pad_params(params):
    """Read the `mincol,colinc,minpad,padchar` prefix parameters shared by
    `~A`, `~S` and `~<...~>`, applying each one's CLHS default."""
    def _param(i, default):
        if len(params) > i and params[i] is not None:
            return params[i]
        return default

    padchar = _param(3, ' ')
    # A `'x` prefix parameter is parsed as a bare Python character, but a
    # `~V` parameter supplies whatever argument was passed -- which for a
    # pad character is a Lisp CHARACTER object, not a str.
    if isinstance(padchar, lisptype.Character):
        padchar = padchar.char
    padchar = str(padchar)[:1] or ' '

    return (
        _lisp_number(_param(0, 0)),
        _lisp_number(_param(1, 1)) or 1,
        _lisp_number(_param(2, 0)),
        padchar,
    )


def _format_pad(text, params, at_flag):
    """Apply CLHS 22.3's `mincol,colinc,minpad,padchar` column padding.

    Shared by `~A` and `~S`, which specify identical padding behaviour and
    previously each honoured only `mincol` with a hardcoded space -- so
    `~,,2A` (minpad) and `~4,,,'xA` (padchar) were silently ignored rather
    than being unimplemented loudly.

    The rule: emit at least `minpad` copies of `padchar`, then keep adding
    `colinc` more until the total width is at least `mincol`. Padding goes
    on the right, or on the left when the `@` modifier is present.
    """
    mincol, colinc, minpad, padchar = _pad_params(params)

    pad = minpad
    while len(text) + pad < mincol:
        pad += colinc

    if pad <= 0:
        return text
    padding = padchar * pad
    return padding + text if at_flag else text + padding


def _justify(texts, params, colon_flag, at_flag):
    """Lay out `~mincol,colinc,minpad,padchar<seg~;seg~>` per CLHS 22.3.6.2.

    Padding is inserted at the *gaps between* segments, not around the whole
    string: `~;` is a padding point. The `:` modifier adds a padding point
    before the first segment and `@` one after the last, so `~10<abc~>` --
    one segment, no modifiers -- has its single padding point on the left
    and therefore right-justifies, which is the common case.

    Total width grows from `minpad` per gap in steps of `colinc` until it
    reaches `mincol`; the resulting spaces are spread as evenly as possible,
    with the leftmost gaps taking the remainder.
    """
    if not texts:
        texts = ['']

    mincol, colinc, minpad, padchar = _pad_params(params)

    # Where padding may go: one point per `~;` separator, plus a leading one
    # for `:` and a trailing one for `@`. A lone segment with neither
    # modifier still gets its single implicit point, on the left -- which is
    # what makes `~10<abc~>` right-justify.
    gaps = len(texts) - 1
    if colon_flag:
        gaps += 1
    if at_flag:
        gaps += 1
    if gaps == 0:
        gaps = 1

    content_width = sum(len(t) for t in texts)
    total_pad = minpad * gaps
    while content_width + total_pad < mincol:
        total_pad += colinc

    base, extra = divmod(total_pad, gaps)
    widths = [base + (1 if i < extra else 0) for i in range(gaps)]

    out = []
    gap_index = 0
    if colon_flag:
        out.append(padchar * widths[gap_index])
        gap_index += 1
    for i, text in enumerate(texts):
        out.append(text)
        is_last = (i == len(texts) - 1)
        if not is_last:
            out.append(padchar * widths[gap_index])
            gap_index += 1
        elif gap_index < len(widths):
            # Either the trailing @ point, or the implicit leading point of a
            # lone segment -- which must go *before* the text, not after.
            if at_flag or len(texts) > 1:
                out.append(padchar * widths[gap_index])
            else:
                out.insert(len(out) - 1, padchar * widths[gap_index])
            gap_index += 1
    return ''.join(out)


def _capitalize_words(s):
    """~:( ... ~) - capitalize the first letter of each word, force the
    rest of each word to lower case."""
    result = []
    at_word_start = True
    for ch in s:
        if ch.isalpha():
            result.append(ch.upper() if at_word_start else ch.lower())
            at_word_start = False
        else:
            result.append(ch)
            at_word_start = True
    return ''.join(result)


def _capitalize_first_word(s):
    """~@( ... ~) - capitalize the first letter of the first word, force
    the rest of the output to lower case."""
    result = []
    capitalized_any = False
    at_word_start = True
    for ch in s:
        if ch.isalpha():
            if not capitalized_any and at_word_start:
                result.append(ch.upper())
                capitalized_any = True
            else:
                result.append(ch.lower())
            at_word_start = False
        else:
            result.append(ch)
            at_word_start = True
    return ''.join(result)


def _is_unspecified(value):
    """A FORMAT prefix parameter position that was left blank (`None`, an
    empty comma slot) or supplied as Lisp NIL (typically via `~V`)."""
    return value is None or value is lisptype.NIL


def _format_char_param(value, default):
    """Read a pad/comma character prefix parameter: `'x` literal, `~V`
    (which supplies a CHARACTER object or a one-char string), or unspecified."""
    if _is_unspecified(value):
        return default
    if isinstance(value, lisptype.Character):
        return value.char
    s = str(value)
    return s[:1] if s else default


def _numeric_pad_params(params):
    """Read the `mincol,padchar,commachar,comma-interval` prefix parameters
    shared by `~D`, `~X`, `~O`, `~B` and `~R` (CLHS 22.3.2), applying each
    one's default. `comma-interval` defaults to 3 and a non-positive value
    (which would make grouping meaningless) falls back to the same default."""
    def _p(i):
        return params[i] if len(params) > i else None

    mincol = _lisp_number(_p(0), 0)
    padchar = _format_char_param(_p(1), ' ')
    commachar = _format_char_param(_p(2), ',')
    comma_interval = _lisp_number(_p(3), 3)
    if comma_interval <= 0:
        comma_interval = 3
    return mincol, padchar, commachar, comma_interval


_RADIX_DIGITS = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"


def _int_to_radix_digits(num, radix):
    """Render a non-negative integer's magnitude in `radix` (2-36), one
    engine for `~D`/`~X`/`~O`/`~B` and explicit-radix `~R` instead of four
    copies each hardcoded to a Python builtin (`hex`/`oct`/`bin`) that only
    covers three of the five radices FORMAT needs."""
    if num == 0:
        return '0'
    chars = []
    while num:
        num, rem = divmod(num, radix)
        chars.append(_RADIX_DIGITS[rem])
    return ''.join(reversed(chars))


def _insert_comma_groups(digits, commachar, interval):
    """Group `digits` from the right in chunks of `interval`, per CLHS
    22.3.2's `:` modifier. A single group (interval >= len(digits)) gets no
    separator at all -- that is what makes `~,,,#:d` applied with a
    comma-interval equal to the digit count print with no commas."""
    n = len(digits)
    if interval <= 0 or interval >= n:
        return digits
    first_len = n % interval or interval
    parts = [digits[:first_len]]
    for i in range(first_len, n, interval):
        parts.append(digits[i:i + interval])
    return commachar.join(parts)


def _format_A_fallback(val):
    """CLHS 22.3.2: if the argument to `~D`/`~X`/`~O`/`~B`/`~R` is not an
    integer, it is printed as if by `~A` -- a fresh, unmodified `~A`, not
    this directive's own colon/at flags reinterpreted."""
    return _printer.princ_to_string(val)


def _format_integer_directive(val, radix, params, colon_flag, at_flag):
    """The shared digit-printing engine behind `~D`/`~X`/`~O`/`~B` and
    explicit-radix `~R`: sign, radix conversion, `:` comma-grouping, then
    `mincol`/`padchar` right-justification -- in that order, since padding
    must see the sign and commas already in place (CLHS 22.3.2)."""
    if not isinstance(val, int):
        return _format_A_fallback(val)
    mincol, padchar, commachar, comma_interval = _numeric_pad_params(params)
    neg = val < 0
    digits = _int_to_radix_digits(abs(val), radix)
    if colon_flag:
        digits = _insert_comma_groups(digits, commachar, comma_interval)
    sign = '-' if neg else ('+' if at_flag else '')
    body = sign + digits
    if len(body) < mincol:
        body = padchar * (mincol - len(body)) + body
    return body


_ENGLISH_ONES = [
    "zero", "one", "two", "three", "four", "five", "six", "seven", "eight",
    "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
    "sixteen", "seventeen", "eighteen", "nineteen",
]
_ENGLISH_TENS = [
    "", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy",
    "eighty", "ninety",
]
_ENGLISH_SCALES = [
    "", "thousand", "million", "billion", "trillion", "quadrillion",
    "quintillion", "sextillion", "septillion", "octillion", "nonillion",
    "decillion", "undecillion", "duodecillion", "tredecillion",
]


def _english_below_100(n):
    if n < 20:
        return _ENGLISH_ONES[n]
    tens, rem = divmod(n, 10)
    return _ENGLISH_TENS[tens] + ("-" + _ENGLISH_ONES[rem] if rem else "")


def _english_below_1000(n):
    hundreds, rem = divmod(n, 100)
    if hundreds == 0:
        return _english_below_100(rem)
    text = _ENGLISH_ONES[hundreds] + " hundred"
    return text + (" " + _english_below_100(rem) if rem else "")


def _english_cardinal(n):
    """Spell `n` out in English (CLHS 22.3.2's `~R` with no radix and
    neither modifier). Groups by thousands rather than a 0-100 lookup table
    so it is not silently wrong the moment a test uses 101."""
    if n == 0:
        return "zero"
    neg = n < 0
    n = abs(n)
    groups = []
    scale = 0
    while n > 0:
        n, grp = divmod(n, 1000)
        if grp:
            groups.append((grp, scale))
        scale += 1
    parts = []
    for grp, idx in reversed(groups):
        text = _english_below_1000(grp)
        if idx > 0:
            scale_name = (_ENGLISH_SCALES[idx] if idx < len(_ENGLISH_SCALES)
                          else "*1000^%d" % idx)
            text += " " + scale_name
        parts.append(text)
    return ("negative " if neg else "") + " ".join(parts)


_ENGLISH_ORDINAL_ONES = {
    "zero": "zeroth", "one": "first", "two": "second", "three": "third",
    "four": "fourth", "five": "fifth", "six": "sixth", "seven": "seventh",
    "eight": "eighth", "nine": "ninth", "ten": "tenth", "eleven": "eleventh",
    "twelve": "twelfth", "thirteen": "thirteenth", "fourteen": "fourteenth",
    "fifteen": "fifteenth", "sixteen": "sixteenth", "seventeen": "seventeenth",
    "eighteen": "eighteenth", "nineteen": "nineteenth",
}
_ENGLISH_ORDINAL_TENS = {
    "twenty": "twentieth", "thirty": "thirtieth", "forty": "fortieth",
    "fifty": "fiftieth", "sixty": "sixtieth", "seventy": "seventieth",
    "eighty": "eightieth", "ninety": "ninetieth",
}
_ENGLISH_ORDINAL_SCALES = {
    name: name + "th" for name in _ENGLISH_SCALES if name
}
_ENGLISH_ORDINAL_SCALES["hundred"] = "hundredth"


def _ordinal_word(word):
    if '-' in word:
        prefix, suffix = word.rsplit('-', 1)
        return prefix + '-' + _ENGLISH_ORDINAL_ONES.get(suffix, suffix)
    return (_ENGLISH_ORDINAL_TENS.get(word)
            or _ENGLISH_ORDINAL_SCALES.get(word)
            or _ENGLISH_ORDINAL_ONES.get(word, word))


def _english_ordinal(n):
    """`~:R`: only the *last* word of the cardinal spelling becomes ordinal
    (CLHS gives `one hundredth`, not `oneth hundredth`) -- so this builds on
    `_english_cardinal` rather than a second number-to-words implementation."""
    cardinal = _english_cardinal(n)
    prefix = ""
    if cardinal.startswith("negative "):
        prefix, cardinal = "negative ", cardinal[len("negative "):]
    words = cardinal.split(" ")
    words[-1] = _ordinal_word(words[-1])
    return prefix + " ".join(words)


_ROMAN_TABLE = [
    (1000, 'M'), (900, 'CM'), (500, 'D'), (400, 'CD'),
    (100, 'C'), (90, 'XC'), (50, 'L'), (40, 'XL'),
    (10, 'X'), (9, 'IX'), (5, 'V'), (4, 'IV'), (1, 'I'),
]
_OLD_ROMAN_TABLE = [
    (1000, 'M'), (500, 'D'), (100, 'C'), (50, 'L'), (10, 'X'), (5, 'V'), (1, 'I'),
]


def _roman_numeral(n, table):
    if n <= 0:
        return ''
    parts = []
    for value, sym in table:
        count, n = divmod(n, value)
        parts.append(sym * count)
    return ''.join(parts)


def _format_directive(control_string, cursor, pos, emitted=None):
    """Process a single format directive starting at pos (after ~).

    Consumes arguments from `cursor` (a _FormatCursor), mutating it in
    place. Returns (output_string, new_pos).

    `emitted` is the list of output chunks produced so far in this control
    string, which `~&` needs in order to know whether it is already at the
    start of a line.
    """
    if pos >= len(control_string):
        return ('~', pos)
    
    # Parse optional parameters: [prefix_params][:][@][directive]
    # Prefix params can be: number, 'char, V (next arg), #, or comma-separated
    colon_flag = False
    at_flag = False
    params = []
    
    # Skip optional numeric/char parameters and commas
    while pos < len(control_string):
        c = control_string[pos]
        if c.isdigit() or c == '-' or c == '+':
            # Parse number
            num_start = pos
            if c in '-+':
                pos += 1
            while pos < len(control_string) and control_string[pos].isdigit():
                pos += 1
            params.append(int(control_string[num_start:pos]))
        elif c == "'":
            # Character parameter 'X
            if pos + 1 < len(control_string):
                params.append(control_string[pos + 1])
                pos += 2
            else:
                pos += 1
        elif c == 'V' or c == 'v':
            # Use next argument as parameter
            params.append(cursor.next())
            pos += 1
        elif c == '#':
            # Number of remaining arguments
            params.append(cursor.remaining_count())
            pos += 1
        elif c == ',':
            pos += 1
            # Empty parameter slot
            if not params or control_string[pos-2] == ',':
                params.append(None)
        elif c == ':':
            colon_flag = True
            pos += 1
        elif c == '@':
            at_flag = True
            pos += 1
        else:
            break

    if pos >= len(control_string):
        return ('~', pos)

    directive = control_string[pos].upper()
    pos += 1

    # Helper to get next arg
    def get_arg():
        return cursor.next()

    # Process directives
    if directive == 'A':
        # ~A - Aesthetic: print as PRINC does, i.e. with *PRINT-ESCAPE* nil.
        # `~:A` prints NIL as "()" rather than "NIL" (CLHS 22.3.4.1).
        val = get_arg()
        if colon_flag and (val is None or val is lisptype.NIL):
            result = "()"
        else:
            result = _printer.princ_to_string(val)
        return (_format_pad(result, params, at_flag), pos)

    elif directive == 'S':
        # ~S - Standard: print as PRIN1 does, i.e. with *PRINT-ESCAPE* true.
        val = get_arg()
        if colon_flag and (val is None or val is lisptype.NIL):
            result = "()"
        else:
            result = _printer.prin1_to_string(val)
        return (_format_pad(result, params, at_flag), pos)


    elif directive == 'D':
        # ~D - Decimal integer (CLHS 22.3.2)
        return (_format_integer_directive(get_arg(), 10, params, colon_flag, at_flag), pos)

    elif directive == 'X':
        # ~X - Hexadecimal
        return (_format_integer_directive(get_arg(), 16, params, colon_flag, at_flag), pos)

    elif directive == 'O':
        # ~O - Octal
        return (_format_integer_directive(get_arg(), 8, params, colon_flag, at_flag), pos)

    elif directive == 'B':
        # ~B - Binary
        return (_format_integer_directive(get_arg(), 2, params, colon_flag, at_flag), pos)

    elif directive == 'R':
        # ~R - explicit radix (same mincol/padchar/commachar/comma-interval
        # tail as ~D/~X/~O/~B, radix is just an extra leading parameter) or,
        # with no parameters at all, English/Roman spelling per CLHS 22.3.2.
        if params and not _is_unspecified(params[0]):
            radix = _lisp_number(params[0], 10)
            val = get_arg()
            if isinstance(val, int):
                result = _format_integer_directive(val, radix, params[1:], colon_flag, at_flag)
            else:
                result = _format_A_fallback(val)
        else:
            val = get_arg()
            if not isinstance(val, int):
                result = _format_A_fallback(val)
            elif at_flag and colon_flag:
                result = _roman_numeral(val, _OLD_ROMAN_TABLE) if val > 0 else ('-' + _roman_numeral(-val, _OLD_ROMAN_TABLE) if val < 0 else '')
            elif at_flag:
                result = _roman_numeral(val, _ROMAN_TABLE) if val > 0 else ('-' + _roman_numeral(-val, _ROMAN_TABLE) if val < 0 else '')
            elif colon_flag:
                result = _english_ordinal(val)
            else:
                result = _english_cardinal(val)
        return (result, pos)
    
    elif directive == 'C':
        # ~C - Character
        val = get_arg()
        if isinstance(val, lisptype.Character):
            if colon_flag:
                # Pretty print special characters
                char_names = {' ': 'Space', '\n': 'Newline', '\t': 'Tab', '\r': 'Return'}
                result = char_names.get(val.char, val.char)
            elif at_flag:
                # Lisp readable form
                char_names = {' ': '#\\Space', '\n': '#\\Newline', '\t': '#\\Tab', '\r': '#\\Return'}
                result = char_names.get(val.char, '#\\' + val.char)
            else:
                result = val.char
        elif isinstance(val, str) and len(val) == 1:
            result = val
        else:
            result = str(val) if val else ''
        return (result, pos)
    
    elif directive == 'F':
        # ~F - Fixed-format floating point
        val = get_arg()
        try:
            num = float(val) if val is not None else 0.0
            # params: width, digits, scale, overflow-char, pad-char
            width = params[0] if params else None
            digits = params[1] if len(params) > 1 else None
            if digits is not None:
                result = f'{num:.{digits}f}'
            else:
                result = str(num)
            if at_flag and num >= 0:
                result = '+' + result
            if width and len(result) < width:
                result = ' ' * (width - len(result)) + result
        except (TypeError, ValueError):
            result = str(val)
        return (result, pos)
    
    elif directive == 'E':
        # ~E - Exponential floating point
        val = get_arg()
        try:
            num = float(val) if val is not None else 0.0
            digits = params[1] if len(params) > 1 and params[1] else 6
            result = f'{num:.{digits}e}'.upper()
            if at_flag and num >= 0:
                result = '+' + result
        except (TypeError, ValueError):
            result = str(val)
        return (result, pos)
    
    elif directive == 'G':
        # ~G - General floating point (choose F or E)
        val = get_arg()
        try:
            num = float(val) if val is not None else 0.0
            result = f'{num:g}'
            if at_flag and num >= 0:
                result = '+' + result
        except (TypeError, ValueError):
            result = str(val)
        return (result, pos)
    
    elif directive == '%':
        # ~% - Newline
        count = params[0] if params and params[0] else 1
        return ('\n' * count, pos)

    elif directive == '&':
        # ~n& - a fresh line, then n-1 further newlines (CLHS 22.3.1.3).
        #
        # It used to emit n newlines unconditionally, with the comment "we
        # don't track column". The column within this control string is exactly
        # what has been emitted so far, so `~&` is a fresh line for the same
        # reason FRESH-LINE is: emit one only if the output does not already
        # end at a line boundary. `~0&` emits nothing at all.
        count = 1 if not params or params[0] is None else params[0]
        if count <= 0:
            return ('', pos)
        preceding = ''.join(emitted) if emitted else ''
        needs_fresh_line = preceding != '' and not preceding.endswith('\n')
        return ('\n' * (count - 1 + int(needs_fresh_line)), pos)

    elif directive == '~':
        # ~~ - Literal tilde
        count = params[0] if params and params[0] else 1
        return ('~' * count, pos)

    elif directive == '|':
        # ~| - Page separator (form feed)
        count = params[0] if params and params[0] else 1
        return ('\f' * count, pos)

    elif directive == 'T':
        # ~T - Tabulation
        colnum = params[0] if params else 1
        colinc = params[1] if len(params) > 1 else 1
        # We don't track column, so just emit spaces
        return (' ' * (colnum if colnum else 1), pos)

    elif directive == '*':
        # ~* - Go to argument
        if at_flag:
            # Go to absolute argument position
            cursor.idx = params[0] if params and params[0] is not None else 0
        elif colon_flag:
            # Go backwards
            count = params[0] if params and params[0] is not None else 1
            cursor.idx = max(0, cursor.idx - count)
        else:
            # Go forwards
            count = params[0] if params and params[0] is not None else 1
            cursor.idx = min(len(cursor.args), cursor.idx + count)
        return ('', pos)

    elif directive == '?':
        # ~? - Recursive processing
        # The next arg is a format string, and the one after is args for it
        fmt_str = get_arg()
        if at_flag:
            # ~@? shares the outer argument stream: the recursive format
            # consumes from the same cursor, and only what it actually uses
            # is unavailable to directives that follow the ~? in the outer
            # control string.
            result = _format_process_cursor(str(fmt_str) if fmt_str else '', cursor)
        else:
            # ~? without @ takes its own separate argument list - not the
            # outer cursor - so it gets a fresh, independent cursor.
            fmt_args = get_arg()
            sub_cursor = _FormatCursor(_format_args_list(fmt_args))
            result = _format_process_cursor(str(fmt_str) if fmt_str else '', sub_cursor)
        return (result, pos)
    
    elif directive == '<':
        # ~< ... ~> - Justification/Logical block
        # This is a complex directive for text justification and pretty printing
        # For now, implement a simplified version that processes content between separators
        # Find matching ~>
        nesting = 1
        end_pos = pos
        segments = []
        # Whether the `~;` that *ended* each segment carried a colon. Only
        # the first one is meaningful (see the ~:; handling below), but it
        # is only knowable while scanning, so it is recorded per segment.
        separator_colons = []
        segment_start = pos

        while end_pos < len(control_string) and nesting > 0:
            if control_string[end_pos] == '~':
                if end_pos + 1 < len(control_string):
                    # Skip any modifiers to find directive char
                    j = end_pos + 1
                    while j < len(control_string) and control_string[j] in '0123456789,:#@':
                        j += 1
                    if j < len(control_string):
                        next_char = control_string[j].upper()
                        has_colon = ':' in control_string[end_pos+1:j]
                        
                        if next_char == '<':
                            nesting += 1
                            end_pos = j + 1
                        elif next_char == '>':
                            nesting -= 1
                            if nesting == 0:
                                # Found the closing ~>
                                segments.append(control_string[segment_start:end_pos])
                                separator_colons.append(False)
                                end_pos = j + 1  # Position after the closing >
                                break
                            end_pos = j + 1
                        elif next_char == ';' and nesting == 1:
                            # Separator within the justification block
                            segments.append(control_string[segment_start:end_pos])
                            separator_colons.append(has_colon)
                            segment_start = j + 1
                            end_pos = j + 1
                        else:
                            end_pos = j + 1
                    else:
                        end_pos += 1
                else:
                    end_pos += 1
            else:
                end_pos += 1
        else:
            # If we exited the loop without finding closing ~>
            segments.append(control_string[segment_start:])
            separator_colons.append(False)
            end_pos = len(control_string)

        # CLHS 22.3.6.2: a first segment terminated by `~:;` is not content
        # -- it is the prefix emitted only when the block has to be broken
        # across lines. There is no line-width model here, so the block is
        # always one line and the prefix is omitted. `has_colon` was already
        # being computed by the scanner above and then discarded, which is
        # why `~<pfx~:;body~>` had no defined behaviour either way.
        if len(segments) > 1 and separator_colons[0]:
            segments = segments[1:]

        # CLHS 22.3.6.2: *every* segment is output, and padding is
        # distributed among the gaps between them so the whole reaches
        # mincol. The previous code processed only `segments[-1]` and
        # dropped the rest, so `~<~A~;~A~>` printed just its second
        # argument and no justification ever happened.
        #
        # All segments share the outer cursor: arguments consumed inside the
        # block must not be re-offered to directives that follow the ~>.
        texts = []
        for seg in segments:
            try:
                texts.append(_format_process_cursor(seg, cursor))
            except _FormatEscape as esc:
                # ~^ inside a justification abandons the remaining segments
                # but keeps what this one produced.
                texts.append(esc.partial)
                break

        return (_justify(texts, params, colon_flag, at_flag), end_pos)

    elif directive == '>':
        # End of justification - should not be reached directly
        return ('', pos)

    elif directive == '(':
        # ~( ... ~) - Case conversion
        # Find matching ~)
        nesting = 1
        end_pos = pos
        while end_pos < len(control_string) and nesting > 0:
            if control_string[end_pos] == '~':
                if end_pos + 1 < len(control_string):
                    # Skip any modifiers to find directive char
                    j = end_pos + 1
                    while j < len(control_string) and control_string[j] in '0123456789,:#@':
                        j += 1
                    if j < len(control_string):
                        next_char = control_string[j].upper()
                        if next_char == '(':
                            nesting += 1
                            end_pos = j + 1
                        elif next_char == ')':
                            nesting -= 1
                            if nesting == 0:
                                # end_pos points to ~, j points to )
                                inner = control_string[pos:end_pos]
                                end_pos = j + 1  # Position after the closing )
                                break
                            end_pos = j + 1
                        else:
                            end_pos += 1
                    else:
                        end_pos += 1
                else:
                    end_pos += 1
            else:
                end_pos += 1
        else:
            # If we exited the loop without finding closing ~)
            inner = control_string[pos:]
        
        # Shares the outer cursor: consumption is now exact (the cursor
        # tracks it directly), replacing the old inner.count('~') estimate.
        inner_result = _format_process_cursor(inner, cursor)

        if colon_flag and at_flag:
            # ~:@( ... ~) - force everything to upper case
            result = inner_result.upper()
        elif colon_flag:
            # ~:( ... ~) - capitalize each word
            result = _capitalize_words(inner_result)
        elif at_flag:
            # ~@( ... ~) - capitalize just the first word, lower case the rest
            result = _capitalize_first_word(inner_result)
        else:
            # ~( ... ~) - force everything to lower case
            result = inner_result.lower()

        return (result, end_pos)
    
    elif directive == ')':
        # End of case conversion - should not be reached directly
        return ('', pos)
    
    elif directive == '[':
        # ~[ ... ~] - Conditional
        # Find the clauses and closing ~]
        nesting = 1
        angle_nesting = 0  # Track ~< ~> nesting
        paren_nesting = 0  # Track ~( ~) nesting
        brace_nesting = 0  # Track ~{ ~} nesting
        clauses = []
        clause_start = pos
        i = pos
        default_clause = None
        end_pos = pos
        
        while i < len(control_string) and nesting > 0:
            if control_string[i] == '~':
                if i + 1 < len(control_string):
                    # Skip params to find directive
                    j = i + 1
                    while j < len(control_string) and control_string[j] in '0123456789,:#@':
                        j += 1
                    if j < len(control_string):
                        d = control_string[j].upper()
                        if d == '[':
                            nesting += 1
                            i = j + 1
                        elif d == ']':
                            nesting -= 1
                            if nesting == 0:
                                clauses.append(control_string[clause_start:i])
                                end_pos = j + 1  # Position after ]
                            i = j + 1
                        elif d == '<':
                            angle_nesting += 1
                            i = j + 1
                        elif d == '>':
                            angle_nesting -= 1
                            i = j + 1
                        elif d == '(':
                            paren_nesting += 1
                            i = j + 1
                        elif d == ')':
                            paren_nesting -= 1
                            i = j + 1
                        elif d == '{':
                            brace_nesting += 1
                            i = j + 1
                        elif d == '}':
                            brace_nesting -= 1
                            i = j + 1
                        elif d == ';' and nesting == 1 and angle_nesting == 0 and paren_nesting == 0 and brace_nesting == 0:
                            # Only treat as clause separator if we're not inside nested ~< ~> or ~( ~) or ~{ ~}
                            clauses.append(control_string[clause_start:i])
                            # Check for :; (default clause)
                            if ':' in control_string[i+1:j+1]:
                                default_clause = len(clauses)
                            clause_start = j + 1
                            i = j + 1
                        else:
                            i += 1
                    else:
                        i += 1
                else:
                    i += 1
            else:
                i += 1
        
        if end_pos == pos:
            end_pos = i  # Fallback if we didn't find proper closing
        
        # All branches below share the outer cursor: whatever a clause
        # consumes must be visible to directives that follow the ~[...~].
        if colon_flag:
            # ~:[ test ~; else ~]
            val = get_arg()
            # T is truthy, NIL/False/None are falsy
            is_true = val is not None and val is not lisptype.NIL and val is not False
            # Also check for T symbol
            if val is lisptype.T:
                is_true = True
            if is_true:
                result = _format_process_cursor(clauses[1] if len(clauses) > 1 else '', cursor)
            else:
                result = _format_process_cursor(clauses[0] if clauses else '', cursor)
        elif at_flag:
            # ~@[ test ~] - if arg is non-nil, process with arg, else skip
            val = get_arg()
            if val is not None and val is not lisptype.NIL and val is not False:
                # Put the value back; the clause consumes it itself.
                cursor.idx -= 1
                result = _format_process_cursor(clauses[0] if clauses else '', cursor)
            else:
                result = ''
        else:
            # ~[ clause0 ~; clause1 ~; ... ~] - select by index.
            # CLHS 22.3.7.2: `~n[` (and so `~#[`, where the parameter is the
            # count of remaining arguments) takes the index from the prefix
            # parameter and consumes *no* argument. Unconditionally calling
            # get_arg() here both selected the wrong clause and stole an
            # argument from whatever followed.
            if params and params[0] is not None:
                idx = _lisp_number(params[0], -1)
            else:
                idx = _lisp_number(get_arg(), -1)
            if 0 <= idx < len(clauses):
                result = _format_process_cursor(clauses[idx], cursor)
            elif default_clause is not None and default_clause < len(clauses):
                result = _format_process_cursor(clauses[default_clause], cursor)
            else:
                result = ''

        return (result, end_pos)
    
    elif directive == ']':
        return ('', pos)
    
    elif directive == '{':
        # ~{ ... ~} - Iteration
        # Find matching ~} taking nesting into account
        nesting = 1
        i = pos
        end_inner = pos
        end_pos = pos
        while i < len(control_string) and nesting > 0:
            if control_string[i] == '~' and i + 1 < len(control_string):
                ch = control_string[i+1]
                if ch == '{':
                    nesting += 1
                    i += 2
                    continue
                elif ch == '}':
                    nesting -= 1
                    if nesting == 0:
                        end_inner = i
                        end_pos = i + 2  # position after ~}
                        break
                    i += 2
                    continue
            i += 1

        # Fallback if no proper closing found
        if nesting == 0:
            inner = control_string[pos:end_inner]
        else:
            inner = control_string[pos:i]
            end_pos = i

        # CLHS 22.3.7.4: an empty body means the control string to iterate
        # with is itself an argument, taken *before* the list argument.
        if not inner:
            inner_arg = get_arg()
            # Same coercion FORMAT itself applies to its control-string
            # argument (see format_fn), so a LispString behaves identically
            # whether it arrives literally or through ~{~}.
            inner = '' if inner_arg is None else str(inner_arg)

        if at_flag:
            # ~@{...~} - use the rest of the outer arguments as the items,
            # directly from the outer cursor: they belong to the same
            # argument stream, not a separate list argument.
            items = cursor.remaining()
            cursor.idx = len(cursor.args)
        else:
            # ~{...~} / ~:{...~} - the next argument is the list of items,
            # a scope of its own (per CLHS 22.3.7): only the single "list"
            # argument itself (already taken by get_arg()) is removed from
            # the outer cursor; what the iteration body does with its
            # elements never touches the outer cursor further.
            items = _format_args_list(get_arg())

        # ~n{...~} bounds the number of iterations (CLHS 22.3.7.4); without
        # a parameter the only bound is the argument list running out.
        max_iterations = params[0] if params and params[0] is not None else None

        result_parts = []
        iterations = 0

        if colon_flag:
            # ~:{...~} - each item is itself a list, and one pass is made per
            # item with that sublist standing in as the whole argument stream.
            for item in items:
                if max_iterations is not None and iterations >= max_iterations:
                    break
                iterations += 1
                sub_cursor = _FormatCursor(_format_args_list(item))
                try:
                    result_parts.append(_format_process_cursor(inner, sub_cursor))
                except _FormatEscape as esc:
                    # CLHS 22.3.9.2: inside ~:{...~}, a plain ~^ ends only
                    # the current sublist's pass, while ~:^ ends the whole
                    # iteration. Treating both as "stop everything" silently
                    # dropped every sublist after the first short one.
                    result_parts.append(esc.partial)
                    if esc.terminate_outer:
                        break
                    continue
        else:
            # One pass at a time over what is left; each pass gets a fresh
            # cursor, and however much it consumed is where the next starts.
            item_list = list(items)
            while item_list:
                if max_iterations is not None and iterations >= max_iterations:
                    break
                iterations += 1
                sub_cursor = _FormatCursor(item_list)
                try:
                    result_parts.append(_format_process_cursor(inner, sub_cursor))
                except _FormatEscape as esc:
                    result_parts.append(esc.partial)
                    break
                consumed = sub_cursor.idx
                if consumed <= 0:
                    # A body consuming nothing would iterate forever; advance
                    # by one so the loop stays bounded.
                    consumed = 1
                item_list = item_list[consumed:]

        return (''.join(result_parts), end_pos)
    
    elif directive == '}':
        return ('', pos)
    
    elif directive == '^':
        # ~^ - CLHS 22.3.9.2: terminate the enclosing ~{...~} iteration, or
        # the whole control string when not inside one. It is a control
        # transfer, not a character, so it raises rather than returning an
        # in-band marker for callers to string-replace out.
        #
        # Whether it fires depends on its prefix parameters:
        #   none    - terminate if no arguments remain
        #   n       - terminate if n is zero
        #   n,m     - terminate if n equals m
        #   n,m,p   - terminate if n <= m <= p
        supplied = [p for p in params if p is not None]
        if len(params) >= 3:
            n, m, p = params[0], params[1], params[2]
            should_escape = _lisp_number(n) <= _lisp_number(m) <= _lisp_number(p)
        elif len(supplied) == 2 or (len(params) == 2 and params[0] is not None):
            should_escape = _lisp_number(params[0]) == _lisp_number(params[1])
        elif len(params) == 1 and params[0] is not None:
            should_escape = _lisp_number(params[0]) == 0
        else:
            should_escape = cursor.remaining_count() <= 0

        if should_escape:
            # ~:^ terminates the iteration one level out (CLHS 22.3.9.2),
            # which is what ~:{...~} bodies use to stop the outer sweep.
            raise _FormatEscape(terminate_outer=colon_flag)
        return ('', pos)

    elif directive == '\n':
        # ~<newline> - Ignored newline
        if at_flag:
            # Keep the newline
            return ('\n', pos)
        else:
            # Ignore newline and following whitespace
            while pos < len(control_string) and control_string[pos] in ' \t':
                pos += 1
            return ('', pos)

    elif directive == 'P':
        # ~P - Plural. ~:P re-examines the previously consumed argument
        # without consuming a new one, so the cursor must not move at all
        # (net-zero) - unlike the old code, which shifted it by one extra.
        if colon_flag:
            val = cursor.prev()
        else:
            val = get_arg()
        try:
            num = int(val) if val is not None else 1
            if at_flag:
                result = 'y' if num == 1 else 'ies'
            else:
                result = '' if num == 1 else 's'
        except (TypeError, ValueError):
            result = 's'
        return (result, pos)

    else:
        # Unknown directive - just output the tilde and char
        return ('~' + directive, pos)


def _format_process_cursor(control_string, cursor):
    """Process a format control string, consuming arguments from `cursor`.

    This is the shared core: passing the *same* cursor into a nested call
    (used by ~<...~>, ~(...~), ~[...~], ~@?) makes consumption inside the
    nested directive visible to whatever follows it in the outer control
    string - the structural fix for FORMAT's argument-cursor model. Passing
    a *fresh* cursor (used per-item by ~{...~}, and by plain ~?) gives a
    nested control string its own independent argument scope, per CLHS.
    """
    result = []
    pos = 0
    while pos < len(control_string):
        c = control_string[pos]
        if c == '~':
            pos += 1
            try:
                output, pos = _format_directive(control_string, cursor, pos,
                                               emitted=result)
            except _FormatEscape as esc:
                # `~^` abandons the rest of *this* control string but keeps
                # what it already produced (CLHS 22.3.9.2). Each frame
                # prepends its own accumulated output as the escape unwinds,
                # so the text is assembled in the order it was generated.
                esc.partial = ''.join(result) + esc.partial
                raise
            result.append(output)
        else:
            result.append(c)
            pos += 1
    return ''.join(result)


def _format_process(control_string, args):
    """Process a format control string with arguments (fresh cursor)."""
    return _format_process_with_tail(control_string, args)[0]


def _format_process_with_tail(control_string, args):
    """Like _format_process but also return the number of arguments consumed
    (i.e. the index of the first remaining argument)."""
    cursor = _FormatCursor(args)
    try:
        result = _format_process_cursor(control_string, cursor)
    except _FormatEscape as esc:
        # A `~^` outside any iteration terminates the control string itself;
        # this is the outermost frame, so the escape stops here rather than
        # escaping FORMAT as a Python exception (standing rule 2).
        result = esc.partial
    return result, cursor.idx


@_registry.cl_function('FORMAT')
def format_fn(destination, control_string, *args):
    """Format output according to Common Lisp FORMAT directives.
    
    Args:
        destination: T for stdout, NIL for string, or stream
        control_string: Format control string with ~ directives
        *args: Arguments to format
    
    Returns:
        NIL if destination is T or stream, formatted string if NIL
    
    Supported directives:
        ~A    Aesthetic (princ-style)
        ~S    Standard (prin1-style)
        ~D    Decimal integer
        ~X    Hexadecimal
        ~O    Octal
        ~B    Binary
        ~R    Radix or English
        ~C    Character
        ~F    Fixed-format float
        ~E    Exponential float
        ~G    General float
        ~%    Newline
        ~&    Fresh line
        ~~    Literal tilde
        ~|    Page separator
        ~T    Tabulation
        ~*    Go to argument
        ~?    Recursive processing
        ~(~) Case conversion
        ~[~] Conditional
        ~{~} Iteration
        ~^    Escape from iteration
        ~P    Plural
    """
    if callable(control_string) and not isinstance(control_string, (str, lisptype.LispString)):
        # CLHS 22.3.1 / the "format control" glossary entry: control-string
        # is a designator for either a string or a function of (stream
        # &rest args) -- the latter is what FORMATTER returns. Call it
        # directly instead of falling into str(control_string) below, which
        # would hand FORMAT the function's Python repr ("<function ... at
        # 0x...>") to interpret as literal directive text.
        if destination is None or destination is lisptype.NIL:
            from .streams import make_string_output_stream as _make_sos, get_output_stream_string as _get_oss
            capture = _make_sos()
            control_string(capture, *args)
            return _get_oss(capture)
        elif destination is True or destination is lisptype.T:
            control_string(lisptype.T, *args)
            return lisptype.NIL
        else:
            control_string(destination, *args)
            return lisptype.NIL

    if control_string is None:
        control_string = ""
    elif not isinstance(control_string, str):
        control_string = str(control_string)

    formatted = _format_process(control_string, args)

    if destination is True or destination is lisptype.T:
        # FORMAT's `destination` is not a plain stream designator: `t` means
        # `*STANDARD-OUTPUT*` here (CLHS 22.3.1), whereas for a stream
        # designator `t` means `*TERMINAL-IO*` (CLHS 21.1.3). Printing to the
        # process's stdout instead meant `(format t ...)` escaped any
        # `(with-output-to-string (*standard-output*) ...)` around it.
        write_text(formatted, lisptype.NIL)
        return lisptype.NIL
    elif destination is None or destination is lisptype.NIL:
        return formatted
    else:
        write_text(formatted, destination)
        return lisptype.NIL


@_registry.cl_function('FORMATTER')
def formatter(control_string):
    """Create formatter function (CLHS 22.3.1: (FORMATTER control-string)).

    Returns a function of (stream &rest args) -- the function-valued half of
    the "format control" designator FORMAT and ERROR/WARN/CERROR datums also
    accept -- that formats args per control-string and writes the result to
    stream, returning the list of arguments it did not consume.
    """
    control_string_str = str(control_string)

    def format_func(stream, *args):
        # Use internal processor to obtain remaining-args index (tail)
        formatted, consumed = _format_process_with_tail(control_string_str, args)
        write_text(formatted, stream)
        # Return the tail (remaining args) as a proper Lisp list -- a bare
        # Python list here is a second, incompatible list representation
        # (finding M), so `(equal (funcall fn stream ... 'a) '(a))` was
        # comparing a `lispCons` against a Python list and always failing,
        # regardless of whether the tail's contents were otherwise correct.
        tail = lisptype.NIL
        for item in reversed(args[consumed:]):
            tail = lisptype.lispCons(item, tail)
        return tail

    return format_func


# NOTE: Pathname operations are defined in pathnames.py with proper Pathname class support
# Functions like PATHNAME, PATHNAMEP, PATHNAME-DIRECTORY, etc. are all in pathnames.py


# Stream operations
# NOTE: actual OPEN/CLOSE and stream operations are implemented in
# lispfunc/streams.py. The simplified stubs were removed to avoid
# clashing registrations that override the full implementations.


# File operations
# NOTE: PROBE-FILE is defined in pathnames.py and imported above


@_registry.cl_function('DELETE-FILE')
def delete_file(filespec):
    """Delete file."""
    import os
    # Resolve similar to LOAD/COMPILE-FILE so relative pathnames are found
    from fclpy.lispfunc.pathnames import Pathname
    import fclpy.state as state
    env = state.current_environment

    if isinstance(filespec, Pathname):
        path_str = filespec.original
    else:
        path_str = str(filespec)

    if not os.path.isabs(path_str):
        resolved = False
        lisp_cwd = os.environ.get('LISP_CWD')
        if lisp_cwd:
            candidate = os.path.normpath(os.path.join(lisp_cwd, path_str))
            if os.path.exists(candidate):
                path_str = candidate
                resolved = True

        if not resolved and env is not None:
            load_truename_sym = lisptype.COMMON_LISP_PACKAGE.intern_symbol('*LOAD-TRUENAME*')
            load_truename = env.find_variable(load_truename_sym)
            if load_truename and load_truename is not lisptype.NIL and isinstance(load_truename, Pathname):
                current_file_path = load_truename.original
                current_dir = os.path.dirname(current_file_path)
                if current_dir:
                    candidate = os.path.normpath(os.path.join(current_dir, path_str))
                    if os.path.exists(candidate):
                        path_str = candidate
                        resolved = True

        if not resolved and env is not None:
            default_sym = lisptype.COMMON_LISP_USER_PACKAGE.intern_symbol('*DEFAULT-PATHNAME-DEFAULTS*')
            default_pathname = env.find_variable(default_sym)
            if default_pathname and default_pathname is not lisptype.NIL and isinstance(default_pathname, Pathname):
                default_path = default_pathname.original
                if os.path.isdir(default_path):
                    default_dir = default_path
                else:
                    default_dir = os.path.dirname(default_path)
                if default_dir:
                    candidate = os.path.normpath(os.path.join(default_dir, path_str))
                    if os.path.exists(candidate):
                        path_str = candidate

    try:
        os.remove(path_str)
        return lisptype.T
    except FileNotFoundError:
        return lisptype.NIL


@_registry.cl_function('RENAME-FILE')
def rename_file(filespec, new_name):
    """Rename file."""
    import os
    os.rename(str(filespec), str(new_name))
    return str(new_name)


@_registry.cl_function('FILE-AUTHOR')
def file_author(pathspec):
    """Get file author."""
    return "unknown"  # Simplified


@_registry.cl_function('FILE-LENGTH')
def file_length(stream):
    """Get file length."""
    return 0  # Simplified


@_registry.cl_function('FILE-POSITION')
def file_position(stream, position=None):
    """Get or set file position."""
    if position is None:
        return 0  # Get position
    else:
        return position  # Set position


@_registry.cl_function('FILE-STRING-LENGTH')
def file_string_length(stream, string):
    """Length of string in file."""
    return len(string)


@_registry.cl_function('FILE-WRITE-DATE')
def file_write_date(pathspec):
    """Get file write date."""
    import os
    import time
    try:
        return int(os.path.getmtime(str(pathspec)))
    except:
        return 0


@_registry.cl_function('COMPILE-FILE')
def compile_file(input_file, output_file=None, **kwargs):
    """Compile file.
    
    In FCLpy, we don't actually compile to bytecode - we copy the source file
    to a .fasl file which will be interpreted when loaded. This allows FCLpy
    to work with Common Lisp build systems that expect compile-and-load workflows.
    
    Returns: MultipleValues(output-truename, warnings-p, failure-p)
      - output-truename: The pathname of the output file
      - warnings-p: NIL (no warnings)
      - failure-p: NIL (no failure)
    """
    import os
    import shutil
    from fclpy.lispfunc.pathnames import Pathname
    
    # Get the input path (resolve relative names similarly to LOAD)
    import fclpy.state as state
    env = state.current_environment

    if isinstance(input_file, Pathname):
        input_path = input_file.original
    else:
        input_path = str(input_file)

    # If input_path is not absolute, try to resolve it using LISP_CWD,
    # *LOAD-TRUENAME* directory, or *DEFAULT-PATHNAME-DEFAULTS* (like LOAD)
    import os
    if not os.path.isabs(input_path):
        resolved = False
        lisp_cwd = os.environ.get('LISP_CWD')
        if lisp_cwd:
            candidate = os.path.normpath(os.path.join(lisp_cwd, input_path))
            if os.path.exists(candidate):
                input_path = candidate
                resolved = True

        if not resolved and env is not None:
            # Try *LOAD-TRUENAME*
            load_truename_sym = lisptype.COMMON_LISP_PACKAGE.intern_symbol('*LOAD-TRUENAME*')
            load_truename = env.find_variable(load_truename_sym)
            try:
                from fclpy.lispfunc.pathnames import Pathname as PN
            except Exception:
                PN = None
            if load_truename and load_truename is not lisptype.NIL and PN is not None and isinstance(load_truename, PN):
                current_file_path = load_truename.original
                current_dir = os.path.dirname(current_file_path)
                if current_dir:
                    candidate = os.path.normpath(os.path.join(current_dir, input_path))
                    if os.path.exists(candidate):
                        input_path = candidate
                        resolved = True

        if not resolved and env is not None:
            default_sym = lisptype.COMMON_LISP_PACKAGE.intern_symbol('*DEFAULT-PATHNAME-DEFAULTS*')
            default_pathname = env.find_variable(default_sym)
            if default_pathname and default_pathname is not lisptype.NIL and PN is not None and isinstance(default_pathname, PN):
                default_path = default_pathname.original
                if os.path.isdir(default_path):
                    default_dir = default_path
                else:
                    default_dir = os.path.dirname(default_path)
                if default_dir:
                    candidate = os.path.normpath(os.path.join(default_dir, input_path))
                    if os.path.exists(candidate):
                        input_path = candidate
                        resolved = True
    
    # Determine output path
    if output_file is not None:
        if isinstance(output_file, Pathname):
            out_path = output_file.original
        else:
            out_path = str(output_file)
    else:
        # Default: replace extension with .fasl
        base = os.path.splitext(input_path)[0]
        out_path = base + ".fasl"
    
    # "Compile" by copying the source file to the output path
    # This allows LOAD to find and interpret the .fasl file
    try:
        if os.path.exists(input_path):
            shutil.copy2(input_path, out_path)
            output_pathname = Pathname(out_path)
            return lisptype.MultipleValues(output_pathname, lisptype.NIL, lisptype.NIL)
        else:
            # File doesn't exist - return failure
            return lisptype.MultipleValues(lisptype.NIL, lisptype.NIL, lisptype.T)
    except Exception as e:
        # Compilation failed
        return lisptype.MultipleValues(lisptype.NIL, lisptype.NIL, lisptype.T)


@_registry.cl_function('COMPILE-FILE-PATHNAME')
def compile_file_pathname(input_file, output_file=None, **kwargs):
    """Get compiled file pathname.
    
    Returns the pathname that COMPILE-FILE would produce for the given input file.
    Returns a .fasl extension version of the input file. The load function
    will handle loading the source if the .fasl doesn't exist.
    """
    from fclpy.lispfunc.pathnames import Pathname
    import os
    
    # Resolve input path similar to compile_file so pathname reflects real location
    import fclpy.state as state
    env = state.current_environment

    if isinstance(input_file, Pathname):
        input_str = input_file.original
    else:
        input_str = str(input_file)

    import os
    if not os.path.isabs(input_str):
        # Try LISP_CWD
        lisp_cwd = os.environ.get('LISP_CWD')
        if lisp_cwd:
            candidate = os.path.normpath(os.path.join(lisp_cwd, input_str))
            if os.path.exists(candidate):
                input_str = candidate

        if env is not None:
            load_truename_sym = lisptype.COMMON_LISP_PACKAGE.intern_symbol('*LOAD-TRUENAME*')
            load_truename = env.find_variable(load_truename_sym)
            try:
                from fclpy.lispfunc.pathnames import Pathname as PN
            except Exception:
                PN = None
            if load_truename and load_truename is not lisptype.NIL and PN is not None and isinstance(load_truename, PN):
                current_file_path = load_truename.original
                current_dir = os.path.dirname(current_file_path)
                if current_dir:
                    candidate = os.path.normpath(os.path.join(current_dir, input_str))
                    if os.path.exists(candidate):
                        input_str = candidate

        if env is not None:
            try:
                from fclpy.lispfunc.pathnames import Pathname as PN
            except Exception:
                PN = None
            default_sym = lisptype.COMMON_LISP_USER_PACKAGE.intern_symbol('*DEFAULT-PATHNAME-DEFAULTS*')
            default_pathname = env.find_variable(default_sym)
            if default_pathname and default_pathname is not lisptype.NIL and PN is not None and isinstance(default_pathname, PN):
                default_path = default_pathname.original
                if os.path.isdir(default_path):
                    default_dir = default_path
                else:
                    default_dir = os.path.dirname(default_path)
                if default_dir:
                    candidate = os.path.normpath(os.path.join(default_dir, input_str))
                    if os.path.exists(candidate):
                        input_str = candidate

    base = os.path.splitext(input_str)[0]
    result = base + ".fasl"
    return Pathname(result)


# Condition operations
@_registry.cl_function('SIMPLE-CONDITION-FORMAT-ARGUMENTS')
def simple_condition_format_arguments(condition):
    """Get the format-arguments slot of a simple-condition (CLHS 9.2).

    Previously a stub that always returned () regardless of what the
    condition actually stored, so any simple-condition/simple-error/
    simple-warning signaled with format arguments (e.g. (error "~A" 10))
    lost them the moment a handler tried to read them back via this
    accessor -- FORMAT would then be called with no arguments at all.
    """
    if isinstance(condition, lisptype.Condition):
        return list(condition.get_slot('format-arguments') or [])
    return []


@_registry.cl_function('SIMPLE-CONDITION-FORMAT-CONTROL')
def simple_condition_format_control(condition):
    """Get the format-control slot of a simple-condition (CLHS 9.2).

    Previously a stub that returned str(condition) -- the condition's
    *report message*, not its format-control slot -- so this only
    happened to work when format-control was a plain string with no
    arguments and the message hadn't diverged from it; a function-valued
    format-control (FORMATTER's result) or one with format arguments was
    silently discarded.
    """
    if isinstance(condition, lisptype.Condition):
        return condition.get_slot('format-control')
    return str(condition)


def end_of_file():
    """End of file condition."""
    return EOFError()


def file_error():
    """File error condition."""
    return FileNotFoundError()


def file_error_pathname(condition):
    """Get pathname from file error."""
    return str(condition)  # Simplified


# Error handling
@_registry.cl_function('ERROR')
def error(format_control, *args):
    """Signal error."""
    msg = format_control.format(*args) if args else str(format_control)
    raise Exception(msg)


# Interactive I/O
def y_or_n_p(control_string=None, *args):
    """Ask yes/no question."""
    if control_string:
        print(control_string.format(*args), end=' ')
    response = input("(y or n) ").strip().lower()
    return lisptype.lisp_bool(response in ('y', 'yes'))


def yes_or_no_p(control_string=None, *args):
    """Ask yes/no question with full words."""
    if control_string:
        print(control_string.format(*args), end=' ')
    response = input("(yes or no) ").strip().lower()
    return lisptype.lisp_bool(response == 'yes')


# WITH- macros (simplified implementations)
def with_open_file(var_filespec_options, *body):
    """Execute with open file."""
    # Simplified - just execute body
    result = None
    for form in body:
        result = form
    return result


# NOTE: the real macro expander lives in evaluation_special_forms.py.
# This module-level stub neither evaluated its body nor created a stream,
# and register_module() would auto-register it as a *function* (its Python
# name differs from the expander's, so the decorator dedup misses it),
# clobbering the macro depending on import order -- standing rule 3.


# NOTE: the real macro expander lives in evaluation_special_forms.py.
# This module-level stub neither evaluated its body nor created a stream,
# and register_module() would auto-register it as a *function* (its Python
# name differs from the expander's, so the decorator dedup misses it),
# clobbering the macro depending on import order -- standing rule 3.


__all__ = [
    # Stream predicates and control
    'clear_output', 'output_stream_p', 'open_stream_p',
    # Write operations
    'write_char', 'write_string', 'write_line', 'write_byte', 'write',
    'prin1_to_string', 'princ_to_string', 'write_to_string',
    'print_fn', 'prin1', 'princ', 'terpri', 'fresh_line',
    'finish_output', 'force_output',
    # Stream creation
    'make_string_output_stream', 'get_output_stream_string',
    'make_broadcast_stream', 'make_concatenated_stream',
    'make_echo_stream', 'make_synonym_stream', 'make_two_way_stream',
    # Pretty printing
    'copy_pprint_dispatch', 'pprint', 'pprint_dispatch',
    'pprint_exit_if_list_exhausted', 'pprint_indent', 'pprint_linear',
    'pprint_logical_block', 'pprint_newline', 'pprint_pop', 'pprint_tab',
    'pprint_tabular', 'pprint_fill', 'set_pprint_dispatch',
    # Format operations
    'format_fn', 'formatter',
    # Pathname operations
    'pathname', 'pathnamep', 'pathname_host', 'pathname_device',
    'pathname_directory', 'pathname_name', 'pathname_type',
    'pathname_version', 'make_pathname', 'namestring',
    'directory_namestring', 'host_namestring', 'file_namestring',
    'enough_namestring', 'parse_namestring', 'merge_pathnames',
    'wild_pathname_p', 'pathname_match_p', 'translate_pathname',
    'logical_pathname', 'translate_logical_pathname', 'truename',
    # File/Stream operations
    'open_fn', 'close_fn', 'stream_element_type', 'stream_external_format',
    # File operations
    'probe_file', 'delete_file', 'rename_file', 'file_author',
    'file_length', 'file_position', 'file_string_length',
    'file_write_date', 'compile_file', 'compile_file_pathname',
    # Condition operations
    'simple_condition_format_arguments', 'simple_condition_format_control',
    'end_of_file', 'file_error', 'file_error_pathname',
    # Error handling
    'error',
    # Interactive I/O
    'y_or_n_p', 'yes_or_no_p',
    # WITH- macros
    'with_open_file',]
