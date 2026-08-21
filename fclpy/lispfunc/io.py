"""I/O functions - input/output, streams, files, and printing.

This module re-exports I/O functions from specialized submodules:
- io_read: Input operations, character reading, readtables
- io_write: Output operations, printing, pathnames, file operations
"""

from .io_read import *
from .io_write import *
from .streams import (
    open_file, close_stream,
    make_broadcast_stream, make_concatenated_stream, make_echo_stream,
    make_synonym_stream, make_two_way_stream,
)

# Add special symbol-safe name for PRINT (expected by __init__)
_s_print_ = print_fn

# Comprehensive exports for backward compatibility
__all__ = [
    # From io_read
    'readtablep', 'streamp', 'input_stream_p', 'interactive_stream_p',
    'read_line', 'read_char', 'read_byte', 'peek_char', 'unread_char',
    'listen', 'clear_input',
    'read', 'read_char_no_hang', 'read_delimited_list',
    'read_from_string', 'read_preserving_whitespace',
    'make_string_input_stream',
    'copy_readtable', 'readtable_case', 'set_readtable_case',
    'get_macro_character', 'set_macro_character',
    'get_dispatch_macro_character', 'set_dispatch_macro_character',
    'make_dispatch_macro_character', 'set_syntax_from_char',
    # From io_write
    'clear_output', 'output_stream_p', 'open_stream_p',
    'write_char', 'write_string', 'write_line', 'write_byte', 'write',
    'print_fn', 'prin1', 'princ', 'terpri', 'fresh_line',
    'finish_output', 'force_output',
    'prin1_to_string', 'princ_to_string', 'write_to_string',
    'make_string_output_stream', 'get_output_stream_string',
    'make_broadcast_stream', 'make_concatenated_stream',
    'make_echo_stream', 'make_synonym_stream', 'make_two_way_stream',
    'pprint', 'pprint_dispatch', 'pprint_newline',
    'pprint_fill', 'pprint_exit_if_list_exhausted', 'pprint_indent',
    'pprint_linear', 'pprint_pop', 'pprint_tab', 'pprint_tabular',
    'set_pprint_dispatch', 'copy_pprint_dispatch',
    'format_fn', 'formatter',
    'pathname', 'pathnamep', 'pathname_host', 'pathname_device',
    'pathname_directory', 'pathname_name', 'pathname_type', 'pathname_version',
    'make_pathname_function', 'namestring', 'directory_namestring', 'host_namestring',
    'file_namestring', 'enough_namestring', 'parse_namestring',
    'merge_pathnames', 'wild_pathname_p', 'pathname_match_p',
    'translate_pathname', 'logical_pathname', 'translate_logical_pathname',
    'truename',
    'open_fn', 'close_fn', 'probe_file', 'delete_file', 'rename_file',
    'file_author', 'file_length', 'file_position', 'file_string_length',
    'file_write_date',
    'stream_element_type', 'stream_external_format',
    'simple_condition_format_arguments', 'simple_condition_format_control',
    'end_of_file', 'file_error', 'file_error_pathname',
    'error', 'y_or_n_p', 'yes_or_no_p',
    'with_open_file',    # Symbol-safe names for operators
    '_s_print_',
]

# Alias the concrete implementations to the legacy exported names expected
# by other modules.
open_fn = open_file
close_fn = close_stream
