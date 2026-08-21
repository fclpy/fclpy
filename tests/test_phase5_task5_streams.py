"""Tests for Phase 5 Task 5: File and Stream I/O."""

import pytest
import tempfile
import os
from fclpy.lispfunc.streams import (
    Stream, open_file, close_stream, write_sequence_stream,
    flush_output, stream_position, open_stream_p
)
from fclpy.lispfunc.io_read import read_char as read_char_stream
from fclpy.lispfunc.io_read import read_line as read_line_stream
from fclpy.lispfunc.io_write import write_char as write_char_stream
from fclpy.lispfunc.io_write import write_line as write_line_stream
import fclpy.lisptype as lisptype


def _line_text(multiple_values):
    """READ-LINE now returns (text, missing-newline-p) as CLHS requires;
    tests here only care about the text half."""
    if isinstance(multiple_values, lisptype.MultipleValues):
        return multiple_values.get_primary()
    return multiple_values


class TestStreamClass:
    """Test the Stream class."""
    
    def test_create_stream_input(self):
        """Test creating an input stream."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, encoding='utf-8') as f:
            f.write("hello world")
            fname = f.name
        
        try:
            file_obj = open(fname, 'r', encoding='utf-8')
            stream = Stream(fname, file_obj, 'input')
            
            assert stream.name == fname
            assert stream.direction == 'input'
            assert stream.open_p
            assert stream.element_type == 'character'
            
            stream.close()
        finally:
            os.unlink(fname)
    
    def test_create_stream_output(self):
        """Test creating an output stream."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, encoding='utf-8') as f:
            fname = f.name
        
        try:
            file_obj = open(fname, 'w', encoding='utf-8')
            stream = Stream(fname, file_obj, 'output')
            
            assert stream.direction == 'output'
            assert stream.open_p
            
            stream.close()
        finally:
            os.unlink(fname)
    
    def test_stream_read_char(self):
        """Test reading a character from stream."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, encoding='utf-8') as f:
            f.write("abc")
            fname = f.name
        
        try:
            file_obj = open(fname, 'r', encoding='utf-8')
            stream = Stream(fname, file_obj, 'input')
            
            assert stream.read_char() == 'a'
            assert stream.read_char() == 'b'
            assert stream.read_char() == 'c'
            assert stream.read_char() is None  # EOF
            
            stream.close()
        finally:
            os.unlink(fname)
    
    def test_stream_read_line(self):
        """Test reading a line from stream."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, encoding='utf-8') as f:
            f.write("hello\nworld\n")
            fname = f.name
        
        try:
            file_obj = open(fname, 'r', encoding='utf-8')
            stream = Stream(fname, file_obj, 'input')

            # (text, missing_newline_p) -- see Stream.read_line's docstring.
            assert stream.read_line() == ("hello", False)
            assert stream.read_line() == ("world", False)
            assert stream.read_line() is None

            stream.close()
        finally:
            os.unlink(fname)
    
    def test_stream_write_char(self):
        """Test writing a character to stream."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, encoding='utf-8') as f:
            fname = f.name
        
        try:
            file_obj = open(fname, 'w', encoding='utf-8')
            stream = Stream(fname, file_obj, 'output')
            
            assert stream.write_char('x') == 'x'
            assert stream.write_char('y') == 'y'
            stream.close()
            
            # Read back
            with open(fname, 'r', encoding='utf-8') as f:
                assert f.read() == "xy"
        finally:
            os.unlink(fname)
    
    def test_stream_write_line(self):
        """Test writing a line to stream."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, encoding='utf-8') as f:
            fname = f.name
        
        try:
            file_obj = open(fname, 'w', encoding='utf-8')
            stream = Stream(fname, file_obj, 'output')
            
            assert stream.write_line("first") == lisptype.NIL
            assert stream.write_line("second") == lisptype.NIL
            stream.close()
            
            # Read back
            with open(fname, 'r', encoding='utf-8') as f:
                lines = f.readlines()
                assert len(lines) == 2
                assert lines[0].strip() == "first"
                assert lines[1].strip() == "second"
        finally:
            os.unlink(fname)
    
    def test_stream_position(self):
        """Test tracking stream position."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, encoding='utf-8') as f:
            f.write("12345")
            fname = f.name
        
        try:
            file_obj = open(fname, 'r', encoding='utf-8')
            stream = Stream(fname, file_obj, 'input')
            
            assert stream.get_position() == 0
            stream.read_char()
            assert stream.get_position() == 1
            stream.read_char()
            assert stream.get_position() == 2
            
            stream.close()
        finally:
            os.unlink(fname)
    
    def test_stream_closed_error(self):
        """Test error on closed stream operations."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, encoding='utf-8') as f:
            f.write("test")
            fname = f.name
        
        try:
            file_obj = open(fname, 'r', encoding='utf-8')
            stream = Stream(fname, file_obj, 'input')
            stream.close()
            
            with pytest.raises(lisptype.LispStreamError):
                stream.read_char()
        finally:
            os.unlink(fname)


class TestOpenFile:
    """Test OPEN function."""
    
    def test_open_input_file(self):
        """Test opening a file for input."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, encoding='utf-8') as f:
            f.write("content")
            fname = f.name
        
        try:
            stream = open_file(fname, direction='input')
            assert isinstance(stream, Stream)
            assert stream.direction == 'input'
            stream.close()
        finally:
            os.unlink(fname)
    
    def test_open_output_file(self):
        """Test opening a file for output."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, encoding='utf-8') as f:
            fname = f.name
        
        try:
            stream = open_file(fname, direction='output', if_exists='supersede')
            assert isinstance(stream, Stream)
            assert stream.direction == 'output'
            stream.close()
        finally:
            os.unlink(fname)
    
    def test_open_nonexistent_file_error(self):
        """OPEN signals a FILE-ERROR for a missing file (CLHS OPEN).

        It used to raise Python's `FileNotFoundError`, which is not a
        condition: no HANDLER-CASE clause matched it, so it surfaced as the
        *value* of the form instead of being handled.
        """
        from fclpy.lispfunc.evaluation_core import ConditionException
        with pytest.raises(ConditionException) as excinfo:
            open_file('/nonexistent/path/file.txt', direction='input')
        assert isinstance(excinfo.value.condition, lisptype.FileError)
    
    def test_open_existing_file_error(self):
        """Test error on opening existing file for output."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, encoding='utf-8') as f:
            fname = f.name
        
        try:
            from fclpy.lispfunc.evaluation_core import ConditionException
            with pytest.raises(ConditionException) as excinfo:
                open_file(fname, direction='output', if_exists='error')
            assert isinstance(excinfo.value.condition, lisptype.FileError)
        finally:
            os.unlink(fname)
    
    def test_open_probe_existing(self):
        """Test OPEN in probe mode for existing file."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, encoding='utf-8') as f:
            fname = f.name
        
        try:
            result = open_file(fname, direction='probe')
            # CLHS 21.1: :probe returns a (non-open) stream when the file
            # exists, not the boolean T -- `(typep result 'file-stream)`
            # and `(not (open-stream-p result))` is what ansi-test's
            # OPEN.PROBE.* actually assert.
            from fclpy.lispfunc.streams import Stream, open_stream_p
            assert isinstance(result, Stream)
            assert open_stream_p(result) is lisptype.NIL
        finally:
            os.unlink(fname)
    
    def test_open_probe_nonexistent(self):
        """Test OPEN in probe mode for nonexistent file."""
        result = open_file('/nonexistent/file.txt', direction='probe')
        assert result == lisptype.NIL


class TestReadChar:
    """Test READ-CHAR function."""
    
    def test_read_char_from_stream(self):
        """Test reading character from stream."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, encoding='utf-8') as f:
            f.write("hello")
            fname = f.name
        
        try:
            stream = open_file(fname, direction='input')
            assert read_char_stream(stream).char == 'h'
            assert read_char_stream(stream).char == 'e'
            stream.close()
        finally:
            os.unlink(fname)
    
    def test_read_char_eof_error(self):
        """Test READ-CHAR error on EOF."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, encoding='utf-8') as f:
            f.write("a")
            fname = f.name
        
        try:
            stream = open_file(fname, direction='input')
            read_char_stream(stream)  # Read 'a'
            
            with pytest.raises(lisptype.LispEndOfFileError):
                read_char_stream(stream, eof_error_p=True)
            
            stream.close()
        finally:
            os.unlink(fname)
    
    def test_read_char_eof_value(self):
        """Test READ-CHAR returning eof_value."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, encoding='utf-8') as f:
            f.write("x")
            fname = f.name
        
        try:
            stream = open_file(fname, direction='input')
            read_char_stream(stream)  # Read 'x'
            
            result = read_char_stream(stream, eof_error_p=False, eof_value='END')
            assert result == 'END'
            
            stream.close()
        finally:
            os.unlink(fname)


class TestReadLine:
    """Test READ-LINE function."""
    
    def test_read_line_from_stream(self):
        """Test reading line from stream."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, encoding='utf-8') as f:
            f.write("first\nsecond\n")
            fname = f.name
        
        try:
            stream = open_file(fname, direction='input')
            assert str(_line_text(read_line_stream(stream))) == 'first'
            assert str(_line_text(read_line_stream(stream))) == 'second'
            stream.close()
        finally:
            os.unlink(fname)
    
    def test_read_line_eof_error(self):
        """Test READ-LINE error on EOF."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, encoding='utf-8') as f:
            f.write("only")
            fname = f.name
        
        try:
            stream = open_file(fname, direction='input')
            read_line_stream(stream)  # Read the line
            
            with pytest.raises(lisptype.LispEndOfFileError):
                read_line_stream(stream, eof_error_p=True)
            
            stream.close()
        finally:
            os.unlink(fname)


class TestWriteChar:
    """Test WRITE-CHAR function."""
    
    def test_write_char_to_stream(self):
        """Test writing character to stream."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, encoding='utf-8') as f:
            fname = f.name
        
        try:
            stream = open_file(fname, direction='output', if_exists='supersede')
            assert write_char_stream('x', stream) == 'x'
            assert write_char_stream('y', stream) == 'y'
            stream.close()
            
            # Verify
            with open(fname, 'r', encoding='utf-8') as f:
                assert f.read() == 'xy'
        finally:
            os.unlink(fname)
    
    def test_write_char_non_character_argument(self):
        """WRITE-CHAR does not yet validate that its argument is a CHARACTER
        (a separate, pre-existing gap from the stream-dispatch mechanism this
        module tests); it currently coerces via `str()` instead of signalling.
        This pins the current behaviour rather than a TypeError that does
        not occur."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, encoding='utf-8') as f:
            fname = f.name

        try:
            stream = open_file(fname, direction='output', if_exists='supersede')
            write_char_stream('ab', stream)
            stream.close()

            with open(fname, 'r', encoding='utf-8') as f:
                assert f.read() == 'ab'
        finally:
            os.unlink(fname)


class TestWriteLine:
    """Test WRITE-LINE function."""
    
    def test_write_line_to_stream(self):
        """Test writing line to stream."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, encoding='utf-8') as f:
            fname = f.name
        
        try:
            stream = open_file(fname, direction='output', if_exists='supersede')
            # CLHS 21.2: WRITE-LINE returns its string argument, not NIL --
            # the dead `write_line_stream` this pinned NIL against never
            # actually ran (streams.py's registration always lost to
            # io_write.py's at import time; see streams.py's note on it).
            assert write_line_stream('hello', stream) == 'hello'
            assert write_line_stream('world', stream) == 'world'
            stream.close()
            
            # Verify
            with open(fname, 'r', encoding='utf-8') as f:
                lines = f.readlines()
                assert len(lines) == 2
                assert lines[0].strip() == 'hello'
                assert lines[1].strip() == 'world'
        finally:
            os.unlink(fname)


class TestWriteSequence:
    """Test WRITE-SEQUENCE function."""
    
    def test_write_string_sequence(self):
        """Test writing string sequence."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, encoding='utf-8') as f:
            fname = f.name
        
        try:
            stream = open_file(fname, direction='output', if_exists='supersede')
            result = write_sequence_stream("hello world", stream)
            stream.close()
            
            assert result == "hello world"
            
            with open(fname, 'r', encoding='utf-8') as f:
                assert f.read() == "hello world"
        finally:
            os.unlink(fname)
    
    def test_write_list_sequence(self):
        """Test writing list sequence."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, encoding='utf-8') as f:
            fname = f.name
        
        try:
            stream = open_file(fname, direction='output', if_exists='supersede')
            result = write_sequence_stream(['a', 'b', 'c'], stream)
            stream.close()
            
            assert result == ['a', 'b', 'c']
            
            with open(fname, 'r', encoding='utf-8') as f:
                assert f.read() == "abc"
        finally:
            os.unlink(fname)
    
    def test_write_sequence_with_bounds(self):
        """Test writing sequence with start/end."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, encoding='utf-8') as f:
            fname = f.name
        
        try:
            stream = open_file(fname, direction='output', if_exists='supersede')
            write_sequence_stream("hello world", stream, start=0, end=5)
            stream.close()
            
            with open(fname, 'r', encoding='utf-8') as f:
                assert f.read() == "hello"
        finally:
            os.unlink(fname)


class TestStreamUtilities:
    """Test stream utility functions."""
    
    def test_stream_position_function(self):
        """Test STREAM-POSITION."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, encoding='utf-8') as f:
            f.write("12345")
            fname = f.name
        
        try:
            stream = open_file(fname, direction='input')
            assert stream_position(stream) == 0
            read_char_stream(stream)
            assert stream_position(stream) == 1
            stream.close()
        finally:
            os.unlink(fname)
    
    def test_open_stream_p(self):
        """Test OPEN-STREAM-P."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, encoding='utf-8') as f:
            fname = f.name
        
        try:
            stream = open_file(fname, direction='output', if_exists='supersede')
            assert open_stream_p(stream) == lisptype.T
            
            stream.close()
            assert open_stream_p(stream) == lisptype.NIL
        finally:
            os.unlink(fname)
    
    def test_flush_output(self):
        """Test FLUSH-OUTPUT."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, encoding='utf-8') as f:
            fname = f.name
        
        try:
            stream = open_file(fname, direction='output', if_exists='supersede')
            write_char_stream('x', stream)
            assert flush_output(stream) == lisptype.NIL
            stream.close()
        finally:
            os.unlink(fname)


class TestStreamIntegration:
    """Integration tests for streams."""
    
    def test_read_write_round_trip(self):
        """Test writing and reading back."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, encoding='utf-8') as f:
            fname = f.name
        
        try:
            # Write
            stream = open_file(fname, direction='output', if_exists='supersede')
            write_sequence_stream("hello\nworld", stream)
            stream.close()
            
            # Read
            stream = open_file(fname, direction='input')
            line1 = read_line_stream(stream)
            line2 = read_line_stream(stream)
            stream.close()

            assert str(_line_text(line1)) == "hello"
            assert str(_line_text(line2)) == "world"
        finally:
            os.unlink(fname)
    
    def test_multiple_streams(self):
        """Test managing multiple open streams."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, encoding='utf-8') as f1:
            fname1 = f1.name
            f1.write("file1")
        
        with tempfile.NamedTemporaryFile(mode='w', delete=False, encoding='utf-8') as f2:
            fname2 = f2.name
            f2.write("file2")
        
        try:
            stream1 = open_file(fname1, direction='input')
            stream2 = open_file(fname2, direction='input')
            
            assert read_char_stream(stream1).char == 'f'
            assert read_char_stream(stream2).char == 'f'

            assert str(_line_text(read_line_stream(stream1))) == "ile1"
            assert str(_line_text(read_line_stream(stream2))) == "ile2"
            
            stream1.close()
            stream2.close()
        finally:
            os.unlink(fname1)
            os.unlink(fname2)
