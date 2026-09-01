"""Tests for Stream enhancements (Phase 8 Task 8)."""

import pytest
from fclpy.lispfunc.streams import (
    StringInputStream, StringOutputStream,
    make_string_input_stream, make_string_output_stream,
    get_output_stream_string, string_input_stream_p, string_output_stream_p
)
from fclpy.lisptype import T, NIL, Character


class TestStringInputStream:
    """Test StringInputStream class."""
    
    def test_create_from_string(self):
        """Create input stream from string."""
        stream = StringInputStream("hello")
        assert isinstance(stream, StringInputStream)
        assert stream.string == "hello"
        assert stream.position == 0
    
    def test_create_with_start_end(self):
        """Create input stream with start/end indices."""
        stream = StringInputStream("hello world", 0, 5)
        assert stream.string == "hello"
        
        stream = StringInputStream("hello world", 6, 11)
        assert stream.string == "world"
    
    def test_read_char(self):
        """Read characters one at a time."""
        stream = StringInputStream("abc")
        assert stream.read_char() == 'a'
        assert stream.read_char() == 'b'
        assert stream.read_char() == 'c'
        assert stream.read_char() is None  # EOF
    
    def test_peek_char(self):
        """Peek at next character without consuming."""
        stream = StringInputStream("abc")
        assert stream.peek_char() == 'a'
        assert stream.peek_char() == 'a'  # Still 'a'
        stream.read_char()
        assert stream.peek_char() == 'b'
    
    def test_unread_char(self):
        """Unread a character."""
        stream = StringInputStream("abc")
        stream.read_char()  # 'a'
        stream.unread_char('a')
        assert stream.read_char() == 'a'
    
    def test_listen(self):
        """Check if characters available."""
        stream = StringInputStream("ab")
        assert stream.listen() == True
        stream.read_char()
        assert stream.listen() == True
        stream.read_char()
        assert stream.listen() == False


class TestStringOutputStream:
    """Test StringOutputStream class."""
    
    def test_create(self):
        """Create output stream."""
        stream = StringOutputStream()
        assert isinstance(stream, StringOutputStream)
        assert stream.position == 0
    
    def test_write_char(self):
        """Write characters one at a time."""
        stream = StringOutputStream()
        stream.write_char('H')
        stream.write_char('i')
        assert stream.peek_string() == "Hi"
    
    def test_write_sequence(self):
        """Write string sequence."""
        stream = StringOutputStream()
        stream.write_sequence("Hello")
        stream.write_sequence(" World")
        assert stream.peek_string() == "Hello World"
    
    def test_get_string_clears_buffer(self):
        """get_string returns content and clears buffer."""
        stream = StringOutputStream()
        stream.write_sequence("Hello")
        result = stream.get_string()
        assert result == "Hello"
        assert stream.peek_string() == ""
        assert stream.position == 0
    
    def test_peek_string_keeps_buffer(self):
        """peek_string returns content without clearing."""
        stream = StringOutputStream()
        stream.write_sequence("Hello")
        assert stream.peek_string() == "Hello"
        assert stream.peek_string() == "Hello"  # Still there


class TestMakeStringInputStream:
    """Test MAKE-STRING-INPUT-STREAM function."""
    
    def test_basic(self):
        """Create input stream with function."""
        stream = make_string_input_stream("hello")
        assert isinstance(stream, StringInputStream)
        assert stream.read_char() == 'h'
    
    def test_with_start_end(self):
        """Create with start and end."""
        stream = make_string_input_stream("hello world", 6, 11)
        assert stream.read_char() == 'w'
        assert stream.read_char() == 'o'


class TestMakeStringOutputStream:
    """Test MAKE-STRING-OUTPUT-STREAM function."""
    
    def test_basic(self):
        """Create output stream with function."""
        stream = make_string_output_stream()
        assert isinstance(stream, StringOutputStream)
    
    def test_write_and_get(self):
        """Write to stream and get result."""
        stream = make_string_output_stream()
        stream.write_sequence("Hello")
        result = get_output_stream_string(stream)
        assert result == "Hello"


class TestGetOutputStreamString:
    """Test GET-OUTPUT-STREAM-STRING function."""
    
    def test_with_string_output_stream(self):
        """Get string from StringOutputStream."""
        stream = make_string_output_stream()
        stream.write_sequence("Test")
        assert get_output_stream_string(stream) == "Test"
    
    def test_clears_buffer(self):
        """Getting string clears the buffer."""
        stream = make_string_output_stream()
        stream.write_sequence("First")
        get_output_stream_string(stream)
        stream.write_sequence("Second")
        assert get_output_stream_string(stream) == "Second"
    
    def test_empty_stream(self):
        """Empty stream returns empty string."""
        stream = make_string_output_stream()
        assert get_output_stream_string(stream) == ""


class TestStreamPredicates:
    """Test stream type predicates."""
    
    def test_string_input_stream_p(self):
        """Test STRING-INPUT-STREAM-P."""
        input_stream = make_string_input_stream("hello")
        output_stream = make_string_output_stream()
        
        assert string_input_stream_p(input_stream) == T
        assert string_input_stream_p(output_stream) == NIL
        assert string_input_stream_p("hello") == NIL
    
    def test_string_output_stream_p(self):
        """Test STRING-OUTPUT-STREAM-P."""
        input_stream = make_string_input_stream("hello")
        output_stream = make_string_output_stream()
        
        assert string_output_stream_p(output_stream) == T
        assert string_output_stream_p(input_stream) == NIL
        assert string_output_stream_p("hello") == NIL


class TestStreamIntegration:
    """Test stream integration scenarios."""
    
    def test_read_entire_string(self):
        """Read entire string character by character."""
        stream = make_string_input_stream("hello")
        result = []
        while True:
            char = stream.read_char()
            if char is None:
                break
            result.append(char)
        assert ''.join(result) == "hello"
    
    def test_write_multiple_types(self):
        """Write various types to output stream."""
        stream = make_string_output_stream()
        stream.write_char('A')
        stream.write_sequence("BC")
        stream.write_char('D')
        assert get_output_stream_string(stream) == "ABCD"
    
    def test_stream_repr(self):
        """Test string representation of streams."""
        input_stream = make_string_input_stream("hello")
        output_stream = make_string_output_stream()
        
        assert "<StringInputStream" in repr(input_stream)
        assert "<StringOutputStream" in repr(output_stream)
