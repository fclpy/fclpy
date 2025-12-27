"""Tests for Phase 4, Task 3: Condition (error) hierarchy."""

import pytest
import fclpy.state as state
from fclpy.lisptype import (
    Condition, SimpleCondition, Warning, Error,
    TypeError, ProgramError, ControlError, FileError, StreamError, EndOfFile,
    ArithmeticError, DivisionByZero, FloatingPointOverflow,
    NIL, T
)
from fclpy.lispenv import setup_standard_environment


class TestConditionHierarchy:
    """Test the condition class hierarchy."""
    
    def test_base_condition_creation(self):
        """Create a basic condition."""
        cond = Condition(message="Something went wrong")
        assert cond.get_slot('message') == "Something went wrong"
        assert isinstance(cond, Condition)
    
    def test_condition_slots(self):
        """Test accessing and setting condition slots."""
        cond = Condition()
        cond.set_slot('custom-slot', 42)
        assert cond.get_slot('custom-slot') == 42
    
    def test_simple_condition(self):
        """Create a simple condition with format string."""
        cond = SimpleCondition("Error: ~A", format_control="Error: ~A")
        assert cond.get_slot('format-control') == "Error: ~A"
        assert isinstance(cond, SimpleCondition)
        assert isinstance(cond, Condition)
    
    def test_warning_creation(self):
        """Create a warning condition."""
        warn = Warning(message="This is a warning")
        assert warn.get_slot('message') == "This is a warning"
        assert isinstance(warn, Warning)
        assert isinstance(warn, Condition)
        assert not isinstance(warn, Error)
    
    def test_error_creation(self):
        """Create an error condition."""
        err = Error(message="This is an error")
        assert err.get_slot('message') == "This is an error"
        assert isinstance(err, Error)
        assert isinstance(err, Condition)
        assert not isinstance(err, Warning)
    
    def test_type_error(self):
        """Create a TypeError condition."""
        err = TypeError(datum=42, expected_type='STRING')
        assert err.get_slot('datum') == 42
        assert err.get_slot('expected-type') == 'STRING'
        assert isinstance(err, TypeError)
        assert isinstance(err, Error)
        assert isinstance(err, Condition)
    
    def test_type_error_message(self):
        """TypeError should have a meaningful default message."""
        err = TypeError(datum=42, expected_type='STRING')
        msg = str(err)
        assert '42' in msg
        assert 'STRING' in msg
    
    def test_file_error(self):
        """Create a FileError condition."""
        err = FileError(pathname="/path/to/file.txt")
        assert err.get_slot('pathname') == "/path/to/file.txt"
        assert isinstance(err, FileError)
        assert isinstance(err, Error)
    
    def test_stream_error(self):
        """Create a StreamError condition."""
        stream_obj = "STREAM-OBJECT"
        err = StreamError(stream=stream_obj)
        assert err.get_slot('stream') == stream_obj
        assert isinstance(err, StreamError)
        assert isinstance(err, Error)
    
    def test_end_of_file_error(self):
        """Create an EndOfFile condition."""
        stream_obj = "STREAM-OBJECT"
        err = EndOfFile(stream=stream_obj)
        assert err.get_slot('stream') == stream_obj
        assert "End of file" in str(err)
        assert isinstance(err, EndOfFile)
        assert isinstance(err, StreamError)
        assert isinstance(err, Error)
    
    def test_arithmetic_error(self):
        """Create an ArithmeticError condition."""
        err = ArithmeticError(operation='+', operands=[1, 'not-a-number'])
        assert err.get_slot('operation') == '+'
        assert err.get_slot('operands') == [1, 'not-a-number']
        assert isinstance(err, ArithmeticError)
        assert isinstance(err, Error)
    
    def test_division_by_zero(self):
        """Create a DivisionByZero condition."""
        err = DivisionByZero(operation='/', operands=[10, 0])
        assert err.get_slot('operation') == '/'
        assert isinstance(err, DivisionByZero)
        assert isinstance(err, ArithmeticError)
        assert isinstance(err, Error)
    
    def test_floating_point_overflow(self):
        """Create a FloatingPointOverflow condition."""
        err = FloatingPointOverflow(operation='EXP')
        assert err.get_slot('operation') == 'EXP'
        assert isinstance(err, FloatingPointOverflow)
        assert isinstance(err, ArithmeticError)
    
    def test_control_error(self):
        """Create a ControlError condition."""
        err = ControlError(message="Invalid control transfer")
        assert err.get_slot('message') == "Invalid control transfer"
        assert isinstance(err, ControlError)
        assert isinstance(err, Error)
    
    def test_program_error(self):
        """Create a ProgramError condition."""
        err = ProgramError(message="Logic error in program")
        assert err.get_slot('message') == "Logic error in program"
        assert isinstance(err, ProgramError)
        assert isinstance(err, Error)


class TestConditionTypeHierarchy:
    """Test type relationships in condition hierarchy."""
    
    def test_all_errors_are_conditions(self):
        """All error types should be instances of Condition."""
        errors = [
            Error(),
            TypeError(datum=1, expected_type='INT'),
            FileError(),
            StreamError(),
            ArithmeticError(),
        ]
        for err in errors:
            assert isinstance(err, Condition)
            assert isinstance(err, Error)
    
    def test_warnings_not_errors(self):
        """Warnings should not be Error instances."""
        warn = Warning()
        assert isinstance(warn, Condition)
        assert not isinstance(warn, Error)
    
    def test_hierarchy_relationships(self):
        """Test specific hierarchy relationships."""
        # FileError is an Error
        assert isinstance(FileError(), Error)
        
        # StreamError is an Error
        assert isinstance(StreamError(), Error)
        
        # EndOfFile is a StreamError
        assert isinstance(EndOfFile(), StreamError)
        
        # DivisionByZero is an ArithmeticError
        assert isinstance(DivisionByZero(), ArithmeticError)


class TestConditionRepresentation:
    """Test condition string representations."""
    
    def test_condition_repr(self):
        """Test __repr__ of conditions."""
        cond = Condition(message="test")
        repr_str = repr(cond)
        assert 'CONDITION' in repr_str
        assert 'test' in repr_str
    
    def test_error_repr(self):
        """Test __repr__ of error conditions."""
        err = Error(message="error message")
        repr_str = repr(err)
        assert 'ERROR' in repr_str
        assert 'error message' in repr_str
    
    def test_type_error_repr(self):
        """Test __repr__ of TypeError."""
        err = TypeError(datum=42, expected_type='INT')
        repr_str = repr(err)
        assert 'TYPEERROR' in repr_str
        assert '42' in repr_str
    
    def test_condition_str(self):
        """Test __str__ of conditions."""
        cond = Condition(message="test message")
        assert str(cond) == "test message"
    
    def test_error_str(self):
        """Test __str__ of error conditions."""
        err = Error(message="error text")
        assert str(err) == "error text"


class TestConditionWithEnvironment:
    """Test conditions work with Lisp environment."""
    
    @pytest.fixture
    def env(self):
        """Create a fresh environment for each test."""
        state.current_environment = None
        state.functions_loaded = False
        return setup_standard_environment()
    
    def test_condition_in_lisp_env(self, env):
        """Test that Python condition objects can be used in Lisp environment."""
        # Create a condition and store it in environment
        err = TypeError(datum=42, expected_type='STRING')
        env.add_variable(__import__('fclpy.lisptype', fromlist=['LispSymbol']).LispSymbol('*ERROR*'), err)
        
        # Retrieve it
        retrieved = env.find_variable(__import__('fclpy.lisptype', fromlist=['LispSymbol']).LispSymbol('*ERROR*'))
        assert retrieved == err
        assert isinstance(retrieved, TypeError)


class TestMultipleConditionSlots:
    """Test conditions with multiple slots."""
    
    def test_condition_multiple_slots(self):
        """Create condition with multiple custom slots."""
        cond = Condition(
            message="Error occurred",
            line=42,
            column=10,
            filename="test.lisp"
        )
        assert cond.get_slot('message') == "Error occurred"
        assert cond.get_slot('line') == 42
        assert cond.get_slot('column') == 10
        assert cond.get_slot('filename') == "test.lisp"
    
    def test_error_with_context_slots(self):
        """Create error with contextual information."""
        err = Error(
            message="Division by zero",
            operation='/',
            operands=[10, 0],
            context="in SUM-VALUES"
        )
        assert err.get_slot('operation') == '/'
        assert err.get_slot('operands') == [10, 0]
        assert err.get_slot('context') == "in SUM-VALUES"
    
    def test_slot_modification(self):
        """Test modifying condition slots."""
        cond = Condition(message="original")
        assert cond.get_slot('message') == "original"
        
        cond.set_slot('message', "modified")
        assert cond.get_slot('message') == "modified"


if __name__ == '__main__':
    pytest.main([__file__, '-v'])
