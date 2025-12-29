"""Tests for DOCUMENTATION function and docstring support in DEFUN/DEFMACRO."""

import pytest
import io
from fclpy.lisptype import LispSymbol, T, NIL, Environment
from fclpy.lispfunc.evaluation import eval, eval_defun, eval_defmacro
from fclpy.lispfunc.utilities import documentation
from fclpy.lispfunc.core import cons, car, cdr
from fclpy.readtable import get_current_readtable
from fclpy.lispreader import LispReader, LispStream
import fclpy.lispenv as lispenv
import fclpy.state as state


def _parse_code(code):
    """Helper to parse Lisp code from a string."""
    rt = get_current_readtable()
    s = io.StringIO(code)
    reader = LispReader(rt.get_macro_character, LispStream(s))
    return reader.read_1()


class TestDocumentationFunction:
    """Test the DOCUMENTATION function for retrieving docstrings."""
    
    def test_documentation_returns_nil_by_default(self):
        """DOCUMENTATION should return NIL for symbols with no docstring."""
        sym = LispSymbol('FOO')
        result = documentation(sym)
        assert result == NIL
    
    def test_documentation_with_function_type(self):
        """DOCUMENTATION should return docstring when doc_type is FUNCTION."""
        sym = LispSymbol('BAR')
        sym.plist = {'DOCUMENTATION': 'This is a test function'}
        
        # Call with explicit FUNCTION type
        func_type_sym = LispSymbol('FUNCTION')
        result = documentation(sym, func_type_sym)
        assert result == 'This is a test function'
    
    def test_documentation_with_default_type(self):
        """DOCUMENTATION should default to FUNCTION type."""
        sym = LispSymbol('BAZ')
        sym.plist = {'DOCUMENTATION': 'A default docstring'}
        
        result = documentation(sym)
        assert result == 'A default docstring'
    
    def test_documentation_with_non_symbol(self):
        """DOCUMENTATION should return NIL for non-symbol arguments."""
        result = documentation(42)
        assert result == NIL
        
        result = documentation("string")
        assert result == NIL


class TestDefunDocstring:
    """Test DEFUN's ability to store and retrieve docstrings."""
    
    def test_defun_with_docstring(self):
        """DEFUN should store docstring in function symbol's plist."""
        lispenv.setup_standard_environment()
        env = state.current_environment
        
        # Parse (DEFUN FOO (X) "Add one to X" (+ X 1))
        code = '(DEFUN ADD-N (N) "Add N to argument" (+ DUMMY N))'
        form = _parse_code(code)
        
        # Evaluate the definition
        result = eval(form, env)
        
        # result should be the function name symbol
        assert isinstance(result, LispSymbol)
        assert result.name == 'ADD-N'
        
        # Check that docstring was stored
        assert hasattr(result, 'plist')
        assert 'DOCUMENTATION' in result.plist
        assert result.plist['DOCUMENTATION'] == 'Add N to argument'
    
    def test_defun_without_docstring(self):
        """DEFUN should work without a docstring."""
        lispenv.setup_standard_environment()
        env = state.current_environment
        
        # Parse (DEFUN BAR (X) (+ X 2))
        code = '(DEFUN BAR (X) (+ X 2))'
        form = _parse_code(code)
        
        result = eval(form, env)
        assert isinstance(result, LispSymbol)
        assert result.name == 'BAR'
        
        # Should have plist but no DOCUMENTATION key
        # (or DOCUMENTATION should map to NIL)
    
    def test_defun_docstring_with_documentation_function(self):
        """DOCUMENTATION function should retrieve docstring from DEFUN."""
        lispenv.setup_standard_environment()
        env = state.current_environment
        
        # Define a function with docstring
        code = '(DEFUN MY-FUNC (X) "My documentation" (+ X 1))'
        form = _parse_code(code)
        func_sym = eval(form, env)
        
        # Now retrieve the docstring
        doc = documentation(func_sym)
        assert doc == 'My documentation'


class TestDefmacroDocstring:
    """Test DEFMACRO's ability to store and retrieve docstrings."""
    
    def test_defmacro_with_docstring(self):
        """DEFMACRO should store docstring in macro symbol's plist."""
        lispenv.setup_standard_environment()
        env = state.current_environment
        
        # Parse (DEFMACRO MY-MACRO (X) "Macro docs" (LIST QUOTE X))
        code = '(DEFMACRO MY-MACRO (X) "Transform X" (LIST (QUOTE QUOTE) X))'
        form = _parse_code(code)
        
        result = eval(form, env)
        assert isinstance(result, LispSymbol)
        assert result.name == 'MY-MACRO'
        
        # Check that docstring was stored
        assert hasattr(result, 'plist')
        assert 'DOCUMENTATION' in result.plist
        assert result.plist['DOCUMENTATION'] == 'Transform X'
    
    def test_defmacro_without_docstring(self):
        """DEFMACRO should work without a docstring."""
        lispenv.setup_standard_environment()
        env = state.current_environment
        
        code = '(DEFMACRO SIMPLE-MACRO (X) (LIST (QUOTE QUOTE) X))'
        form = _parse_code(code)
        
        result = eval(form, env)
        assert isinstance(result, LispSymbol)
        assert result.name == 'SIMPLE-MACRO'
    
    def test_defmacro_docstring_with_documentation_function(self):
        """DOCUMENTATION function should retrieve docstring from DEFMACRO."""
        lispenv.setup_standard_environment()
        env = state.current_environment
        
        # Define a macro with docstring
        code = '(DEFMACRO QUOTE-IT (X) "Quote the argument" (LIST (QUOTE QUOTE) X))'
        form = _parse_code(code)
        macro_sym = eval(form, env)
        
        # Now retrieve the docstring
        doc = documentation(macro_sym)
        assert doc == 'Quote the argument'


class TestDocstringIntegration:
    """Test docstring integration with the rest of the system."""
    
    def test_multiple_definitions_with_docstrings(self):
        """Multiple functions with docstrings should each store their own."""
        lispenv.setup_standard_environment()
        env = state.current_environment
        
        # Define multiple functions
        code1 = '(DEFUN FUNC1 (X) "First function" (+ X 1))'
        code2 = '(DEFUN FUNC2 (X) "Second function" (+ X 2))'
        
        form1 = _parse_code(code1)
        form2 = _parse_code(code2)
        
        sym1 = eval(form1, env)
        sym2 = eval(form2, env)
        
        # Check each has its own docstring
        assert documentation(sym1) == 'First function'
        assert documentation(sym2) == 'Second function'
    
    def test_docstring_persists_across_calls(self):
        """Docstring should remain accessible after function is called."""
        lispenv.setup_standard_environment()
        env = state.current_environment
        
        code = '(DEFUN CALLABLE (X) "This function adds one" (+ X 1))'
        form = _parse_code(code)
        func_sym = eval(form, env)
        
        # Get the docstring before calling
        doc_before = documentation(func_sym)
        
        # Call the function
        call_code = '(CALLABLE 5)'
        call_form = _parse_code(call_code)
        result = eval(call_form, env)
        
        # Get the docstring after calling
        doc_after = documentation(func_sym)
        
        assert result == 6
        assert doc_before == doc_after
        assert doc_after == 'This function adds one'
    
    def test_documentation_for_built_in_functions(self):
        """DOCUMENTATION should return NIL for built-in functions (not yet documented)."""
        lispenv.setup_standard_environment()
        env = state.current_environment
        
        # Built-in functions don't have docstrings yet
        plus_sym = LispSymbol('PLUS')
        result = documentation(plus_sym)
        assert result == NIL
