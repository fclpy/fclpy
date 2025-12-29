"""
Tests for Phase 3: Basic Evaluation - Lambda List Parser.

This test module verifies that lambda list parsing works correctly for
different parameter types: regular, &optional, &rest, &key, &aux.
"""

import pytest
from fclpy.lisptype import (
    LispSymbol, lispCons, NIL, T
)
from fclpy.lispfunc.evaluation import eval, parse_lambda_list
from fclpy.lispenv import setup_standard_environment
from fclpy.lispfunc.core import car, cdr
import fclpy.state as state


@pytest.fixture
def env():
    """Setup a clean environment for each test."""
    state.current_environment = None
    state.functions_loaded = False
    return setup_standard_environment()


class TestParseLambdaList:
    """Test lambda list parser."""
    
    def test_parse_empty_lambda_list(self):
        """Parse empty parameter list."""
        result = parse_lambda_list(NIL)
        
        assert result['required'] == []
        assert result['optional'] == []
        assert result['rest'] is None
        assert result['keyword'] == []
        assert result['aux'] == []
    
    def test_parse_required_parameters(self):
        """Parse required parameters only."""
        # (a b c) -> 3 required params
        a = LispSymbol('a')
        b = LispSymbol('b')
        c = LispSymbol('c')
        lambda_list = lispCons(a, lispCons(b, lispCons(c, NIL)))
        
        result = parse_lambda_list(lambda_list)
        
        assert len(result['required']) == 3
        assert result['required'][0].name == 'a'
        assert result['required'][1].name == 'b'
        assert result['required'][2].name == 'c'
        assert result['optional'] == []
        assert result['rest'] is None
        assert result['keyword'] == []
        assert result['aux'] == []
    
    def test_parse_with_optional(self):
        """Parse parameters with &optional."""
        # (a &optional b c) -> 1 required, 2 optional
        a = LispSymbol('a')
        and_optional = LispSymbol('&OPTIONAL')
        b = LispSymbol('b')
        c = LispSymbol('c')
        lambda_list = lispCons(a, lispCons(and_optional, lispCons(b, lispCons(c, NIL))))
        
        result = parse_lambda_list(lambda_list)
        
        assert len(result['required']) == 1
        assert result['required'][0].name == 'a'
        assert len(result['optional']) == 2
        # Optional should be list of [symbol, default_value] pairs or just symbols
        assert result['rest'] is None
    
    def test_parse_with_rest(self):
        """Parse parameters with &rest."""
        # (a &rest args) -> 1 required, rest args
        a = LispSymbol('a')
        and_rest = LispSymbol('&REST')
        args = LispSymbol('args')
        lambda_list = lispCons(a, lispCons(and_rest, lispCons(args, NIL)))
        
        result = parse_lambda_list(lambda_list)
        
        assert len(result['required']) == 1
        assert result['required'][0].name == 'a'
        assert result['rest'] is not None
        assert result['rest'].name == 'args'
        assert result['optional'] == []
        assert result['keyword'] == []
        assert result['aux'] == []
    
    def test_parse_with_keyword(self):
        """Parse parameters with &key."""
        # (a &key x y) -> 1 required, 2 keyword
        a = LispSymbol('a')
        and_key = LispSymbol('&KEY')
        x = LispSymbol('x')
        y = LispSymbol('y')
        lambda_list = lispCons(a, lispCons(and_key, lispCons(x, lispCons(y, NIL))))
        
        result = parse_lambda_list(lambda_list)
        
        assert len(result['required']) == 1
        assert result['required'][0].name == 'a'
        assert len(result['keyword']) == 2
        assert result['optional'] == []
        assert result['rest'] is None
        assert result['aux'] == []
    
    def test_parse_with_aux(self):
        """Parse parameters with &aux."""
        # (a &aux b c) -> 1 required, 2 aux
        a = LispSymbol('a')
        and_aux = LispSymbol('&AUX')
        b = LispSymbol('b')
        c = LispSymbol('c')
        lambda_list = lispCons(a, lispCons(and_aux, lispCons(b, lispCons(c, NIL))))
        
        result = parse_lambda_list(lambda_list)
        
        assert len(result['required']) == 1
        assert result['required'][0].name == 'a'
        assert result['optional'] == []
        assert result['rest'] is None
        assert result['keyword'] == []
        assert len(result['aux']) == 2
    
    def test_parse_complex_lambda_list(self):
        """Parse complex lambda list with all types."""
        # (a b &optional c d &rest args &key x y &aux z)
        a = LispSymbol('a')
        b = LispSymbol('b')
        and_optional = LispSymbol('&OPTIONAL')
        c = LispSymbol('c')
        d = LispSymbol('d')
        and_rest = LispSymbol('&REST')
        args = LispSymbol('args')
        and_key = LispSymbol('&KEY')
        x = LispSymbol('x')
        y = LispSymbol('y')
        and_aux = LispSymbol('&AUX')
        z = LispSymbol('z')
        
        lambda_list = lispCons(a, lispCons(b, lispCons(and_optional, lispCons(c,
                   lispCons(d, lispCons(and_rest, lispCons(args,
                   lispCons(and_key, lispCons(x, lispCons(y,
                   lispCons(and_aux, lispCons(z, NIL))))))))))))
        
        result = parse_lambda_list(lambda_list)
        
        assert len(result['required']) == 2
        assert len(result['optional']) == 2
        assert result['rest'].name == 'args'
        assert len(result['keyword']) == 2
        assert len(result['aux']) == 1
    
    def test_parse_optional_with_defaults(self):
        """Parse optional parameters with default values."""
        # (a &optional (b 10) (c 20)) -> 1 required, 2 optional with defaults
        a = LispSymbol('a')
        and_optional = LispSymbol('&OPTIONAL')
        b = LispSymbol('b')
        c = LispSymbol('c')
        b_spec = lispCons(b, lispCons(10, NIL))
        c_spec = lispCons(c, lispCons(20, NIL))
        
        lambda_list = lispCons(a, lispCons(and_optional, lispCons(b_spec, lispCons(c_spec, NIL))))
        
        result = parse_lambda_list(lambda_list)
        
        assert len(result['required']) == 1
        assert len(result['optional']) == 2
        # Optionals should have defaults


class TestLambdaFunctionWithParsing:
    """Test that LAMBDA uses the parser correctly."""
    
    def test_lambda_simple_parameters(self, env):
        """LAMBDA with simple required parameters."""
        # (LAMBDA (x y) (+ x y)) should create a function
        lambda_sym = LispSymbol('LAMBDA')
        x = LispSymbol('x')
        y = LispSymbol('y')
        params = lispCons(x, lispCons(y, NIL))
        
        plus_sym = LispSymbol('+')
        body = lispCons(plus_sym, lispCons(x, lispCons(y, NIL)))
        
        lambda_form = lispCons(lambda_sym, lispCons(params, lispCons(body, NIL)))
        
        func = eval(lambda_form, env)
        
        # Call the function
        result = func(3, 4)
        assert result == 7
    
    def test_lambda_with_optional(self, env):
        """LAMBDA with optional parameters."""
        # (LAMBDA (x &optional y) (+ x (or y 10))) 
        # When called with 1 arg, should use default
        lambda_sym = LispSymbol('LAMBDA')
        x = LispSymbol('x')
        and_optional = LispSymbol('&OPTIONAL')
        y = LispSymbol('y')
        params = lispCons(x, lispCons(and_optional, lispCons(y, NIL)))
        
        # For now, this test expects the function to accept optional args
        # Full implementation would handle defaults properly
        plus_sym = LispSymbol('+')
        body = lispCons(plus_sym, lispCons(x, lispCons(y, NIL)))
        
        lambda_form = lispCons(lambda_sym, lispCons(params, lispCons(body, NIL)))
        
        func = eval(lambda_form, env)
        
        # Should be callable
        assert callable(func)
    
    def test_lambda_with_rest(self, env):
        """LAMBDA with rest parameter."""
        # (LAMBDA (x &rest args) (list x args))
        lambda_sym = LispSymbol('LAMBDA')
        x = LispSymbol('x')
        and_rest = LispSymbol('&REST')
        args = LispSymbol('args')
        params = lispCons(x, lispCons(and_rest, lispCons(args, NIL)))
        
        list_sym = LispSymbol('LIST')
        body = lispCons(list_sym, lispCons(x, lispCons(args, NIL)))
        
        lambda_form = lispCons(lambda_sym, lispCons(params, lispCons(body, NIL)))
        
        func = eval(lambda_form, env)
        
        # Should be callable
        assert callable(func)
