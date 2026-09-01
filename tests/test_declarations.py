"""Tests for DECLARE and DECLAIM special forms."""

import pytest
import io
from fclpy.lisptype import LispSymbol, T, NIL, Environment
from fclpy.lispfunc.evaluation import eval_declare, eval_declaim, eval
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


class TestDeclare:
    """Test DECLARE special form for local declarations."""
    
    def test_declare_returns_nil(self):
        """DECLARE should return NIL."""
        lispenv.setup_standard_environment()
        env = state.current_environment
        
        # Create a simple (DECLARE (OPTIMIZE (SPEED 3))) form
        declare_sym = LispSymbol('DECLARE')
        optimize_sym = LispSymbol('OPTIMIZE')
        speed_sym = LispSymbol('SPEED')
        
        # (DECLARE (OPTIMIZE (SPEED 3)))
        speed_level = cons(speed_sym, cons(3, NIL))
        optimize_spec = cons(optimize_sym, cons(speed_level, NIL))
        declare_form = cons(declare_sym, cons(optimize_spec, NIL))
        
        result = eval_declare(declare_form, env)
        assert result == NIL
    
    def test_declare_stores_in_environment(self):
        """DECLARE should store declarations in the environment."""
        lispenv.setup_standard_environment()
        env = state.current_environment
        
        # Create declarations
        declare_sym = LispSymbol('DECLARE')
        optimize_sym = LispSymbol('OPTIMIZE')
        speed_sym = LispSymbol('SPEED')
        
        speed_level = cons(speed_sym, cons(3, NIL))
        optimize_spec = cons(optimize_sym, cons(speed_level, NIL))
        declare_form = cons(declare_sym, cons(optimize_spec, NIL))
        
        # Evaluate declaration
        eval_declare(declare_form, env)
        
        # Check that declaration was stored
        assert hasattr(env, '_declarations')
        assert 'OPTIMIZE' in env._declarations
    
    def test_declare_multiple_specs(self):
        """DECLARE should handle multiple declaration specs."""
        lispenv.setup_standard_environment()
        env = state.current_environment
        
        declare_sym = LispSymbol('DECLARE')
        optimize_sym = LispSymbol('OPTIMIZE')
        special_sym = LispSymbol('SPECIAL')
        speed_sym = LispSymbol('SPEED')
        x_sym = LispSymbol('X')
        
        # (DECLARE (OPTIMIZE (SPEED 3)) (SPECIAL X))
        speed_level = cons(speed_sym, cons(3, NIL))
        optimize_spec = cons(optimize_sym, cons(speed_level, NIL))
        special_spec = cons(special_sym, cons(x_sym, NIL))
        
        declare_form = cons(declare_sym, cons(optimize_spec, cons(special_spec, NIL)))
        
        result = eval_declare(declare_form, env)
        assert result == NIL
        assert hasattr(env, '_declarations')
        assert 'OPTIMIZE' in env._declarations
    
    def test_declare_type_annotation(self):
        """DECLARE should handle TYPE declarations."""
        lispenv.setup_standard_environment()
        env = state.current_environment
        
        declare_sym = LispSymbol('DECLARE')
        type_sym = LispSymbol('TYPE')
        integer_sym = LispSymbol('INTEGER')
        x_sym = LispSymbol('X')
        
        # (DECLARE (TYPE INTEGER X))
        type_spec = cons(type_sym, cons(integer_sym, cons(x_sym, NIL)))
        declare_form = cons(declare_sym, cons(type_spec, NIL))
        
        result = eval_declare(declare_form, env)
        assert result == NIL


class TestDeclaim:
    """Test DECLAIM special form for global declarations."""
    
    def test_declaim_returns_nil(self):
        """DECLAIM should return NIL."""
        lispenv.setup_standard_environment()
        env = state.current_environment
        
        # Create a simple (DECLAIM (OPTIMIZE (SPEED 3))) form
        declaim_sym = LispSymbol('DECLAIM')
        optimize_sym = LispSymbol('OPTIMIZE')
        speed_sym = LispSymbol('SPEED')
        
        speed_level = cons(speed_sym, cons(3, NIL))
        optimize_spec = cons(optimize_sym, cons(speed_level, NIL))
        declaim_form = cons(declaim_sym, cons(optimize_spec, NIL))
        
        result = eval_declaim(declaim_form, env)
        assert result == NIL
    
    def test_declaim_stores_optimization_policy(self):
        """DECLAIM should store optimization settings globally."""
        lispenv.setup_standard_environment()
        env = state.current_environment
        
        # Create (DECLAIM (OPTIMIZE (SPEED 3) (SAFETY 1)))
        declaim_sym = LispSymbol('DECLAIM')
        optimize_sym = LispSymbol('OPTIMIZE')
        speed_sym = LispSymbol('SPEED')
        safety_sym = LispSymbol('SAFETY')
        
        speed_level = cons(speed_sym, cons(3, NIL))
        safety_level = cons(safety_sym, cons(1, NIL))
        optimize_spec = cons(optimize_sym, cons(speed_level, cons(safety_level, NIL)))
        declaim_form = cons(declaim_sym, cons(optimize_spec, NIL))
        
        # Get root environment
        root_env = env
        while root_env.parent is not None:
            root_env = root_env.parent
        
        eval_declaim(declaim_form, env)
        
        # Check that optimization policy was set
        assert hasattr(root_env, '_optimization_policy')
        assert root_env._optimization_policy.get('speed') == 3
        assert root_env._optimization_policy.get('safety') == 1
    
    def test_declaim_special_variables(self):
        """DECLAIM should register special variables globally."""
        lispenv.setup_standard_environment()
        env = state.current_environment
        
        # Create (DECLAIM (SPECIAL X Y Z))
        declaim_sym = LispSymbol('DECLAIM')
        special_sym = LispSymbol('SPECIAL')
        x_sym = LispSymbol('X')
        y_sym = LispSymbol('Y')
        z_sym = LispSymbol('Z')
        
        special_spec = cons(special_sym, cons(x_sym, cons(y_sym, cons(z_sym, NIL))))
        declaim_form = cons(declaim_sym, cons(special_spec, NIL))
        
        # Get root environment
        root_env = env
        while root_env.parent is not None:
            root_env = root_env.parent
        
        eval_declaim(declaim_form, env)
        
        # Check that special variables were registered
        assert hasattr(root_env, '_special_variables')
        assert 'X' in root_env._special_variables
        assert 'Y' in root_env._special_variables
        assert 'Z' in root_env._special_variables
    
    def test_declaim_multiple_specs(self):
        """DECLAIM should handle multiple declaration specs."""
        lispenv.setup_standard_environment()
        env = state.current_environment
        
        declaim_sym = LispSymbol('DECLAIM')
        optimize_sym = LispSymbol('OPTIMIZE')
        special_sym = LispSymbol('SPECIAL')
        speed_sym = LispSymbol('SPEED')
        x_sym = LispSymbol('X')
        
        # (DECLAIM (OPTIMIZE (SPEED 3)) (SPECIAL X))
        speed_level = cons(speed_sym, cons(3, NIL))
        optimize_spec = cons(optimize_sym, cons(speed_level, NIL))
        special_spec = cons(special_sym, cons(x_sym, NIL))
        declaim_form = cons(declaim_sym, cons(optimize_spec, cons(special_spec, NIL)))
        
        result = eval_declaim(declaim_form, env)
        assert result == NIL


class TestDeclarationIntegration:
    """Test declarations in context with other forms."""
    
    def test_declare_in_let_form(self):
        """DECLARE can appear in a LET form."""
        lispenv.setup_standard_environment()
        env = state.current_environment
        
        # (LET ((X 1)) (DECLARE (OPTIMIZE (SPEED 3))) X)
        code = "(LET ((X 1)) (DECLARE (OPTIMIZE (SPEED 3))) X)"
        form = _parse_code(code)
        result = eval(form, env)
        # Should return 1 if X is properly bound
        assert result == 1
    
    def test_declare_in_function(self):
        """DECLARE can appear at the start of a function body."""
        lispenv.setup_standard_environment()
        env = state.current_environment
        
        # Define function with declaration
        code = "(DEFUN ADD-ONE (X) (DECLARE (OPTIMIZE (SPEED 3))) (+ X 1))"
        form = _parse_code(code)
        eval(form, env)
        
        # Now call the function
        call_code = "(ADD-ONE 5)"
        call_form = _parse_code(call_code)
        result = eval(call_form, env)
        assert result == 6
