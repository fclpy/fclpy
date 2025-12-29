"""Tests for optimization policy declarations and placeholders."""

import pytest
import io
from fclpy.lisptype import LispSymbol, T, NIL, Environment
from fclpy.lispfunc.evaluation import eval
from fclpy.lispfunc.utilities import get_optimization_policy, is_variable_special
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


class TestOptimizationPolicy:
    """Test optimization policy storage and retrieval."""
    
    def test_get_optimization_policy_default(self):
        """get_optimization_policy should return default values when not set."""
        lispenv.setup_standard_environment()
        env = state.current_environment
        
        policy = get_optimization_policy(env)
        
        # All qualities should have default value of 1
        assert policy['speed'] == 1
        assert policy['safety'] == 1
        assert policy['debug'] == 1
        assert policy['compilation-speed'] == 1
        assert policy['space'] == 1
    
    def test_declaim_optimize_updates_policy(self):
        """DECLAIM with OPTIMIZE should update the optimization policy."""
        lispenv.setup_standard_environment()
        env = state.current_environment
        
        # Clear any existing policy first
        root_env = env
        while root_env.parent:
            root_env = root_env.parent
        if hasattr(root_env, '_optimization_policy'):
            delattr(root_env, '_optimization_policy')
        
        # Evaluate a DECLAIM with specific optimization settings
        code = '(DECLAIM (OPTIMIZE (SPEED 3) (SAFETY 0) (DEBUG 1)))'
        form = _parse_code(code)
        eval(form, env)
        
        # Check that policy was updated
        policy = get_optimization_policy(env)
        assert policy['speed'] == 3
        assert policy['safety'] == 0
        assert policy['debug'] == 1
        # Unmentioned qualities keep defaults
        assert policy['compilation-speed'] == 1
        assert policy['space'] == 1
    
    def test_optimization_policy_multiple_declaims(self):
        """Multiple DECLAIM calls should accumulate optimization settings."""
        lispenv.setup_standard_environment()
        env = state.current_environment
        
        # First DECLAIM
        code1 = '(DECLAIM (OPTIMIZE (SPEED 2)))'
        form1 = _parse_code(code1)
        eval(form1, env)
        
        policy1 = get_optimization_policy(env)
        assert policy1['speed'] == 2
        
        # Second DECLAIM overrides
        code2 = '(DECLAIM (OPTIMIZE (SPEED 3) (SAFETY 2)))'
        form2 = _parse_code(code2)
        eval(form2, env)
        
        policy2 = get_optimization_policy(env)
        assert policy2['speed'] == 3
        assert policy2['safety'] == 2
    
    def test_optimization_levels_clamped(self):
        """Optimization levels should be clamped to 0-3 range."""
        lispenv.setup_standard_environment()
        env = state.current_environment
        
        # Try to set a value above 3
        code = '(DECLAIM (OPTIMIZE (SPEED 10) (SAFETY -5)))'
        form = _parse_code(code)
        eval(form, env)
        
        policy = get_optimization_policy(env)
        # Should be clamped to max 3 and min 0
        assert policy['speed'] == 3
        assert policy['safety'] == 0


class TestSpecialVariableDeclarations:
    """Test SPECIAL variable declarations."""
    
    def test_is_variable_special_default(self):
        """is_variable_special should return False for undeclared variables."""
        lispenv.setup_standard_environment()
        env = state.current_environment
        
        x = LispSymbol('X')
        result = is_variable_special(x, env)
        assert result == False
    
    def test_declaim_special_registers_variable(self):
        """DECLAIM with SPECIAL should register variables as special."""
        lispenv.setup_standard_environment()
        env = state.current_environment
        
        # Evaluate DECLAIM with SPECIAL
        code = '(DECLAIM (SPECIAL X Y Z))'
        form = _parse_code(code)
        eval(form, env)
        
        # Check that variables are registered as special
        x = LispSymbol('X')
        y = LispSymbol('Y')
        z = LispSymbol('Z')
        
        assert is_variable_special(x, env) == True
        assert is_variable_special(y, env) == True
        assert is_variable_special(z, env) == True
    
    def test_multiple_special_declarations(self):
        """Multiple SPECIAL declarations should accumulate."""
        lispenv.setup_standard_environment()
        env = state.current_environment
        
        # First DECLAIM
        code1 = '(DECLAIM (SPECIAL A B))'
        form1 = _parse_code(code1)
        eval(form1, env)
        
        a = LispSymbol('A')
        assert is_variable_special(a, env) == True
        
        # Second DECLAIM
        code2 = '(DECLAIM (SPECIAL C D))'
        form2 = _parse_code(code2)
        eval(form2, env)
        
        c = LispSymbol('C')
        assert is_variable_special(c, env) == True
        assert is_variable_special(a, env) == True  # Still special
    
    def test_special_variable_not_special_by_default(self):
        """Undeclared variables should not be special."""
        lispenv.setup_standard_environment()
        env = state.current_environment
        
        unknown = LispSymbol('UNKNOWN')
        result = is_variable_special(unknown, env)
        assert result == False


class TestOptimizationPolicyIntegration:
    """Test integration of optimization policy with declarations."""
    
    def test_mixed_declarations(self):
        """Multiple types of declarations should work together."""
        lispenv.setup_standard_environment()
        env = state.current_environment
        
        # Declare both optimization and special variables
        code = '(DECLAIM (OPTIMIZE (SPEED 3) (SAFETY 1)) (SPECIAL *GLOBAL* *STATE*))'
        form = _parse_code(code)
        result = eval(form, env)
        
        # Should return NIL
        assert result == NIL
        
        # Check policy
        policy = get_optimization_policy(env)
        assert policy['speed'] == 3
        assert policy['safety'] == 1
        
        # Check special variables
        global_sym = LispSymbol('*GLOBAL*')
        state_sym = LispSymbol('*STATE*')
        assert is_variable_special(global_sym, env) == True
        assert is_variable_special(state_sym, env) == True
    
    def test_optimization_policy_not_yet_used_in_compilation(self):
        """Optimization policy is stored but doesn't affect function compilation yet.
        
        This test verifies that setting optimization doesn't change behavior,
        it just stores the settings for future use.
        """
        lispenv.setup_standard_environment()
        env = state.current_environment
        
        # Set optimization to max speed, min safety
        code1 = '(DECLAIM (OPTIMIZE (SPEED 3) (SAFETY 0)))'
        form1 = _parse_code(code1)
        eval(form1, env)
        
        # Define a function - it should work the same regardless of optimization
        code2 = '(DEFUN FAST-ADD (A B) (+ A B))'
        form2 = _parse_code(code2)
        sym = eval(form2, env)
        
        # Call the function
        code3 = '(FAST-ADD 3 4)'
        form3 = _parse_code(code3)
        result = eval(form3, env)
        
        # Result should be correct regardless of optimization settings
        assert result == 7
