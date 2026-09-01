"""Tests for file loading functionality.

These tests verify that LOAD works correctly, especially:
1. Nested LOAD calls work properly  
2. Functions defined in one file are visible in subsequently loaded files
3. LOAD returns T on success

This prevents regression of the bug where load_and_evaluate_file had
'return lisptype.NIL' in its finally block, causing all LOADs to return NIL.
"""

import os
import pytest

import fclpy.lisptype as lisptype
import fclpy.lispfunc as lispfunc
from fclpy import lispenv
from fclpy.runtime import load_and_evaluate_file


@pytest.fixture
def fresh_environment():
    """Create a fresh Lisp environment for each test."""
    import fclpy.state as state
    
    # Setup fresh environment
    lispenv.setup_standard_environment()
    env = lispenv.current_environment
    
    return env


@pytest.fixture
def fixtures_dir():
    """Get the path to the test fixtures directory."""
    return os.path.join(os.path.dirname(__file__), 'fixtures')


class TestLoadFunction:
    """Tests for the LOAD function and load_and_evaluate_file."""
    
    def test_load_returns_t_on_success(self, fresh_environment, fixtures_dir):
        """LOAD should return T on successful file load."""
        filepath = os.path.join(fixtures_dir, 'rt-minimal.lsp')
        result = load_and_evaluate_file(filepath, fresh_environment)
        assert result is lisptype.T
    
    def test_load_single_file_defines_function(self, fresh_environment, fixtures_dir):
        """Loading a file should define functions in it."""
        filepath = os.path.join(fixtures_dir, 'rt-minimal.lsp')
        load_and_evaluate_file(filepath, fresh_environment)
        
        # Check that RECORD-FILE-LOAD function was defined
        func_sym = lisptype.COMMON_LISP_USER_PACKAGE.intern_symbol('RECORD-FILE-LOAD')
        func = fresh_environment.find_func(func_sym)
        assert func is not None
        assert callable(func)
    
    def test_load_sequence_all_return_t(self, fresh_environment, fixtures_dir):
        """Loading files in sequence should all return T."""
        # Load rt-minimal.lsp first (defines RECORD-FILE-LOAD and *TEST-LOADED-FILES*)
        rt_path = os.path.join(fixtures_dir, 'rt-minimal.lsp')
        result1 = load_and_evaluate_file(rt_path, fresh_environment)
        assert result1 is lisptype.T
        
        # Load test-package.lsp (uses RECORD-FILE-LOAD from previous file)
        pkg_path = os.path.join(fixtures_dir, 'test-package.lsp')
        result2 = load_and_evaluate_file(pkg_path, fresh_environment)
        assert result2 is lisptype.T
        
        # Load sample-tests.lsp (also uses RECORD-FILE-LOAD)
        tests_path = os.path.join(fixtures_dir, 'sample-tests.lsp')
        result3 = load_and_evaluate_file(tests_path, fresh_environment)
        assert result3 is lisptype.T
    
    def test_nested_load_returns_t(self, fresh_environment, fixtures_dir):
        """Loading a file that contains LOAD calls should return T.
        
        This is the key regression test for the bug where load_and_evaluate_file
        returned NIL from its finally block, breaking nested LOADs.
        """
        # Load the chain file which does nested LOADs
        chain_path = os.path.join(fixtures_dir, 'load-chain.lsp')
        result = load_and_evaluate_file(chain_path, fresh_environment)
        
        # The overall LOAD should return T (not NIL!)
        # This was the bug: finally block was returning NIL
        assert result is lisptype.T, "LOAD should return T on success, not NIL"
    
    def test_load_nonexistent_file_returns_nil(self, fresh_environment, fixtures_dir):
        """Loading a nonexistent file should return NIL."""
        filepath = os.path.join(fixtures_dir, 'nonexistent.lsp')
        
        # The function catches FileNotFoundError and returns NIL
        result = load_and_evaluate_file(filepath, fresh_environment)
        assert result is lisptype.NIL


class TestLoadReturnValue:
    """Tests specifically for LOAD return value behavior."""
    
    def test_successful_load_returns_t(self, fresh_environment, fixtures_dir):
        """A successful LOAD must return T, not NIL."""
        filepath = os.path.join(fixtures_dir, 'rt-minimal.lsp')
        result = load_and_evaluate_file(filepath, fresh_environment)
        
        # This is the critical assertion - the bug was returning NIL
        assert result is lisptype.T
        assert result is not lisptype.NIL
        assert result is not None
    
    def test_multiple_loads_all_return_t(self, fresh_environment, fixtures_dir):
        """Multiple sequential LOADs should all return T."""
        files = ['rt-minimal.lsp', 'test-package.lsp', 'sample-tests.lsp']
        
        for filename in files:
            filepath = os.path.join(fixtures_dir, filename)
            result = load_and_evaluate_file(filepath, fresh_environment)
            assert result is lisptype.T, f"LOAD of {filename} should return T"
