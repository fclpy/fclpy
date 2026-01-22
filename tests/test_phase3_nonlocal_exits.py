"""
Tests for Task 7: Non-local exits (BLOCK/RETURN-FROM, CATCH/THROW, TAGBODY/GO).

This tests non-local exit mechanisms for control flow.
"""

import pytest
from fclpy.lisptype import (
    LispSymbol, lispCons, NIL, T
)
from fclpy.lispfunc.evaluation import eval
from fclpy.lispenv import setup_standard_environment
from fclpy.lispfunc.core import car, cdr
import fclpy.state as state


@pytest.fixture
def env():
    """Setup a clean environment for each test."""
    state.current_environment = None
    state.functions_loaded = False
    env_obj = setup_standard_environment()
    yield env_obj
    # Cleanup after test to avoid leaking state to other tests
    state.current_environment = None
    state.functions_loaded = False


class TestBlockReturnFrom:
    """Test BLOCK and RETURN-FROM for non-local exits."""
    
    def test_block_simple(self, env):
        """BLOCK should evaluate its body normally if no RETURN-FROM."""
        # (BLOCK myblock (+ 1 2))
        block_sym = LispSymbol('BLOCK')
        block_name = LispSymbol('MYBLOCK')
        plus = LispSymbol('+')
        expr = lispCons(plus, lispCons(1, lispCons(2, NIL)))
        form = lispCons(block_sym, lispCons(block_name, lispCons(expr, NIL)))
        
        result = eval(form, env)
        
        # Should return the value of the expression
        assert result == 3
    
    def test_block_multiple_forms(self, env):
        """BLOCK should evaluate all forms and return the last."""
        # (BLOCK myblock 1 2 3)
        block_sym = LispSymbol('BLOCK')
        block_name = LispSymbol('MYBLOCK')
        form = lispCons(block_sym, lispCons(block_name, lispCons(1, lispCons(2, lispCons(3, NIL)))))
        
        result = eval(form, env)
        
        # Should return the last value
        assert result == 3
    
    def test_return_from_simple(self, env):
        """RETURN-FROM should exit the block early."""
        # (BLOCK myblock (RETURN-FROM myblock 42) 1 2)
        block_sym = LispSymbol('BLOCK')
        block_name = LispSymbol('MYBLOCK')
        return_sym = LispSymbol('RETURN-FROM')
        return_form = lispCons(return_sym, lispCons(block_name, lispCons(42, NIL)))
        form = lispCons(block_sym, lispCons(block_name, lispCons(return_form, lispCons(1, lispCons(2, NIL)))))
        
        result = eval(form, env)
        
        # Should return 42, not 2
        assert result == 42
    
    def test_return_from_in_function_call(self, env):
        """RETURN-FROM should exit the block from within called functions."""
        # (BLOCK myblock (+ (RETURN-FROM myblock 100) 1))
        # The RETURN-FROM should skip the rest and return 100
        block_sym = LispSymbol('BLOCK')
        block_name = LispSymbol('MYBLOCK')
        return_sym = LispSymbol('RETURN-FROM')
        return_form = lispCons(return_sym, lispCons(block_name, lispCons(100, NIL)))
        plus = LispSymbol('+')
        expr = lispCons(plus, lispCons(return_form, lispCons(1, NIL)))
        form = lispCons(block_sym, lispCons(block_name, lispCons(expr, NIL)))
        
        result = eval(form, env)
        
        # Should return 100
        assert result == 100
    
    def test_nested_blocks_inner_return(self, env):
        """RETURN-FROM in inner block should only exit that block."""
        # (BLOCK outer
        #   (BLOCK inner (RETURN-FROM inner 10))
        #   20)
        block_sym = LispSymbol('BLOCK')
        outer_name = LispSymbol('OUTER')
        inner_name = LispSymbol('INNER')
        return_sym = LispSymbol('RETURN-FROM')
        
        return_form = lispCons(return_sym, lispCons(inner_name, lispCons(10, NIL)))
        inner_block = lispCons(block_sym, lispCons(inner_name, lispCons(return_form, NIL)))
        outer_block = lispCons(block_sym, lispCons(outer_name, lispCons(inner_block, lispCons(20, NIL))))
        
        result = eval(outer_block, env)
        
        # Should return 20 (the inner RETURN-FROM returns 10, then outer continues with 20)
        assert result == 20
    
    def test_nested_blocks_outer_return(self, env):
        """RETURN-FROM in outer block should exit through inner blocks."""
        # (BLOCK outer
        #   (BLOCK inner (RETURN-FROM outer 5))
        #   20)
        block_sym = LispSymbol('BLOCK')
        outer_name = LispSymbol('OUTER')
        inner_name = LispSymbol('INNER')
        return_sym = LispSymbol('RETURN-FROM')
        
        return_form = lispCons(return_sym, lispCons(outer_name, lispCons(5, NIL)))
        inner_block = lispCons(block_sym, lispCons(inner_name, lispCons(return_form, NIL)))
        outer_block = lispCons(block_sym, lispCons(outer_name, lispCons(inner_block, lispCons(20, NIL))))
        
        result = eval(outer_block, env)
        
        # Should return 5 (the RETURN-FROM outer skips both blocks)
        assert result == 5


class TestCatchThrow:
    """Test CATCH and THROW for exception-like control flow."""
    
    def test_catch_no_throw(self, env):
        """CATCH should evaluate body normally if no THROW."""
        # (CATCH 'mytag (+ 1 2))
        catch_sym = LispSymbol('CATCH')
        quote_sym = LispSymbol('QUOTE')
        tag = LispSymbol('MYTAG')
        quoted_tag = lispCons(quote_sym, lispCons(tag, NIL))
        plus = LispSymbol('+')
        expr = lispCons(plus, lispCons(1, lispCons(2, NIL)))
        form = lispCons(catch_sym, lispCons(quoted_tag, lispCons(expr, NIL)))
        
        result = eval(form, env)
        
        # Should return the value of the expression
        assert result == 3
    
    def test_throw_basic(self, env):
        """THROW should exit the CATCH and return the thrown value."""
        # (CATCH 'mytag (THROW 'mytag 99))
        catch_sym = LispSymbol('CATCH')
        throw_sym = LispSymbol('THROW')
        quote_sym = LispSymbol('QUOTE')
        tag = LispSymbol('MYTAG')
        quoted_tag = lispCons(quote_sym, lispCons(tag, NIL))
        throw_form = lispCons(throw_sym, lispCons(quoted_tag, lispCons(99, NIL)))
        form = lispCons(catch_sym, lispCons(quoted_tag, lispCons(throw_form, NIL)))
        
        result = eval(form, env)
        
        # Should return 99
        assert result == 99
    
    def test_throw_skips_remaining_forms(self, env):
        """THROW should skip remaining forms in CATCH."""
        # (CATCH 'mytag (THROW 'mytag 7) 999)
        catch_sym = LispSymbol('CATCH')
        throw_sym = LispSymbol('THROW')
        quote_sym = LispSymbol('QUOTE')
        tag = LispSymbol('MYTAG')
        quoted_tag = lispCons(quote_sym, lispCons(tag, NIL))
        throw_form = lispCons(throw_sym, lispCons(quoted_tag, lispCons(7, NIL)))
        form = lispCons(catch_sym, lispCons(quoted_tag, lispCons(throw_form, lispCons(999, NIL))))
        
        result = eval(form, env)
        
        # Should return 7, not 999
        assert result == 7
    
    def test_throw_wrong_tag(self, env):
        """THROW with different tag should propagate up."""
        # (CATCH 'tag1 (THROW 'tag2 50))
        # This should raise an error (uncaught throw)
        catch_sym = LispSymbol('CATCH')
        throw_sym = LispSymbol('THROW')
        quote_sym = LispSymbol('QUOTE')
        tag1 = LispSymbol('TAG1')
        tag2 = LispSymbol('TAG2')
        quoted_tag1 = lispCons(quote_sym, lispCons(tag1, NIL))
        quoted_tag2 = lispCons(quote_sym, lispCons(tag2, NIL))
        throw_form = lispCons(throw_sym, lispCons(quoted_tag2, lispCons(50, NIL)))
        form = lispCons(catch_sym, lispCons(quoted_tag1, lispCons(throw_form, NIL)))
        
        # Should raise an exception (uncaught throw)
        with pytest.raises(Exception):
            eval(form, env)


class TestTagbodyGo:
    """Test TAGBODY and GO for labeled jumps."""
    
    def test_tagbody_no_go(self, env):
        """TAGBODY should evaluate sequentially without GO and return NIL."""
        # (TAGBODY 1 2 3) - forms are evaluated, result is NIL
        tagbody_sym = LispSymbol('TAGBODY')
        form = lispCons(tagbody_sym, lispCons(1, lispCons(2, lispCons(3, NIL))))
        
        result = eval(form, env)
        
        # TAGBODY always returns NIL
        assert result is NIL or result is None
    
    def test_tagbody_with_tags(self, env):
        """TAGBODY with tags but no GO should execute all forms."""
        # (TAGBODY start (+ 1 2) middle (+ 3 4) end)
        tagbody_sym = LispSymbol('TAGBODY')
        start_tag = LispSymbol('START')
        middle_tag = LispSymbol('MIDDLE')
        end_tag = LispSymbol('END')
        plus_sym = LispSymbol('+')
        
        form1 = lispCons(plus_sym, lispCons(1, lispCons(2, NIL)))
        form2 = lispCons(plus_sym, lispCons(3, lispCons(4, NIL)))
        
        form = lispCons(tagbody_sym, 
                       lispCons(start_tag,
                               lispCons(form1,
                                       lispCons(middle_tag,
                                               lispCons(form2,
                                                       lispCons(end_tag, NIL))))))
        
        result = eval(form, env)
        assert result is NIL or result is None
    
    def test_go_forward(self, env):
        """GO should jump forward to a tag, skipping intermediate code."""
        # Use a side-effect to track what executed
        # We'll use a LET to bind a counter variable
        # (LET ((X 0))
        #   (TAGBODY
        #     (SETQ X 1)
        #     (GO END)
        #     (SETQ X 999)   ; Should be skipped
        #     END)
        #   X)
        let_sym = LispSymbol('LET')
        tagbody_sym = LispSymbol('TAGBODY')
        go_sym = LispSymbol('GO')
        setq_sym = LispSymbol('SETQ')
        x_sym = LispSymbol('X')
        end_tag = LispSymbol('END')
        
        # Binding: ((X 0))
        binding = lispCons(x_sym, lispCons(0, NIL))
        bindings = lispCons(binding, NIL)
        
        # SETQ forms
        setq_1 = lispCons(setq_sym, lispCons(x_sym, lispCons(1, NIL)))
        go_end = lispCons(go_sym, lispCons(end_tag, NIL))
        setq_999 = lispCons(setq_sym, lispCons(x_sym, lispCons(999, NIL)))
        
        # TAGBODY
        tagbody = lispCons(tagbody_sym,
                         lispCons(setq_1,
                                 lispCons(go_end,
                                         lispCons(setq_999,
                                                 lispCons(end_tag, NIL)))))
        
        # LET body: TAGBODY then X
        let_form = lispCons(let_sym, lispCons(bindings, lispCons(tagbody, lispCons(x_sym, NIL))))
        
        result = eval(let_form, env)
        
        # X should be 1, not 999 (because GO skipped the second SETQ)
        assert result == 1
    
    def test_go_backward_loop(self, env):
        """GO can jump backward to create a loop."""
        # (LET ((X 0))
        #   (TAGBODY
        #     LOOP
        #     (SETQ X (+ X 1))
        #     (IF (< X 3) (GO LOOP))
        #     END)
        #   X)
        let_sym = LispSymbol('LET')
        tagbody_sym = LispSymbol('TAGBODY')
        go_sym = LispSymbol('GO')
        setq_sym = LispSymbol('SETQ')
        if_sym = LispSymbol('IF')
        x_sym = LispSymbol('X')
        plus_sym = LispSymbol('+')
        lt_sym = LispSymbol('<')
        loop_tag = LispSymbol('LOOP')
        end_tag = LispSymbol('END')
        
        # Binding: ((X 0))
        binding = lispCons(x_sym, lispCons(0, NIL))
        bindings = lispCons(binding, NIL)
        
        # (+ X 1)
        plus_form = lispCons(plus_sym, lispCons(x_sym, lispCons(1, NIL)))
        # (SETQ X (+ X 1))
        setq_inc = lispCons(setq_sym, lispCons(x_sym, lispCons(plus_form, NIL)))
        # (< X 3)
        lt_form = lispCons(lt_sym, lispCons(x_sym, lispCons(3, NIL)))
        # (GO LOOP)
        go_loop = lispCons(go_sym, lispCons(loop_tag, NIL))
        # (IF (< X 3) (GO LOOP))
        if_form = lispCons(if_sym, lispCons(lt_form, lispCons(go_loop, NIL)))
        
        # TAGBODY
        tagbody = lispCons(tagbody_sym,
                         lispCons(loop_tag,
                                 lispCons(setq_inc,
                                         lispCons(if_form,
                                                 lispCons(end_tag, NIL)))))
        
        # LET body: TAGBODY then X
        let_form = lispCons(let_sym, lispCons(bindings, lispCons(tagbody, lispCons(x_sym, NIL))))
        
        result = eval(let_form, env)
        
        # Loop should run 3 times: X=1, X=2, X=3, then exit
        assert result == 3


class TestNonLocalExitCombinations:
    """Test combinations of different non-local exit mechanisms."""
    
    def test_throw_from_block(self, env):
        """THROW should exit blocks on the way out."""
        # (CATCH 'tag
        #   (BLOCK myblock
        #     (THROW 'tag 42)))
        catch_sym = LispSymbol('CATCH')
        block_sym = LispSymbol('BLOCK')
        throw_sym = LispSymbol('THROW')
        quote_sym = LispSymbol('QUOTE')
        tag = LispSymbol('TAG')
        block_name = LispSymbol('MYBLOCK')
        
        quoted_tag = lispCons(quote_sym, lispCons(tag, NIL))
        throw_form = lispCons(throw_sym, lispCons(quoted_tag, lispCons(42, NIL)))
        block_form = lispCons(block_sym, lispCons(block_name, lispCons(throw_form, NIL)))
        catch_form = lispCons(catch_sym, lispCons(quoted_tag, lispCons(block_form, NIL)))
        
        result = eval(catch_form, env)
        
        # Should return 42
        assert result == 42
    
    def test_return_from_vs_throw(self, env):
        """RETURN-FROM should not be caught by CATCH."""
        # (CATCH 'tag
        #   (BLOCK myblock (RETURN-FROM myblock 10)))
        # Should return 10, not be caught
        catch_sym = LispSymbol('CATCH')
        block_sym = LispSymbol('BLOCK')
        return_sym = LispSymbol('RETURN-FROM')
        quote_sym = LispSymbol('QUOTE')
        tag = LispSymbol('TAG')
        block_name = LispSymbol('MYBLOCK')
        
        quoted_tag = lispCons(quote_sym, lispCons(tag, NIL))
        return_form = lispCons(return_sym, lispCons(block_name, lispCons(10, NIL)))
        block_form = lispCons(block_sym, lispCons(block_name, lispCons(return_form, NIL)))
        catch_form = lispCons(catch_sym, lispCons(quoted_tag, lispCons(block_form, NIL)))
        
        result = eval(catch_form, env)
        
        # Should return 10
        assert result == 10
