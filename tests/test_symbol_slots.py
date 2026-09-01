"""Test that symbols have value, function, and plist slots."""

import pytest
from fclpy.lisptype import LispSymbol, symbol_value, set_symbol_value
from fclpy.lisptype import symbol_function, set_symbol_function
from fclpy.lisptype import symbol_plist, set_symbol_plist


def test_symbol_value_slot():
    """Test that symbols can store and retrieve values."""
    sym = LispSymbol('X')
    assert symbol_value(sym) is None
    
    set_symbol_value(sym, 42)
    assert symbol_value(sym) == 42
    
    set_symbol_value(sym, "hello")
    assert symbol_value(sym) == "hello"


def test_symbol_function_slot():
    """Test that symbols can store and retrieve function definitions."""
    sym = LispSymbol('MY-FUNC')
    assert symbol_function(sym) is None
    
    def my_fn():
        return 123
    
    set_symbol_function(sym, my_fn)
    assert symbol_function(sym) is my_fn
    assert symbol_function(sym)() == 123


def test_symbol_plist_slot():
    """Test that symbols can store and retrieve property lists."""
    sym = LispSymbol('MY-SYM')
    assert symbol_plist(sym) == {}
    
    plist = {'color': 'red', 'size': 'large'}
    set_symbol_plist(sym, plist)
    assert symbol_plist(sym) == plist
    assert symbol_plist(sym)['color'] == 'red'


def test_symbol_slots_independent():
    """Test that different symbols have independent slots."""
    sym1 = LispSymbol('A')
    sym2 = LispSymbol('B')
    
    set_symbol_value(sym1, 10)
    set_symbol_value(sym2, 20)
    
    assert symbol_value(sym1) == 10
    assert symbol_value(sym2) == 20


def test_symbol_value_type_check():
    """Test that accessor functions check type."""
    with pytest.raises(TypeError):
        symbol_value(42)
    
    with pytest.raises(TypeError):
        set_symbol_value(42, "value")


def test_backward_compatibility():
    """Test that existing symbol creation still works."""
    sym = LispSymbol('TEST')
    assert sym.name == 'TEST'
    assert sym.package is None
    assert hasattr(sym, 'value')
    assert hasattr(sym, 'function')
    assert hasattr(sym, 'plist')
