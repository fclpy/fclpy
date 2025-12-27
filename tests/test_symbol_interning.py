"""Test that symbols are properly interned through packages."""

import pytest
from fclpy.lisptype import (
    LispSymbol, intern_symbol, COMMON_LISP_USER_PACKAGE, 
    COMMON_LISP_PACKAGE, KEYWORD_PACKAGE, intern_keyword
)


def test_intern_symbol_uses_package():
    """Test that intern_symbol properly associates symbols with packages."""
    sym = intern_symbol("MY-VAR")
    assert sym.package is COMMON_LISP_USER_PACKAGE


def test_intern_symbol_in_cl_package():
    """Test that we can intern in different packages."""
    sym = intern_symbol("MY-BUILTIN", COMMON_LISP_PACKAGE)
    assert sym.package is COMMON_LISP_PACKAGE
    assert sym.name == "MY-BUILTIN"


def test_intern_symbol_same_object():
    """Test that interning the same symbol twice returns same object."""
    sym1 = intern_symbol("MYVAR")
    sym2 = intern_symbol("MYVAR")
    assert sym1 is sym2


def test_intern_symbol_case_insensitive():
    """Test that symbol interning is case-insensitive."""
    sym1 = intern_symbol("myvar")
    sym2 = intern_symbol("MYVAR")
    sym3 = intern_symbol("MyVar")
    assert sym1 is sym2
    assert sym2 is sym3
    assert sym1.name == "MYVAR"


def test_intern_symbol_with_symbol_object():
    """Test that intern_symbol handles symbol objects."""
    sym1 = intern_symbol("TEST")
    sym2 = intern_symbol(sym1)
    assert sym1 is sym2


def test_intern_symbol_by_package_name():
    """Test that we can intern using package name string."""
    sym = intern_symbol("MY-SYM", "COMMON-LISP")
    assert sym.package is COMMON_LISP_PACKAGE


def test_keyword_not_in_user_package():
    """Test that keywords are in KEYWORD package, not CL-USER."""
    kw = intern_keyword("FOO")
    assert kw.package is KEYWORD_PACKAGE
    
    # Regular symbol should be in CL-USER
    sym = intern_symbol("BAR")
    assert sym.package is COMMON_LISP_USER_PACKAGE
    assert sym is not kw


def test_py_str_to_sym_uses_intern():
    """Test that py_str_to_sym uses the package system."""
    from fclpy.lisptype import py_str_to_sym
    sym = py_str_to_sym("TEST-VAR")
    assert sym.package is not None
    assert isinstance(sym.package.__class__.__name__, str)
