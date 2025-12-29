"""Test that keywords work properly."""

import pytest
from fclpy.lisptype import lispKeyword, intern_keyword, KEYWORD_PACKAGE
from fclpy.lisptype import Environment
from fclpy.lispfunc.evaluation import eval as lisp_eval


def test_keyword_package_exists():
    """Test that KEYWORD package exists."""
    assert KEYWORD_PACKAGE is not None
    assert KEYWORD_PACKAGE.name == "KEYWORD"


def test_intern_keyword_creates_keyword():
    """Test that intern_keyword creates a lispKeyword object."""
    kw = intern_keyword("FOO")
    assert isinstance(kw, lispKeyword)
    assert kw.name == "FOO"
    assert kw.package is KEYWORD_PACKAGE


def test_intern_keyword_with_colon():
    """Test that intern_keyword handles names with leading colons."""
    kw = intern_keyword(":FOO")
    assert isinstance(kw, lispKeyword)
    assert kw.name == "FOO"


def test_intern_keyword_returns_same_object():
    """Test that interning the same keyword twice returns the same object."""
    kw1 = intern_keyword("FOO")
    kw2 = intern_keyword("FOO")
    assert kw1 is kw2


def test_keyword_string_representation():
    """Test that keywords print with leading colon."""
    kw = intern_keyword("FOO")
    assert repr(kw) == ":FOO"
    assert str(kw) == ":FOO"


def test_keyword_evaluates_to_itself():
    """Test that keywords evaluate to themselves."""
    kw = intern_keyword("BAR")
    env = Environment()
    result = lisp_eval(kw, env)
    assert result is kw


def test_multiple_keywords_independent():
    """Test that different keywords are independent."""
    kw1 = intern_keyword("FOO")
    kw2 = intern_keyword("BAR")
    assert kw1 is not kw2
    assert kw1.name != kw2.name


def test_keyword_case_insensitivity():
    """Test that keywords are case-insensitive (uppercase internally)."""
    kw1 = intern_keyword("foo")
    kw2 = intern_keyword("FOO")
    kw3 = intern_keyword("Foo")
    assert kw1 is kw2
    assert kw2 is kw3
    assert kw1.name == "FOO"


def test_intern_keyword_with_keyword_object():
    """Test that intern_keyword handles keyword objects."""
    kw1 = intern_keyword("TEST")
    kw2 = intern_keyword(kw1)
    assert kw1 is kw2


def test_keyword_in_package():
    """Test that keywords are stored in KEYWORD package."""
    kw = intern_keyword("MYKEY")
    found, status = KEYWORD_PACKAGE.find_symbol("MYKEY")
    assert found is kw
    assert status == ':EXTERNAL'
