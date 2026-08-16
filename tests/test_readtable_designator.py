"""The readtable designator rule and the standard readtable (CLHS 23.1).

These pin the mechanism behind `(copy-readtable nil)`. NIL denotes the
*standard* readtable wherever a readtable designator is accepted -- not the
current one -- and every operator that takes a readtable resolves its argument
the same way, through `coerce_to_readtable`. Eight operators previously each
carried their own `if readtable is None` check that handled an omitted
argument and nothing else, so all eight raised on NIL.

That mattered well beyond the reader: ansi-test's `my-with-standard-io-syntax`
binds `*readtable*` to `(copy-readtable nil)`, and `def-print-test` is built on
it, so the raise made every one of the 189 printer tests in
`printer/print-integers.lsp` fail regardless of what the printer did.
"""

import pytest

import fclpy.lisptype as lisptype
from fclpy.lispfunc import eval_string, setup_environment
from fclpy.lispfunc.io import copy_readtable, readtable_case, readtablep
from fclpy.readtable import (
    Readtable,
    coerce_to_readtable,
    get_current_readtable,
    standard_readtable,
    _OMITTED,
)


@pytest.fixture()
def env():
    return setup_environment()


class TestReadtableDesignator:
    """CLHS glossary: NIL denotes the standard readtable, a readtable itself."""

    def test_nil_denotes_the_standard_readtable(self):
        assert coerce_to_readtable(lisptype.NIL, 'TEST') is standard_readtable()

    def test_python_none_denotes_the_standard_readtable(self):
        # NIL reaches Python as the NIL singleton or as None depending on the
        # call path (CLAUDE.md: NIL has three representations).
        assert coerce_to_readtable(None, 'TEST') is standard_readtable()

    def test_a_readtable_denotes_itself(self):
        rt = Readtable()
        assert coerce_to_readtable(rt, 'TEST') is rt

    def test_omitted_denotes_the_current_readtable(self):
        assert coerce_to_readtable(_OMITTED, 'TEST') is get_current_readtable()

    def test_a_non_readtable_is_a_type_error(self):
        with pytest.raises(lisptype.LispTypeError):
            coerce_to_readtable(42, 'TEST')


class TestStandardReadtable:
    """CLHS 23.1.1: it is distinct from the current readtable, and immutable."""

    def test_the_current_readtable_is_not_the_standard_one(self):
        assert get_current_readtable() is not standard_readtable()

    def test_the_standard_readtable_is_immutable(self):
        with pytest.raises(lisptype.LispError):
            standard_readtable().set_macro_character('~', lambda c, s: None)

    def test_a_copy_of_the_standard_readtable_is_mutable(self):
        rt = standard_readtable().copy()
        rt.set_macro_character('~', lambda c, s: None)
        assert rt.get_macro_character('~') is not None
        # ... and mutating the copy did not reach the original.
        assert standard_readtable().get_macro_character('~') is None


class TestCopyReadtable:
    """`copy-readtable.1`-`.7`, reproduced at the unit level."""

    def test_copy_of_nil_is_a_fresh_readtable(self):
        rt = copy_readtable(lisptype.NIL)
        assert readtablep(rt) is lisptype.T
        assert rt is not standard_readtable()
        assert rt is not get_current_readtable()

    def test_copy_with_no_argument_copies_the_current_readtable(self):
        rt = copy_readtable()
        assert readtablep(rt) is lisptype.T
        assert rt is not get_current_readtable()

    def test_to_readtable_is_modified_and_returned(self):
        target = copy_readtable()
        result = copy_readtable(get_current_readtable(), target)
        assert result is target

    def test_nil_to_readtable_makes_a_fresh_one(self):
        result = copy_readtable(get_current_readtable(), lisptype.NIL)
        assert result is not get_current_readtable()


class TestMacroCharacterReturnValues:
    """CLHS 23.2's return contracts -- no Python object may be the value.

    The readtable stores a macro character as a Python `(function, flag)`
    tuple; handing that back is standing rule 2, and it is what
    `(get-macro-character #\\()` did once the character-designator fix made the
    lookup succeed at all.
    """

    def test_get_macro_character_returns_two_values(self, env):
        assert eval_string(
            "(length (multiple-value-list (get-macro-character #\\()))",
            env) == 2

    def test_open_paren_is_a_terminating_macro_character(self, env):
        # Second value is non-terminating-p, so `(` -- which terminates a
        # token -- must answer NIL for it.
        assert eval_string(
            "(nth-value 1 (get-macro-character #\\())", env) is lisptype.NIL

    def test_get_macro_character_of_a_constituent_is_nil(self, env):
        assert eval_string("(null (get-macro-character #\\a))", env) is lisptype.T

    def test_set_macro_character_returns_t(self, env):
        assert eval_string(
            "(let ((*readtable* (copy-readtable nil)))"
            "  (set-macro-character #\\~ (lambda (s c) 42)))", env) is lisptype.T

    def test_get_dispatch_macro_character_absent_is_nil(self, env):
        assert eval_string(
            "(let ((*readtable* (copy-readtable nil)))"
            "  (get-dispatch-macro-character #\\# #\\~))", env) is lisptype.NIL


class TestReadtableIsAnObject:
    """READTABLEP and TYPEP must agree, and both must say yes."""

    def test_readtablep_of_the_current_readtable(self, env):
        assert eval_string("(readtablep *readtable*)", env) is lisptype.T

    def test_typep_readtable(self, env):
        assert eval_string("(typep (copy-readtable nil) 'readtable)", env) is lisptype.T

    def test_readtablep_of_a_non_readtable(self, env):
        assert eval_string("(readtablep 17)", env) is lisptype.NIL


class TestReadtableCase:
    """CLHS 23.2: a keyword, and a place."""

    def test_readtable_case_is_a_keyword(self):
        assert readtable_case() is lisptype.intern_keyword('UPCASE')

    def test_setf_readtable_case(self, env):
        result = eval_string(
            "(let ((*readtable* (copy-readtable nil)))"
            "  (setf (readtable-case *readtable*) :preserve)"
            "  (readtable-case *readtable*))", env)
        assert result is lisptype.intern_keyword('PRESERVE')

    def test_setf_readtable_case_unwinds(self, env):
        """The binding is dynamic, so the outer readtable is left alone."""
        eval_string(
            "(let ((*readtable* (copy-readtable nil)))"
            "  (setf (readtable-case *readtable*) :preserve))", env)
        assert eval_string("(readtable-case *readtable*)", env) \
            is lisptype.intern_keyword('UPCASE')


class TestReadtableVariableHasOneHome:
    """`*READTABLE*` is a variable the reader actually consults.

    It used to be a module global in `readtable.py` *plus* a separate
    `*READTABLE*` variable that nothing read -- plan.md C7's defect, where a
    control variable is not connected to the mechanism it names.
    """

    def test_get_current_readtable_follows_the_binding(self, env):
        inner = []
        eval_string("(copy-readtable nil)", env)  # ensure initialized
        outer = get_current_readtable()
        fresh = standard_readtable().copy()
        sym = lisptype.COMMON_LISP_PACKAGE.intern_symbol('*READTABLE*')
        saved = sym.value
        try:
            sym.value = fresh
            inner.append(get_current_readtable())
        finally:
            sym.value = saved
        assert inner[0] is fresh
        assert get_current_readtable() is outer

    def test_a_non_readtable_value_is_loud(self):
        sym = lisptype.COMMON_LISP_PACKAGE.intern_symbol('*READTABLE*')
        saved = sym.value
        try:
            sym.value = 42
            with pytest.raises(lisptype.LispTypeError):
                get_current_readtable()
        finally:
            sym.value = saved
