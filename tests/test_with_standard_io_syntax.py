"""WITH-STANDARD-IO-SYNTAX establishes the twenty-one bindings CLHS 23.4 gives it.

It was registered as a `cl_function`, so its body forms were evaluated *before*
the form ran, in the caller's dynamic environment, and it bound nothing:
`(let ((*print-base* 2)) (with-standard-io-syntax (prin1-to-string 5)))`
answered "101" where ANSI requires "5". 455 uses across 58 ansi-test files go
through it, `def-pprint-test` among them.

The behaviour is covered thoroughly by `reader/with-standard-io-syntax.lsp`;
what these tests add is the *architectural* guard plan.md section 7 asks for --
one registration, of the right kind, and one home for the standard pprint
dispatch table.
"""

import pytest

import fclpy.lisptype as lisptype
from fclpy.lispfunc import eval_string, setup_environment
from fclpy.lispfunc import registry as _registry
from fclpy.lispfunc.io_write import (
    PprintDispatchTable,
    standard_pprint_dispatch,
)
from fclpy.readtable import standard_readtable


@pytest.fixture()
def env():
    return setup_environment()


class TestRegistration:
    """Standing rule 3: one implementation, and it must not be a function."""

    def test_registered_as_a_macro_not_a_function(self):
        entry = _registry.function_registry['WITH-STANDARD-IO-SYNTAX']
        assert entry.kind == 'macro', (
            "a cl_function evaluates its arguments before the form runs, so it "
            "cannot establish bindings its body forms see")

    def test_no_competing_special_form_registration(self):
        # Two registrations of one operator win or lose by module import
        # order, which is how a fix silently fails to apply (standing rule 3).
        assert 'WITH-STANDARD-IO-SYNTAX' not in _registry.special_registry


class TestBindings:
    """CLHS 23.4's binding list, rebinding whatever the caller had bound."""

    @pytest.mark.parametrize('outer, expression, expected', [
        ('(*print-base* 8)', '*print-base*', 10),
        ('(*read-base* 8)', '*read-base*', 10),
        ('(*print-array* nil)', '*print-array*', lisptype.T),
        ('(*print-escape* nil)', '*print-escape*', lisptype.T),
        ('(*print-gensym* nil)', '*print-gensym*', lisptype.T),
        ('(*print-readably* nil)', '*print-readably*', lisptype.T),
        ('(*read-eval* nil)', '*read-eval*', lisptype.T),
        ('(*print-circle* t)', '*print-circle*', lisptype.NIL),
        ('(*print-pretty* t)', '*print-pretty*', lisptype.NIL),
        ('(*read-suppress* t)', '*read-suppress*', lisptype.NIL),
        ('(*print-length* 100)', '*print-length*', lisptype.NIL),
        ('(*print-level* 100)', '*print-level*', lisptype.NIL),
        ('(*print-lines* 100)', '*print-lines*', lisptype.NIL),
        ('(*print-miser-width* 100)', '*print-miser-width*', lisptype.NIL),
        ('(*print-right-margin* 100)', '*print-right-margin*', lisptype.NIL),
        ('(*print-radix* t)', '*print-radix*', lisptype.NIL),
    ])
    def test_rebinds_over_an_enclosing_binding(self, env, outer, expression, expected):
        result = eval_string(
            f'(let ({outer}) (with-standard-io-syntax {expression}))', env)
        assert result == expected

    def test_print_case_is_the_keyword_upcase(self, env):
        result = eval_string(
            '(let ((*print-case* :downcase)) (with-standard-io-syntax *print-case*))',
            env)
        assert result is lisptype.intern_keyword('UPCASE')

    def test_read_default_float_format_is_single_float(self, env):
        result = eval_string('(with-standard-io-syntax *read-default-float-format*)', env)
        assert str(result) == 'SINGLE-FLOAT'

    def test_package_is_cl_user_by_name_not_by_caller(self, env):
        result = eval_string(
            '(let ((*package* (find-package "KEYWORD")))'
            '  (with-standard-io-syntax (package-name *package*)))', env)
        assert str(result) == 'COMMON-LISP-USER'

    def test_readtable_is_the_standard_readtable(self, env):
        eval_string('(with-standard-io-syntax (setq *x* *readtable*))', env)
        assert eval_string('*x*', env) is standard_readtable()

    def test_pprint_dispatch_is_the_standard_table(self, env):
        eval_string('(with-standard-io-syntax (setq *x* *print-pprint-dispatch*))', env)
        assert eval_string('*x*', env) is standard_pprint_dispatch()

    def test_the_bindings_do_not_outlive_the_form(self, env):
        result = eval_string(
            '(let ((*print-base* 2))'
            '  (list (with-standard-io-syntax (prin1-to-string 5))'
            '        (prin1-to-string 5)))', env)
        assert [str(x) for x in result] == ['5', '101']


class TestBodySemantics:
    """The form is LET's, so its value, values and exits are LET's."""

    def test_empty_body_is_nil(self, env):
        assert eval_string('(with-standard-io-syntax)', env) in (lisptype.NIL, None)

    def test_returns_the_last_form(self, env):
        result = eval_string(
            '(let ((i 3)) (with-standard-io-syntax (incf i 10) (+ i 2)))', env)
        assert result == 15

    def test_a_non_local_exit_passes_through(self, env):
        result = eval_string(
            '(block done'
            '  (tagbody'
            '   (with-standard-io-syntax (go 10) 10 (return-from done :bad))'
            '   10'
            '   (return-from done :good)))', env)
        assert result is lisptype.intern_keyword('GOOD')


class TestStandardPprintDispatch:
    """One home for the table, as `standard_readtable()` is for the readtable."""

    def test_is_one_shared_object(self):
        assert standard_pprint_dispatch() is standard_pprint_dispatch()

    def test_copy_pprint_dispatch_of_nil_is_a_distinct_table(self, env):
        copy = eval_string('(copy-pprint-dispatch nil)', env)
        assert isinstance(copy, PprintDispatchTable)
        assert copy is not standard_pprint_dispatch()

    def test_copy_pprint_dispatch_rejects_a_non_table(self, env):
        # Loud, not silent (standing rule 4). The evaluator turns the raise
        # into a signaled Lisp condition, which is the boundary standing
        # rule 2 asks for -- a Python exception must not be the form's value.
        from fclpy.lispfunc.evaluation_core import ConditionException
        with pytest.raises((ConditionException, lisptype.LispNotImplementedError)):
            eval_string('(copy-pprint-dispatch 42)', env)
