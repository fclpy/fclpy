"""Regression tests for signal-before-unwind handler dispatch (plan.md M8).

Context: handlers used to be invoked from a Python `except` clause inside
HANDLER-BIND, i.e. *after* the protected form's frames had already been torn
down. A handler therefore could not throw to a CATCH tag or invoke a restart
established inside the protected form -- the entire point of HANDLER-BIND
(plan.md Finding E). The minimal case is the ANSI suite's own
HANDLER-BIND.13, which crashed with "Uncaught THROW DONE" and aborted the
remaining ~17000 tests of the 22036-test run.

Handlers are now invoked by `signal_condition` at the signal point, from a
handler stack (`state.handler_stack`) that HANDLER-BIND, HANDLER-CASE and
IGNORE-ERRORS all push onto. The end-to-end authority is
ansi-test/conditions/ (handler-bind.lsp 1-17 all pass); these tests pin the
individual mechanisms so a future change cannot quietly undo one of them.
"""

import pytest
import fclpy.state as state
import fclpy.lisptype as lisptype
from fclpy.lispfunc.evaluation_core import ConditionException
from fclpy.lispfunc import setup_environment, eval_string


@pytest.fixture
def env():
    state.current_environment = None
    state.functions_loaded = False
    return setup_environment()


def sym_name(value):
    """The name of a returned Lisp symbol, for comparing against 'GOOD'/'BAD'."""
    return getattr(value, 'name', str(value)).upper()


class TestHandlerRunsBeforeUnwinding:
    """The defining property: at the time a handler runs, the protected form's
    dynamic environment is still live."""

    def test_handler_can_throw_to_tag_inside_protected_form(self, env):
        """ANSI HANDLER-BIND.13, the crash that blocked the suite.

        The CATCH is *inside* the HANDLER-BIND, so the handler's THROW can only
        find it if the handler runs before the stack unwinds.
        """
        result = eval_string("""
            (handler-bind ((error #'(lambda (c) (declare (ignore c))
                                      (throw 'done 'good))))
              (catch 'done (error "an error")))
        """, env)
        assert sym_name(result) == 'GOOD'

    def test_handler_can_return_from_block_inside_protected_form(self, env):
        """Same property via RETURN-FROM rather than THROW."""
        result = eval_string("""
            (handler-bind ((error #'(lambda (c) (declare (ignore c))
                                      (return-from inner 'good))))
              (block inner (error "an error") 'bad))
        """, env)
        assert sym_name(result) == 'GOOD'

    def test_unwind_protect_cleanup_inside_protected_form_runs_after_handler(self, env):
        """The cleanup must not have run yet when the handler executes -- that
        is what "before unwinding" means for UNWIND-PROTECT."""
        result = eval_string("""
            (let ((log '()))
              (catch 'done
                (handler-bind ((error #'(lambda (c) (declare (ignore c))
                                          (push 'handler log)
                                          (throw 'done nil))))
                  (unwind-protect (error "an error")
                    (push 'cleanup log))))
              (reverse log))
        """, env)
        assert [sym_name(x) for x in list(result)] == ['HANDLER', 'CLEANUP']


class TestDeclining:
    """A handler declines by returning normally; signaling then continues."""

    def test_declining_handler_lets_signal_continue_outward(self, env):
        """ANSI HANDLER-BIND.12: the inner handler returns normally, so the
        outer one must still get the condition."""
        result = eval_string("""
            (block done
              (handler-bind ((error #'(lambda (c) (declare (ignore c))
                                        (return-from done 'good))))
                (handler-bind ((error #'(lambda (c) c)))
                  (error "an error"))))
        """, env)
        assert sym_name(result) == 'GOOD'

    def test_handlers_in_one_cluster_are_tried_in_order(self, env):
        """ANSI HANDLER-BIND.11: two handlers for the same type in one binding
        list; the first declines and the second must then run."""
        result = eval_string("""
            (block done
              (handler-bind ((error #'(lambda (c) c))
                             (error #'(lambda (c) (declare (ignore c))
                                        (return-from done 'good))))
                (error "an error")))
        """, env)
        assert sym_name(result) == 'GOOD'

    def test_handler_is_disestablished_while_it_runs(self, env):
        """ANSI HANDLER-BIND.6. The inner handler re-signals with (error c); it
        must not re-enter itself (which would loop forever), so the condition
        goes to the *outer* handler. CLHS 9.1.4.1.1."""
        result = eval_string("""
            (block foo
              (handler-bind ((error #'(lambda (c) (declare (ignore c))
                                        (return-from foo 'good))))
                (handler-bind ((error #'(lambda (c) (error c)))
                               (error #'(lambda (c) (declare (ignore c))
                                          (return-from foo 'bad))))
                  (error "an error"))))
        """, env)
        assert sym_name(result) == 'GOOD'


class TestHandlerOrderingAcrossForms:
    """HANDLER-BIND and HANDLER-CASE share one handler stack, so ordering
    between them is by dynamic nesting, not by which form was used."""

    def test_inner_handler_bind_wins_over_outer_handler_case(self, env):
        result = eval_string("""
            (handler-case
                (block done
                  (handler-bind ((error #'(lambda (c) (declare (ignore c))
                                            (return-from done 'good))))
                    (error "an error")))
              (error (c) (declare (ignore c)) 'bad))
        """, env)
        assert sym_name(result) == 'GOOD'

    def test_inner_handler_case_wins_over_outer_handler_bind(self, env):
        result = eval_string("""
            (block done
              (handler-bind ((error #'(lambda (c) (declare (ignore c))
                                        (return-from done 'bad))))
                (handler-case (error "an error")
                  (error (c) (declare (ignore c)) 'good))))
        """, env)
        assert sym_name(result) == 'GOOD'

    def test_nested_handler_case_transfers_to_its_own_frame(self, env):
        """An inner HANDLER-CASE whose clauses do not match must not have its
        transfer intercepted by the outer one, nor vice versa."""
        result = eval_string("""
            (handler-case
                (handler-case (error "an error")
                  (warning (c) (declare (ignore c)) 'bad))
              (error (c) (declare (ignore c)) 'good))
        """, env)
        assert sym_name(result) == 'GOOD'


class TestHandlerTypeSpecifiers:
    """A handler's type is an ordinary type specifier, matched via TYPEP."""

    def test_function_designator_symbol_is_accepted(self, env):
        """ANSI HANDLER-BIND.8: the handler is a quoted symbol naming a
        function, not a function object."""
        result = eval_string("""
            (progn
              (defun my-handler-fn (c) (declare (ignore c)) (throw 'foo 'good))
              (catch 'foo
                (handler-bind ((simple-error 'my-handler-fn))
                  (error "simple error"))))
        """, env)
        assert sym_name(result) == 'GOOD'

    def test_compound_type_specifier_not_error_does_not_match_an_error(self, env):
        """ANSI HANDLER-BIND.16: (NOT ERROR) must decline an error."""
        result = eval_string("""
            (catch 'done
              (handler-bind (((not error) #'identity)
                             (error #'(lambda (c) (declare (ignore c))
                                        (throw 'done 'good))))
                (error "an error")))
        """, env)
        assert sym_name(result) == 'GOOD'

    def test_class_object_as_handler_type(self, env):
        """ANSI HANDLER-BIND.17: the type is a class *object* rather than a
        type-name symbol. HANDLER-BIND does not evaluate the type, hence the
        read-time #. -- writing (find-class 'error) here would instead be a
        compound type specifier whose head is FIND-CLASS.
        """
        result = eval_string("""
            (catch 'done
              (handler-bind ((#.(find-class 'error)
                              #'(lambda (c) (declare (ignore c))
                                  (throw 'done 'good))))
                (error "an error")))
        """, env)
        assert sym_name(result) == 'GOOD'

    def test_non_condition_type_name_never_matches(self, env):
        """ANSI HANDLER-BIND.14: a SYMBOL handler "can never succeed"; the
        condition must fall through to the ERROR handler."""
        result = eval_string("""
            (catch 'done
              (handler-bind ((symbol #'identity)
                             (error #'(lambda (c) (declare (ignore c))
                                        (throw 'done 'good))))
                (error "an error")))
        """, env)
        assert sym_name(result) == 'GOOD'


class TestSignalSemantics:
    """SIGNAL offers a condition to the handlers without unwinding."""

    def test_signal_returns_nil_when_unhandled(self, env):
        assert eval_string('(signal "a simple condition")', env) == lisptype.NIL

    def test_signal_does_not_abandon_the_rest_of_the_form(self, env):
        """SIGNAL must return control to its caller, not unwind: the 42 after
        the SIGNAL still has to be evaluated."""
        assert eval_string('(progn (signal "ignored") 42)', env) == 42

    def test_signal_default_type_is_simple_condition_not_error(self, env):
        """ANSI HANDLER-BIND.10: (SIGNAL "...") builds a SIMPLE-CONDITION, so an
        ERROR handler must decline it and a SIMPLE-CONDITION handler must win --
        even though the ERROR binding comes first."""
        result = eval_string("""
            (block done
              (handler-bind ((error #'(lambda (c) (declare (ignore c))
                                        (return-from done 'bad)))
                             (simple-condition #'(lambda (c) (declare (ignore c))
                                                   (return-from done 'good))))
                (signal "A simple condition")))
        """, env)
        assert sym_name(result) == 'GOOD'

    def test_error_still_unwinds_when_unhandled(self, env):
        with pytest.raises(ConditionException):
            eval_string('(error "an error")', env)

    def test_error_as_a_function_designator_signals_a_matchable_condition(self, env):
        """#'ERROR used to raise a bare Python Exception carrying only a
        message, which no handler clause could match. RT's own report-error
        reaches ERROR this way, via (apply #'error args)."""
        result = eval_string("""
            (handler-case (funcall #'error "an error")
              (simple-error (c) (declare (ignore c)) 'good))
        """, env)
        assert sym_name(result) == 'GOOD'


class TestIgnoreErrors:
    def test_returns_the_condition_object_as_second_value(self, env):
        """CLHS: the second value is the condition. It used to be str(e), a
        Python string leaking out as a Lisp value."""
        result = eval_string("""
            (multiple-value-bind (v c) (ignore-errors (error "an error"))
              (list v (typep c 'condition)))
        """, env)
        values = list(result)
        assert values[0] == lisptype.NIL
        assert values[1] == lisptype.T

    def test_passes_through_a_non_local_exit(self, env):
        """IGNORE-ERRORS caught bare `Exception`, which swallowed THROW and
        RETURN-FROM as though they were errors -- plan.md Finding K's defect
        class in a second operator."""
        result = eval_string("""
            (catch 'out (ignore-errors (throw 'out 'good)))
        """, env)
        assert sym_name(result) == 'GOOD'

    def test_returns_normal_values_when_no_error(self, env):
        result = eval_string("""
            (multiple-value-bind (v c) (ignore-errors 42) (list v c))
        """, env)
        values = list(result)
        assert values[0] == 42
        assert values[1] == lisptype.NIL


class TestHandlerStackDiscipline:
    """The handler stack must be left exactly as it was found, on every path."""

    def test_stack_is_empty_after_normal_return(self, env):
        eval_string('(handler-bind ((error #\'identity)) 1)', env)
        assert state.handler_stack == []

    def test_stack_is_empty_after_a_handled_error(self, env):
        eval_string("""
            (catch 'done
              (handler-bind ((error #'(lambda (c) (declare (ignore c))
                                        (throw 'done nil))))
                (error "an error")))
        """, env)
        assert state.handler_stack == []

    def test_stack_is_empty_after_an_unhandled_error(self, env):
        with pytest.raises(ConditionException):
            eval_string('(handler-bind ((warning #\'identity)) (error "an error"))', env)
        assert state.handler_stack == []

    def test_stack_is_empty_after_handler_case_clause_runs(self, env):
        eval_string('(handler-case (error "an error") (error (c) c))', env)
        assert state.handler_stack == []


class TestConditionLattice:
    """CLHS Figure 9-1 relationships that type-based handler dispatch needs."""

    def test_error_is_a_serious_condition(self, env):
        assert eval_string("(typep (make-condition 'simple-error) 'serious-condition)",
                           env) == lisptype.T

    def test_storage_condition_is_serious_but_not_an_error(self, env):
        assert issubclass(lisptype.StorageCondition, lisptype.SeriousCondition)
        assert not issubclass(lisptype.StorageCondition, lisptype.Error)

    def test_unbound_variable_is_a_cell_error(self, env):
        assert issubclass(lisptype.UnboundVariable, lisptype.CellError)
        assert issubclass(lisptype.UndefinedFunction, lisptype.CellError)
        assert issubclass(lisptype.UnboundSlot, lisptype.CellError)

    def test_reader_error_is_both_parse_error_and_stream_error(self, env):
        assert issubclass(lisptype.ReaderError, lisptype.ParseError)
        assert issubclass(lisptype.ReaderError, lisptype.StreamError)

    def test_style_warning_is_a_warning_not_an_error(self, env):
        assert issubclass(lisptype.StyleWarning, lisptype.Warning)
        assert not issubclass(lisptype.StyleWarning, lisptype.Error)

    def test_simple_condition_owns_the_format_control_slots(self, env):
        """SIMPLE-ERROR/SIMPLE-WARNING inherit FORMAT-CONTROL from
        SIMPLE-CONDITION rather than each redefining it."""
        for cls in (lisptype.SimpleCondition, lisptype.SimpleError, lisptype.SimpleWarning):
            condition = cls(format_control="~A!", format_arguments=[1])
            assert condition.get_slot('format-control') == "~A!"
            assert condition.get_slot('format-arguments') == [1]

    def test_non_condition_type_names_do_not_resolve_to_condition_classes(self, env):
        """_condition_class_for_name maps by naming convention over lisptype's
        namespace, which also holds non-condition classes; it must reject them."""
        from fclpy.lispfunc.evaluation_conditions import _condition_class_for_name

        assert _condition_class_for_name('PACKAGE') is None
        assert _condition_class_for_name('ENVIRONMENT') is None
        assert _condition_class_for_name('ERROR') is lisptype.Error


class TestSimpleConditionReport:
    def test_report_applies_format_control_to_arguments(self, env):
        """A simple condition reports by applying FORMAT to its control and
        arguments, while the slots keep the unrendered control."""
        condition = eval_string('(make-condition \'simple-error '
                                ':format-control "value: ~A" :format-arguments (list 7))',
                                env)
        assert condition.get_slot('format-control') == "value: ~A"
        assert str(condition) == "value: 7"

    def test_signaled_condition_report_is_formatted(self, env):
        result = eval_string("""
            (handler-case (error "value: ~A" 7)
              (error (c) (format nil "~A" c)))
        """, env)
        assert str(result) == "value: 7"
