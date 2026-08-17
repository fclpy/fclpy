"""Special form registry stubs.

These are stub implementations registered with the registry so that
special forms appear as bound. The actual evaluation is handled by
the eval dispatcher in evaluation_core.py.
"""

import fclpy.lisptype as lisptype
from . import registry as _registry


@_registry.cl_special('FUNCTION')
def function_fn(name):
    """FUNCTION special form (stub returning name)."""
    return name


@_registry.cl_special('QUOTE')
def quote_fn(expression):
    return expression


@_registry.cl_special('DEFMACRO')
def special_defmacro(*args):
    """DEFMACRO special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('DEFMACRO (evaluated in evaluator)')


@_registry.cl_special('DECLARE')
def special_declare(*args):
    """DECLARE special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('DECLARE (evaluated in evaluator)')


@_registry.cl_special('DECLAIM')
def special_declaim(*args):
    """DECLAIM special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('DECLAIM (evaluated in evaluator)')


# Register remaining special forms as stubs; real semantics handled in eval dispatcher.
@_registry.cl_special('IF')
def special_if(*args):
    """IF special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('IF (evaluated in evaluator)')


@_registry.cl_special('COND')
def special_cond(*args):
    """COND special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('COND (evaluated in evaluator)')


@_registry.cl_special('DEFUN')
def special_defun(*args):
    """DEFUN special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('DEFUN (evaluated in evaluator)')


@_registry.cl_special('SETQ')
def special_setq(*args):
    """SETQ special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('SETQ (evaluated in evaluator)')


@_registry.cl_special('SETF')
def special_setf(*args):
    """SETF special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('SETF (evaluated in evaluator)')


@_registry.cl_special('DEFVAR')
def special_defvar(*args):
    """DEFVAR special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('DEFVAR (evaluated in evaluator)')


@_registry.cl_special('LET')
def special_let(*args):
    """LET special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('LET (evaluated in evaluator)')


@_registry.cl_special('WHEN')
def special_when(*args):
    """WHEN special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('WHEN (evaluated in evaluator)')


@_registry.cl_special('FLET')
def special_flet(*args):
    """FLET special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('FLET (evaluated in evaluator)')


@_registry.cl_special('LABELS')
def special_labels(*args):
    """LABELS special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('LABELS (evaluated in evaluator)')


@_registry.cl_special('SIGNAL')
def special_signal(*args):
    """SIGNAL special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('SIGNAL (evaluated in evaluator)')


@_registry.cl_special('ERROR')
def special_error(*args):
    """ERROR special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('ERROR (evaluated in evaluator)')


@_registry.cl_special('CERROR')
def special_cerror(*args):
    """CERROR special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('CERROR (evaluated in evaluator)')


@_registry.cl_special('WARN')
def special_warn(*args):
    """WARN special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('WARN (evaluated in evaluator)')


@_registry.cl_special('RESTART-CASE')
def special_restart_case(*args):
    """RESTART-CASE special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('RESTART-CASE (evaluated in evaluator)')


@_registry.cl_special('RESTART-BIND')
def special_restart_bind(*args):
    """RESTART-BIND special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('RESTART-BIND (evaluated in evaluator)')


@_registry.cl_special('INVOKE-RESTART')
def special_invoke_restart(*args):
    """INVOKE-RESTART special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('INVOKE-RESTART (evaluated in evaluator)')


@_registry.cl_special('ABORT')
def special_abort(*args):
    """ABORT special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('ABORT (evaluated in evaluator)')


@_registry.cl_special('HANDLER-BIND')
def special_handler_bind(*args):
    """HANDLER-BIND special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('HANDLER-BIND (evaluated in evaluator)')


@_registry.cl_special('HANDLER-CASE')
def special_handler_case(*args):
    """HANDLER-CASE special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('HANDLER-CASE (evaluated in evaluator)')


# WITH-OPEN-FILE is implemented as a macro (registered at import time
# by `evaluation_special_forms`). Do not register it as a special.


@_registry.cl_special('LOOP-FINISH')
def special_loop_finish(*args):
    """LOOP-FINISH special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('LOOP-FINISH (evaluated in evaluator)')


@_registry.cl_special('INLINE')
def special_inline(*args):
    """INLINE declaration (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('INLINE (evaluated in evaluator)')


@_registry.cl_special('IGNORE')
def special_ignore(*args):
    """IGNORE declaration (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('IGNORE (evaluated in evaluator)')


@_registry.cl_special('IGNORABLE')
def special_ignorable(*args):
    """IGNORABLE declaration (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('IGNORABLE (evaluated in evaluator)')


# Additional special forms handled by evaluator but not previously registered
@_registry.cl_special('LET*')
def special_let_star(*args):
    """LET* special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('LET* (evaluated in evaluator)')


@_registry.cl_special('UNLESS')
def special_unless(*args):
    """UNLESS special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('UNLESS (evaluated in evaluator)')


@_registry.cl_special('PROGN')
def special_progn(*args):
    """PROGN special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('PROGN (evaluated in evaluator)')


@_registry.cl_special('AND')
def special_and(*args):
    """AND special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('AND (evaluated in evaluator)')


@_registry.cl_special('OR')
def special_or(*args):
    """OR special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('OR (evaluated in evaluator)')


@_registry.cl_special('PROG1')
def special_prog1(*args):
    """PROG1 special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('PROG1 (evaluated in evaluator)')


@_registry.cl_special('PROG2')
def special_prog2(*args):
    """PROG2 special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('PROG2 (evaluated in evaluator)')


@_registry.cl_special('PROG')
def special_prog(*args):
    """PROG special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('PROG (evaluated in evaluator)')


@_registry.cl_special('PROG*')
def special_prog_star(*args):
    """PROG* special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('PROG* (evaluated in evaluator)')


@_registry.cl_special('DEFPARAMETER')
def special_defparameter(*args):
    """DEFPARAMETER special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('DEFPARAMETER (evaluated in evaluator)')


@_registry.cl_special('DEFSTRUCT')
def special_defstruct(*args):
    """DEFSTRUCT special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('DEFSTRUCT (evaluated in evaluator)')


@_registry.cl_special('DEFINE-METHOD-COMBINATION')
def special_define_method_combination(*args):
    """DEFINE-METHOD-COMBINATION special form (handled by evaluator).

    It has to be a special operator here rather than the `cl_function` that
    used to live in `utilities_errors.py`: none of its subforms may be
    evaluated -- the long form's body is what *computes* an effective
    method, per generic-function call."""
    raise lisptype.LispNotImplementedError('DEFINE-METHOD-COMBINATION (evaluated in evaluator)')


@_registry.cl_special('LOOP')
def special_loop(*args):
    """LOOP special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('LOOP (evaluated in evaluator)')


@_registry.cl_special('POP')
def special_pop(*args):
    """POP special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('POP (evaluated in evaluator)')


@_registry.cl_special('PUSH')
def special_push(*args):
    """PUSH special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('PUSH (evaluated in evaluator)')


@_registry.cl_special('PUSHNEW')
def special_pushnew(*args):
    """PUSHNEW special form (handled by evaluator).

    Was a `cl_function` (`lispfunc.sequences_higher.pushnew`) that received
    `place` already evaluated to a value -- so it could never write the
    result back anywhere except a Python-list-backed variable, and it
    ignored :test/:key/:test-not entirely. `place` is a place designator,
    not a value; PUSHNEW must see it unevaluated the way PUSH/POP already
    do (plan.md C16)."""
    raise lisptype.LispNotImplementedError('PUSHNEW (evaluated in evaluator)')


@_registry.cl_special('LAMBDA')
def special_lambda(*args):
    """LAMBDA special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('LAMBDA (evaluated in evaluator)')


@_registry.cl_special('QUASIQUOTE')
def special_quasiquote(*args):
    """QUASIQUOTE special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('QUASIQUOTE (evaluated in evaluator)')

@_registry.cl_special('SYMBOL-MACROLET')
def special_symbol_macrolet(*args):
    """SYMBOL-MACROLET special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('SYMBOL-MACROLET (evaluated in evaluator)')


@_registry.cl_special('MACROLET')
def special_macrolet(*args):
    """MACROLET special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('MACROLET (evaluated in evaluator)')


@_registry.cl_special('DEFSETF')
def special_defsetf(*args):
    """DEFSETF special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('DEFSETF (evaluated in evaluator)')


@_registry.cl_special('DEFINE-COMPILER-MACRO')
def special_define_compiler_macro(*args):
    """DEFINE-COMPILER-MACRO special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('DEFINE-COMPILER-MACRO (evaluated in evaluator)')


@_registry.cl_special('DEFINE-CONDITION')
def special_define_condition(*args):
    """DEFINE-CONDITION special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('DEFINE-CONDITION (evaluated in evaluator)')


@_registry.cl_special('DEFTYPE')
def special_deftype(*args):
    """DEFTYPE special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('DEFTYPE (evaluated in evaluator)')


@_registry.cl_special('MACROEXPAND-1')
def special_macroexpand_1(*args):
    """MACROEXPAND-1 special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('MACROEXPAND-1 (evaluated in evaluator)')


@_registry.cl_special('MACRO-FUNCTION')
def special_macro_function(*args):
    """MACRO-FUNCTION special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('MACRO-FUNCTION (evaluated in evaluator)')


@_registry.cl_special('BLOCK')
def special_block(*args):
    """BLOCK special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('BLOCK (evaluated in evaluator)')


@_registry.cl_special('RETURN-FROM')
def special_return_from(*args):
    """RETURN-FROM special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('RETURN-FROM (evaluated in evaluator)')


@_registry.cl_special('CATCH')
def special_catch(*args):
    """CATCH special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('CATCH (evaluated in evaluator)')


@_registry.cl_special('THROW')
def special_throw(*args):
    """THROW special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('THROW (evaluated in evaluator)')


@_registry.cl_special('UNWIND-PROTECT')
def special_unwind_protect(*args):
    """UNWIND-PROTECT special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('UNWIND-PROTECT (evaluated in evaluator)')


@_registry.cl_special('IGNORE-ERRORS')
def special_ignore_errors(*args):
    """IGNORE-ERRORS special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('IGNORE-ERRORS (evaluated in evaluator)')


@_registry.cl_special('TAGBODY')
def special_tagbody(*args):
    """TAGBODY special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('TAGBODY (evaluated in evaluator)')


@_registry.cl_special('GO')
def special_go(*args):
    """GO special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('GO (evaluated in evaluator)')


@_registry.cl_special('INCF')
def special_incf(*args):
    """INCF special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('INCF (evaluated in evaluator)')


@_registry.cl_special('DECF')
def special_decf(*args):
    """DECF special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('DECF (evaluated in evaluator)')


@_registry.cl_special('DO')
def special_do(*args):
    """DO special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('DO (evaluated in evaluator)')


@_registry.cl_special('DO*')
def special_do_star(*args):
    """DO* special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('DO* (evaluated in evaluator)')


@_registry.cl_special('DOLIST')
def special_dolist(*args):
    """DOLIST special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('DOLIST (evaluated in evaluator)')


@_registry.cl_special('DOTIMES')
def special_dotimes(*args):
    """DOTIMES special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('DOTIMES (evaluated in evaluator)')


__all__ = [
    'function_fn',
    'quote_fn',
    'special_defmacro',
    'special_declare',
    'special_declaim',
    'special_if',
    'special_cond',
    'special_defun',
    'special_setq',
    'special_defvar',
    'special_let',
    'special_when',
    'special_flet',
    'special_labels',
    'special_signal',
    'special_error',
    'special_cerror',
    'special_warn',
    'special_restart_case',
    'special_restart_bind',
    'special_invoke_restart',
    'special_abort',
    'special_handler_bind',
    'special_handler_case',
    'special_with_open_file',
    'special_loop_finish',
    'special_inline',
    'special_ignore',
    'special_ignorable',
    # New registrations
    'special_let_star',
    'special_unless',
    'special_progn',
    'special_and',
    'special_or',
    'special_prog1',
    'special_prog2',
    'special_defparameter',
    'special_defstruct',
    'special_loop',
    'special_pop',
    'special_lambda',
    'special_quasiquote',
    'special_macroexpand_1',
    'special_macro_function',
    'special_block',
    'special_return_from',
    'special_catch',
    'special_throw',
    'special_unwind_protect',
    'special_ignore_errors',
    'special_tagbody',
    'special_go',
    'special_incf',
    'special_decf',
    'special_do',
    'special_do_star',
    'special_dolist',
    'special_dotimes',
]
