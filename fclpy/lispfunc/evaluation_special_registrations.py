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


@_registry.cl_special('WITH-OPEN-FILE')
def special_with_open_file(*args):
    """WITH-OPEN-FILE special form (handled by evaluator)."""
    raise lisptype.LispNotImplementedError('WITH-OPEN-FILE (evaluated in evaluator)')


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
]
