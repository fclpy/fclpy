"""Regression tests for DO-SYMBOLS / DO-EXTERNAL-SYMBOLS / DO-ALL-SYMBOLS.

Per CLHS, these three macros (like DO/DO*/DOLIST/DOTIMES) accept a body of
{tag | statement}* -- an implicit TAGBODY -- wrapped in an implicit block
named NIL. The evaluator previously ran the body as a flat sequence of forms
with neither GO-tag support nor RETURN support, so GO/RETURN inside these
forms either raised an uncaught GoException/ReturnFromException or silently
did the wrong thing. Mirrors ansi-test/packages/do-symbols.lsp (test 8),
do-external-symbols.lsp (test 8), and do-all-symbols.lsp (tests 5, 6, 7, 8, 12).
"""

import io

from fclpy import lisptype, lispenv
from fclpy.lispfunc.evaluation_core import eval as lisp_eval
from fclpy.lispreader import LispReader, LispStream
from fclpy.readtable import get_current_readtable


def setup_env():
    lispenv.setup_standard_environment()
    import fclpy.state as state
    return state.current_environment


def read_str(code):
    stream = LispStream(io.StringIO(code))
    readtable = get_current_readtable()
    reader = LispReader(readtable.get_macro_character, stream)
    return reader.read_1()


def read_and_eval(expr_str, env):
    form = read_str(expr_str)
    return lisp_eval(form, env)


def run(code):
    env = setup_env()
    read_and_eval('(defpackage "DS-TEST" (:export "A") )', env)
    read_and_eval('(intern "A" "DS-TEST")', env)
    read_and_eval('(intern "B" "DS-TEST")', env)
    read_and_eval('(export (intern "A" "DS-TEST") "DS-TEST")', env)
    return read_and_eval(code, env)


class TestDoSymbolsImplicitTagbody:
    def test_go_skips_and_loops_within_body(self):
        # Mirrors do-symbols.8: a tag jumps forward, and the loop naturally
        # visits both branches across iterations.
        result = run(
            '''
            (let ((x nil))
              (do-symbols (s "DS-TEST")
                (when (equal (symbol-name s) "B") (go bar))
                (push (symbol-name s) x)
                (go foo)
                bar
                (push "SKIPPED" x)
                foo))
            '''
        )
        assert result is lisptype.NIL

    def test_default_return_is_nil(self):
        result = run('(do-symbols (s "DS-TEST") (declare (ignore s)) t)')
        assert result is lisptype.NIL


class TestDoExternalSymbolsImplicitTagbody:
    def test_go_within_body(self):
        result = run(
            '''
            (do-external-symbols (s "DS-TEST")
              (when (equal (symbol-name s) "A") (go done))
              done)
            '''
        )
        assert result is lisptype.NIL


class TestDoAllSymbolsImplicitNilBlock:
    def test_bare_return_terminates_loop(self):
        # Mirrors do-all-symbols.6: a bare RETURN exits DO-ALL-SYMBOLS itself.
        result = run('(do-all-symbols (x :bad) (return :good))')
        assert isinstance(result, lisptype.LispSymbol) and result.name == 'GOOD'

    def test_return_from_nil_block_not_caught_by_outer_block(self):
        # Mirrors do-all-symbols.5: RETURN inside DO-ALL-SYMBOLS must not
        # escape past the form -- it exits DO-ALL-SYMBOLS's own NIL block,
        # letting the enclosing BLOCK NIL's body continue normally.
        result = run(
            '''
            (block nil
              (do-all-symbols (x (return :bad)))
              :good)
            '''
        )
        assert isinstance(result, lisptype.LispSymbol) and result.name == 'GOOD'

    def test_return_terminates_after_one_iteration(self):
        # Mirrors do-all-symbols.12: RETURN actually stops iteration rather
        # than merely being ignored on the first pass.
        result = run(
            '''
            (let ((should-have-returned nil))
              (block done
                (do-all-symbols (s :bad1)
                  (when should-have-returned
                    (return-from done :bad2))
                  (setq should-have-returned t)
                  (return :good))))
            '''
        )
        assert isinstance(result, lisptype.LispSymbol) and result.name == 'GOOD'

    def test_go_tag_is_local_to_each_iteration(self):
        # Mirrors do-all-symbols.7/8: GO inside the body must resolve to the
        # body's own implicit tagbody tag, not escape to an outer TAGBODY
        # that happens to share the tag name.
        result = run(
            '''
            (block done
              (tagbody
               (do-all-symbols (x (return-from done :good))
                 (go tag)
                 (return-from done :bad1)
                 tag)
               tag
               (return-from done :bad2)))
            '''
        )
        assert isinstance(result, lisptype.LispSymbol) and result.name == 'GOOD'
