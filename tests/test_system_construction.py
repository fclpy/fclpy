"""LOAD, COMPILE-FILE, PROVIDE/REQUIRE and the mechanisms they exposed.

Every test here is written through the *Lisp* surface, because each mechanism
it covers was broken in a way that a direct Python call would have hidden:

* `LOAD` accepted only a filename, so a stream `filespec` became the
  *pathname* ``"<StringInputStream pos=0 len=59>"``;
* `WITH-OUTPUT-TO-STRING (var string)` bound `var` to a throwaway stream and
  never transferred its contents into `string`, so every assertion about what
  something *prints* compared against the empty string;
* `&rest` was bound to only the arguments before the first keyword-shaped
  value, so `(defun f (a &rest args &key ...) ...)` forwarded nothing;
* a copied readtable's built-in reader functions still read through the
  readtable they were copied from, so `set-macro-character` on a copy was
  invisible inside any list;
* `FMAKUNBOUND` unlinked a function binding but left the name cache
  `FBOUNDP` consults first.
"""

import io
import os

import pytest

from fclpy import lispenv, lisptype
from fclpy.lispfunc.evaluation_core import eval as lisp_eval
from fclpy.lispreader import LispReader, LispStream
from fclpy.readtable import get_current_readtable


@pytest.fixture(autouse=True)
def env():
    """A freshly bootstrapped standard environment."""
    lispenv.setup_standard_environment()
    import fclpy.state as state
    return state.current_environment


def ev(source):
    """Read and evaluate `source`, returning the value as a Python object."""
    import fclpy.state as state
    stream = LispStream(io.StringIO(source))
    readtable = get_current_readtable()
    form = LispReader(readtable.get_macro_character, stream).read_1()
    return lisp_eval(form, state.current_environment)


def evs(source):
    """Read and evaluate `source`, returning the value as a Python string."""
    return str(ev(source))


class TestModules:
    """`*MODULES*`, PROVIDE and REQUIRE (CLHS 24.1.5)."""

    def test_modules_starts_bound_to_the_empty_list(self):
        # It had no value at all, so *every* modules.lsp test failed on the
        # bare reference before reaching what it was testing.
        assert evs('*modules*') == 'NIL'
        assert evs("(every #'stringp *modules*)") == 'T'

    def test_provide_records_the_module_name_string(self):
        assert evs('(let ((*modules* *modules*))'
                   '  (provide "FOO")'
                   "  (not (not (member \"FOO\" *modules* :test #'string=))))") == 'T'

    def test_provide_is_idempotent(self):
        assert evs('(let ((*modules* *modules*))'
                   '  (provide "FOO") (provide "FOO")'
                   "  (count \"FOO\" *modules* :test #'string=))") == '1'

    @pytest.mark.parametrize('designator', ['"FOO"', ':|FOO|', "'|FOO|"])
    def test_a_module_name_is_a_string_designator(self, designator):
        assert evs('(let ((*modules* *modules*))'
                   '  (provide %s)'
                   "  (not (not (member \"FOO\" *modules* :test #'string=))))"
                   % designator) == 'T'

    def test_provide_leaves_the_outer_binding_alone(self):
        ev('(let ((*modules* *modules*)) (provide "FOO"))')
        assert evs('*modules*') == 'NIL'

    def test_require_of_a_provided_module_does_nothing(self):
        assert evs('(let ((*modules* *modules*))'
                   '  (provide "FOO") (require "FOO") :ok)') == ':OK'

    def test_require_of_an_unknown_module_signals(self):
        # The stub returned the module name, i.e. reported success for a
        # module that was never loaded.
        assert evs('(handler-case (require "NO-SUCH-MODULE-XYZ")'
                   '  (error (c) :signalled))') == ':SIGNALLED'


class TestWithCompilationUnit:
    """WITH-COMPILATION-UNIT is a macro, so it has the body's values."""

    def test_empty_body_is_nil(self):
        assert evs('(with-compilation-unit ())') == 'NIL'

    def test_returns_all_the_body_values(self):
        assert evs('(multiple-value-list'
                   ' (with-compilation-unit () (values 1 2 3)))') == '(1 2 3)'

    def test_no_values_stays_no_values(self):
        assert evs('(multiple-value-list'
                   ' (with-compilation-unit () (values)))') == 'NIL'

    def test_options_are_not_a_function_call(self):
        # As a `cl_function` its option list `(:OVERRIDE NIL)` was *evaluated*,
        # so this signalled UNDEFINED-FUNCTION OVERRIDE.
        assert evs('(with-compilation-unit (:override nil) :foo)') == ':FOO'

    def test_a_non_local_exit_out_of_the_body_works(self):
        assert evs('(let ((x nil))'
                   '  (list (block done'
                   '          (with-compilation-unit (:override nil)'
                   '            (setq x 1) (return-from done 2) (setq x 2)))'
                   '        x))') == '(2 1)'


class TestFillPointerOutputStream:
    """`(WITH-OUTPUT-TO-STRING (var string) ...)` appends to `string`."""

    def test_output_reaches_the_supplied_string(self):
        assert evs("""(let ((s (make-array '(0) :element-type 'character
                                            :adjustable t :fill-pointer 0)))
                        (with-output-to-string (*standard-output* s)
                          (princ "hi"))
                        s)""") == 'hi'

    def test_the_form_returns_the_body_value_not_the_text(self):
        assert evs("""(let ((s (make-array '(0) :element-type 'character
                                            :adjustable t :fill-pointer 0)))
                        (with-output-to-string (*standard-output* s) :body))""") == ':BODY'

    def test_text_written_before_a_non_local_exit_is_kept(self):
        assert evs("""(let ((s (make-array '(0) :element-type 'character
                                            :adjustable t :fill-pointer 0)))
                        (block done
                          (with-output-to-string (*standard-output* s)
                            (princ "partial")
                            (return-from done nil)))
                        s)""") == 'partial'


class TestFillPointerIsTheStringsLength:
    """A fill-pointered string's *content* stops at the fill pointer."""

    def test_str_and_length_agree(self):
        s = lisptype.LispString('FOOZZZZ')
        s.fill_pointer = 3
        assert str(s) == 'FOO'
        assert len(s) == 3
        assert repr(s) == '"FOO"'


class TestSymbolp:
    """SYMBOLP and TYPEP answer the same lattice question."""

    @pytest.mark.parametrize('form', ['nil', 't', ':foo', "'a"])
    def test_symbols_are_symbols(self, form):
        assert evs('(symbolp %s)' % form) == 'T'
        assert evs("(typep %s 'symbol)" % form) == 'T'

    @pytest.mark.parametrize('form', ['3', '"a"', "#\\a", "'(a)"])
    def test_non_symbols_are_not(self, form):
        assert evs('(symbolp %s)' % form) == 'NIL'

    def test_keywords_are_keywords_and_nil_is_not(self):
        assert evs('(keywordp :foo)') == 'T'
        assert evs('(keywordp nil)') == 'NIL'
        assert evs("(keywordp 'a)") == 'NIL'


class TestRestAndKeyInUserLambdaLists:
    """`&rest` gets every remaining argument (CLHS 3.4.1)."""

    def test_rest_captures_keyword_arguments_too(self):
        assert evs('(progn (defun %f (a &rest args &key b) (list a args b))'
                   '       (%f 1 :b 2))') == '(1 (:B 2) 2)'

    def test_rest_captures_keyword_shaped_values_with_no_key_parameters(self):
        # This bound ARGS to NIL: the keyword region was located by scanning
        # the *arguments* for the first keyword-shaped value.
        assert evs('(progn (defun %g (a &rest args) (list a args))'
                   '       (%g 1 :b 2))') == '(1 (:B 2))'

    def test_a_key_parameter_may_name_its_keyword_separately(self):
        assert evs('(progn (defun %h (&key ((:x y) 9)) y)'
                   '       (%h :x 3))') == '3'

    def test_supplied_p_still_works(self):
        assert evs('(progn (defun %i (&key (b nil b-p)) (list b b-p))'
                   '       (list (%i) (%i :b 2)))') == '((NIL NIL) (2 T))'

    def test_leftmost_pair_wins_for_a_repeated_keyword(self):
        assert evs('(progn (defun %j (&key b) b) (%j :b 1 :b 2))') == '1'

    def test_an_unrecognized_keyword_is_a_program_error(self):
        assert evs('(progn (defun %k (&key b) b)'
                   '       (handler-case (%k :bad 1)'
                   '         (program-error (c) :pe)))') == ':PE'

    def test_allow_other_keys_suppresses_that_error(self):
        assert evs('(progn (defun %l (&key b &allow-other-keys) b)'
                   '       (%l :bad 1 :b 7))') == '7'
        assert evs('(progn (defun %m (&key b) b)'
                   '       (%m :bad 1 :b 7 :allow-other-keys t))') == '7'


class TestCopiedReadtablesAreReadtables:
    """A copy's built-in readers read through the *copy* (CLHS 23.2)."""

    def test_a_macro_character_set_on_a_copy_works_inside_a_list(self):
        assert evs(r"""(let ((*readtable* (copy-readtable nil)))
                         (set-macro-character #\! (get-macro-character #\'))
                         (read-from-string "(list 1 !good)"))""") \
            == '(LIST 1 (QUOTE GOOD))'

    def test_the_original_readtable_is_unaffected(self):
        ev(r"""(let ((*readtable* (copy-readtable nil)))
                 (set-macro-character #\! (get-macro-character #\')))""")
        assert evs(r'(get-macro-character #\!)') == 'NIL'


class TestReadFromString:
    """READ-FROM-STRING reads through a string input stream (CLHS 23.2)."""

    def test_a_lisp_string_argument_works(self):
        # `io.StringIO(LispString)` raised a Python TypeError that surfaced as
        # the form's value.
        assert evs('(read-from-string (concatenate \'string "(1 " "2)"))') == '(1 2)'

    def test_it_returns_the_stopping_index_as_a_second_value(self):
        assert evs('(multiple-value-list (read-from-string "  abc  "))') == '(ABC 6)'


class TestUncaughtThrowIsAControlError:
    """CLHS 5.2: a THROW with no outstanding catcher signals CONTROL-ERROR."""

    def test_no_catcher(self):
        assert evs("(handler-case (throw 'nope 1)"
                   '  (control-error (c) :ce))') == ':CE'

    def test_a_catcher_further_out_still_catches(self):
        assert evs("(catch 'a (catch 'b (throw 'a 7)))") == '7'

    def test_a_catcher_is_no_longer_outstanding_after_it_returns(self):
        ev("(catch 'a 1)")
        assert evs("(handler-case (throw 'a 1) (control-error (c) :ce))") == ':CE'


class TestFmakunbound:
    """FMAKUNBOUND removes the definition and returns its argument."""

    def test_it_removes_a_symbol_function_definition(self):
        assert evs('(let ((g (gensym)))'
                   "  (setf (symbol-function g) #'car)"
                   '  (list (not (not (fboundp g)))'
                   '        (eq (fmakunbound g) g)'
                   '        (fboundp g)))') == '(T T NIL)'

    def test_it_removes_a_defun_definition(self):
        assert evs('(progn (defun %fmu () 1)'
                   "  (list (not (not (fboundp '%fmu)))"
                   "        (fmakunbound '%fmu)"
                   "        (fboundp '%fmu)))") == '(T %FMU NIL)'

    def test_a_non_function_name_is_a_type_error(self):
        assert evs('(handler-case (fmakunbound 1) (type-error (c) :te))') == ':TE'


class TestPathnameEquality:
    """Two pathnames naming the same file are EQUAL (CLHS 5.3)."""

    def test_equal_and_equalp(self):
        assert evs('(not (not (equal #p"foo.txt" #p"foo.txt")))') == 'T'
        assert evs('(not (not (equalp #p"foo.txt" #p"foo.txt")))') == 'T'
        assert evs('(equal #p"foo.txt" #p"bar.txt")') == 'NIL'
        assert evs('(equal #p"foo.txt" "foo.txt")') == 'NIL'


class TestFileErrors:
    """A missing file is a FILE-ERROR carrying its pathname, not a Python
    `FileNotFoundError` (which matches no handler)."""

    def test_load_signals_file_error(self):
        assert evs('(handler-case (load "no-such-file-xyz.lsp")'
                   '  (file-error (c) :fe))') == ':FE'

    def test_load_if_does_not_exist_nil_returns_nil(self):
        assert evs('(load "no-such-file-xyz.lsp" :if-does-not-exist nil)') == 'NIL'

    def test_load_rejects_an_unknown_keyword(self):
        assert evs('(handler-case (load "x.lsp" :bad-key-arg t)'
                   '  (program-error (c) :pe))') == ':PE'

    def test_compile_file_signals_file_error(self):
        assert evs('(handler-case (compile-file "no-such-file-xyz.lsp")'
                   '  (file-error (c) :fe))') == ':FE'

    def test_file_error_pathname_returns_the_slot_unchanged(self):
        # Not coerced to a pathname: CLHS specifies the reader of a slot, and
        # the suite passes namestrings, pathnames and streams as `:pathname`.
        assert evs('(file-error-pathname'
                   " (make-condition 'file-error :pathname \"foo.txt\"))") == 'foo.txt'


class TestCompileFileDoesNotRunTheProgram:
    """COMPILE-FILE processes top-level forms; it does not evaluate them."""

    def test_a_defun_in_the_file_is_not_defined_by_compiling_it(self, tmp_path):
        source = tmp_path / 'cf-test.lsp'
        source.write_text('(defun %cf-not-defined () :nope)\n', encoding='utf-8')
        ev('(compile-file #p"%s")' % str(source).replace('\\', '/'))
        assert evs("(fboundp '%cf-not-defined)") == 'NIL'

    def test_an_eval_when_compile_toplevel_form_is_evaluated(self, tmp_path):
        source = tmp_path / 'cf-test-2.lsp'
        source.write_text('(eval-when (:compile-toplevel)'
                          ' (defparameter *%cf-ran* :yes))\n', encoding='utf-8')
        ev('(compile-file #p"%s")' % str(source).replace('\\', '/'))
        assert evs('*%cf-ran*') == ':YES'

    def test_the_output_loads_back(self, tmp_path):
        source = tmp_path / 'cf-test-3.lsp'
        source.write_text('(defun %cf-loaded () :good)\n', encoding='utf-8')
        target = evs('(namestring (compile-file #p"%s"))'
                     % str(source).replace('\\', '/'))
        assert os.path.exists(target)
        ev('(load #p"%s")' % target.replace('\\', '/'))
        assert evs('(%cf-loaded)') == ':GOOD'

    def test_read_time_evaluation_is_resolved_at_compile_time(self, tmp_path):
        # `#.` is read-time evaluation, so a byte-copying COMPILE-FILE defers
        # it to load time, when `*compile-file-truename*` is NIL.
        source = tmp_path / 'cf-test-4.lsp'
        source.write_text("(defun %cf-truename () '#.*compile-file-truename*)\n",
                          encoding='utf-8')
        target = evs('(namestring (compile-file #p"%s"))'
                     % str(source).replace('\\', '/'))
        ev('(load #p"%s")' % target.replace('\\', '/'))
        assert evs('(not (not (pathnamep (%cf-truename))))') == 'T'


class TestLoadBindsRatherThanAssigns:
    """LOAD binds `*PACKAGE*` and `*READTABLE*` for the load's extent."""

    def test_a_load_from_a_stream_works_at_all(self):
        assert evs('(with-input-from-string'
                   '   (s "(defparameter *%ld-var* :loaded)")'
                   '  (load s))') == 'T'
        assert evs('*%ld-var*') == ':LOADED'

    def test_an_in_package_inside_the_load_is_undone(self):
        before = evs('(package-name *package*)')
        ev('(with-input-from-string (s "(in-package :keyword)") (load s))')
        assert evs('(package-name *package*)') == before

    def test_a_setq_of_package_inside_the_load_governs_later_forms(self):
        # The reader interns through `*PACKAGE*`; a plain SETQ writes the
        # variable's value cell and never touched `state.current_package`, so
        # every later form was read in the *old* package.
        assert evs('(with-input-from-string'
                   '   (s "(setq *package* (find-package \\"KEYWORD\\"))'
                   '       (defparameter zz \'in-keyword)")'
                   '  (load s))') == 'T'
        assert evs('(symbol-package :zz)') != 'NIL'
