"""Tests for the pathname component model (fclpy/lispfunc/pathnames.py).

`Pathname` is a component record (CLHS 19.2: host/device/directory/name/
type/version), not a namestring wrapper -- see the module docstring. These
tests exercise the Python-level API directly; `tests/` unit tests are a fast
regression net, not a substitute for the ANSI suite (`docs/ansi_checklist.md`
tracks `pathnames/` there).
"""

import pytest
import os
import tempfile
from fclpy.lispfunc.pathnames import (
    Pathname, pathname_from_namestring, pathname, pathnamep,
    pathname_directory, pathname_name, pathname_type,
    namestring, file_namestring, directory_namestring,
    absolute_pathname_p, relative_pathname_p, probe_file, truename,
)
import fclpy.lisptype as lisptype


class TestPathnameComponents:
    """Test the Pathname component record."""

    def test_create_pathname_simple(self):
        path = pathname_from_namestring("file.txt")
        assert path.name == "file"
        assert path.type == "txt"

    def test_create_pathname_with_directory(self):
        path = pathname_from_namestring("home/user/file.txt")
        assert path.directory is not None
        assert path.name == "file"
        assert path.type == "txt"

    def test_create_pathname_no_extension(self):
        path = pathname_from_namestring("README")
        assert path.name == "README"
        assert path.type is None

    def test_pathname_relative_path(self):
        path = pathname_from_namestring("src/main.py")
        assert path.name == "main"
        assert path.type == "py"

    def test_pathname_str(self):
        path = pathname_from_namestring("file.txt")
        assert "file.txt" in str(path)


class TestMakePathname:
    """Test PATHNAME and MAKE-PATHNAME."""

    def test_pathname_from_string(self):
        path = pathname("test.txt")
        assert isinstance(path, Pathname)
        assert path.name == "test"
        assert path.type == "txt"

    def test_pathname_identity(self):
        """PATHNAME is the identity on an existing Pathname (CLHS 19.4.1):
        `(eq x (pathname x))`."""
        original = pathname_from_namestring("test.txt")
        assert pathname(original) is original

    def test_make_pathname_with_name_type(self):
        from fclpy.lispfunc.pathnames import make_pathname_function
        path = make_pathname_function(name="document", type="pdf")
        assert path.name == "document"
        assert path.type == "pdf"


class TestPathnameAccessors:
    """Test pathname accessor functions."""

    def test_pathname_directory(self):
        path = pathname_from_namestring("home/user/file.txt")
        result = pathname_directory(path)
        assert result is not lisptype.NIL

    def test_pathname_directory_nil(self):
        path = pathname_from_namestring("file.txt")
        assert pathname_directory(path) == lisptype.NIL

    def test_pathname_name(self):
        path = pathname_from_namestring("file.txt")
        assert pathname_name(path) == "file"

    def test_pathname_type(self):
        path = pathname_from_namestring("file.txt")
        assert pathname_type(path) == "txt"

    def test_pathname_type_nil(self):
        path = pathname_from_namestring("README")
        assert pathname_type(path) == lisptype.NIL


class TestPathnameNamestring:
    """Test namestring conversion functions."""

    def test_namestring(self):
        path = pathname_from_namestring("file.txt")
        assert namestring(path) == "file.txt"

    def test_namestring_with_string(self):
        assert namestring("path/file.txt") == "path/file.txt"

    def test_file_namestring(self):
        path = pathname_from_namestring("home/user/file.txt")
        assert file_namestring(path) == "file.txt"

    def test_directory_namestring(self):
        path = pathname_from_namestring("home/user/file.txt")
        result = directory_namestring(path)
        assert result != ""


class TestPathnameTests:
    """Test pathname test functions."""

    def test_pathnamep(self):
        path = pathname_from_namestring("file.txt")
        assert pathnamep(path) == lisptype.T
        assert pathnamep("not a pathname") == lisptype.NIL

    def test_absolute_pathname_p(self):
        abs_path = pathname_from_namestring(os.path.abspath("file.txt"))
        rel_path = pathname_from_namestring("file.txt")

        assert absolute_pathname_p(abs_path) == lisptype.T
        assert absolute_pathname_p(rel_path) == lisptype.NIL

    def test_relative_pathname_p(self):
        abs_path = pathname_from_namestring(os.path.abspath("file.txt"))
        rel_path = pathname_from_namestring("file.txt")

        assert relative_pathname_p(rel_path) == lisptype.T
        assert relative_pathname_p(abs_path) == lisptype.NIL


class TestPathnameFileOperations:
    """Test pathname file operation functions."""

    def test_probe_file_exists(self):
        with tempfile.NamedTemporaryFile(mode='w', delete=False, encoding='utf-8') as f:
            fname = f.name
            f.write("test")

        try:
            result = probe_file(fname)
            assert isinstance(result, Pathname)
        finally:
            os.unlink(fname)

    def test_probe_file_not_exists(self):
        result = probe_file(os.path.join("nonexistent", "file.txt"))
        assert result == lisptype.NIL

    def test_file_write_date(self):
        from fclpy.lispfunc.pathnames import file_write_date

        with tempfile.NamedTemporaryFile(mode='w', delete=False, encoding='utf-8') as f:
            fname = f.name
            f.write("test")

        try:
            result = file_write_date(fname)
            assert isinstance(result, int)
            assert result > 0
        finally:
            os.unlink(fname)

    def test_file_write_date_nonexistent(self):
        from fclpy.lispfunc.pathnames import file_write_date

        result = file_write_date(os.path.join("nonexistent", "file.txt"))
        assert result == lisptype.NIL


class TestPathnameDirectory:
    """Test DIRECTORY function."""

    def test_directory_list(self):
        from fclpy.lispfunc.pathnames import directory
        from fclpy.lispfunc.sequence_protocol import list_elements

        with tempfile.TemporaryDirectory() as tmpdir:
            file1 = os.path.join(tmpdir, "file1.txt")
            file2 = os.path.join(tmpdir, "file2.txt")

            with open(file1, 'w') as f:
                f.write("test1")
            with open(file2, 'w') as f:
                f.write("test2")

            results = list_elements(directory(tmpdir))

            assert len(results) == 2
            assert all(isinstance(r, Pathname) for r in results)

    def test_directory_with_wildcard(self):
        from fclpy.lispfunc.pathnames import directory
        from fclpy.lispfunc.sequence_protocol import list_elements

        with tempfile.TemporaryDirectory() as tmpdir:
            file1 = os.path.join(tmpdir, "file1.txt")
            file2 = os.path.join(tmpdir, "file2.txt")
            other = os.path.join(tmpdir, "other.log")

            for fname in [file1, file2, other]:
                with open(fname, 'w') as f:
                    f.write("test")

            pattern = os.path.join(tmpdir, "*.txt")
            results = list_elements(directory(pattern))

            assert len(results) == 2


class TestPathnameCanonical:
    """Test TRUENAME for canonical pathnames."""

    def test_truename(self):
        with tempfile.NamedTemporaryFile(mode='w', delete=False, encoding='utf-8') as f:
            fname = f.name
            f.write("test")

        try:
            result = truename(fname)
            assert isinstance(result, Pathname)
            assert absolute_pathname_p(result) == lisptype.T
        finally:
            os.unlink(fname)

    def test_truename_nonexistent_signals_file_error(self):
        """TRUENAME signals a FILE-ERROR naming the file (CLHS TRUENAME).

        It used to raise Python's `FileNotFoundError`, which is not a
        condition: no HANDLER-CASE clause matched it, so it surfaced as the
        *value* of the form instead of being handled.
        """
        from fclpy.lispfunc.evaluation_core import ConditionException
        missing = os.path.join("nonexistent", "very_unlikely_file.txt")
        with pytest.raises(ConditionException) as excinfo:
            truename(missing)
        condition = excinfo.value.condition
        assert isinstance(condition, lisptype.FileError)
        assert condition.get_slot('pathname') == missing


class TestPathnameIntegration:
    """Integration tests for pathname handling."""

    def test_pathname_round_trip(self):
        original = os.path.join("src", "main.py").replace(os.sep, '/')
        path = pathname_from_namestring(original)

        assert namestring(path) == original

    def test_pathname_with_all_components(self):
        path = pathname_from_namestring("home/user/documents/report.pdf")

        result_dir = pathname_directory(path)
        assert result_dir is not lisptype.NIL

        assert pathname_name(path) == "report"
        assert pathname_type(path) == "pdf"
        assert file_namestring(path) == "report.pdf"

    def test_multiple_dot_filename(self):
        """Name/type split on the *last* dot."""
        path = pathname_from_namestring("archive.tar.gz")
        assert pathname_name(path) == "archive.tar"
        assert pathname_type(path) == "gz"

    def test_pathname_case_preservation(self):
        path = pathname_from_namestring("MyFile.TXT")
        assert pathname_name(path) == "MyFile"
        assert pathname_type(path) == "TXT"

    def test_pathname_equal_by_components(self):
        """Two pathnames built differently but naming the same components
        are EQUAL (CLHS 19.2), independent of how each was constructed."""
        from fclpy.lispfunc.pathnames import make_pathname_function
        a = pathname_from_namestring("foo.txt")
        b = make_pathname_function(name="foo", type="txt")
        assert a == b

    def test_wild_and_unspecified_components_are_distinct(self):
        """A `:wild` component is not the same as an unspecified (NIL) one."""
        from fclpy.lispfunc.pathnames import make_pathname_function, wild_pathname_p
        wild = make_pathname_function(name=lisptype.intern_keyword('WILD'))
        plain = make_pathname_function()
        assert wild_pathname_p(wild) == lisptype.T
        assert wild_pathname_p(plain) == lisptype.NIL
        assert wild != plain
