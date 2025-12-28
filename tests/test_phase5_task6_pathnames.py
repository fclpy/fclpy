"""Tests for Phase 5 Task 6: Pathname Handling."""

import pytest
import os
import tempfile
from fclpy.lispfunc.pathnames import (
    Pathname, make_pathname, pathnamep, pathname_directory, pathname_name,
    pathname_type, namestring, file_namestring, directory_namestring,
    absolute_pathname_p, relative_pathname_p, probe_file, truename
)
import fclpy.lisptype as lisptype


class TestPathnameClass:
    """Test the Pathname class."""
    
    def test_create_pathname_simple(self):
        """Test creating a simple pathname."""
        path = Pathname("file.txt")
        assert path.filename == "file"
        assert path.extension == "txt"
        assert path.name == "file.txt"
    
    def test_create_pathname_with_directory(self):
        """Test creating pathname with directory."""
        path = Pathname(os.path.join("home", "user", "file.txt"))
        # Check components exist
        assert path.directory is not None
        assert path.filename == "file"
        assert path.extension == "txt"
        assert path.name == "file.txt"
    
    def test_create_pathname_no_extension(self):
        """Test pathname without extension."""
        path = Pathname("README")
        assert path.filename == "README"
        assert path.extension is None
        assert path.name == "README"
    
    def test_pathname_relative_path(self):
        """Test relative pathname."""
        path = Pathname("src/main.py")
        assert path.filename == "main"
        assert path.extension == "py"
    
    def test_pathname_str(self):
        """Test string representation."""
        path = Pathname("file.txt")
        assert "file.txt" in str(path)


class TestMakePathname:
    """Test PATHNAME and MAKE-PATHNAME functions."""
    
    def test_pathname_from_string(self):
        """Test PATHNAME function with string."""
        path = make_pathname("test.txt")
        assert isinstance(path, Pathname)
        assert path.name == "test.txt"
    
    def test_make_pathname_with_name_type(self):
        """Test MAKE-PATHNAME with name and type."""
        path = make_pathname(name="document", type="pdf")
        assert path.filename == "document"
        assert path.extension == "pdf"


class TestPathnameAccessors:
    """Test pathname accessor functions."""
    
    def test_pathname_directory(self):
        """Test PATHNAME-DIRECTORY."""
        path = Pathname(os.path.join("home", "user", "file.txt"))
        result = pathname_directory(path)
        # Directory should exist (not NIL)
        assert result is not lisptype.NIL
    
    def test_pathname_directory_nil(self):
        """Test PATHNAME-DIRECTORY returns NIL for relative path."""
        path = Pathname("file.txt")
        assert pathname_directory(path) == lisptype.NIL
    
    def test_pathname_name(self):
        """Test PATHNAME-NAME."""
        path = Pathname("file.txt")
        assert pathname_name(path) == "file"
    
    def test_pathname_type(self):
        """Test PATHNAME-TYPE."""
        path = Pathname("file.txt")
        assert pathname_type(path) == "txt"
    
    def test_pathname_type_nil(self):
        """Test PATHNAME-TYPE returns NIL."""
        path = Pathname("README")
        assert pathname_type(path) == lisptype.NIL


class TestPathnameNamestring:
    """Test namestring conversion functions."""
    
    def test_namestring(self):
        """Test NAMESTRING."""
        path = Pathname("file.txt")
        assert namestring(path) == "file.txt"
    
    def test_namestring_with_string(self):
        """Test NAMESTRING with string input."""
        assert namestring("path/file.txt") == "path/file.txt"
    
    def test_file_namestring(self):
        """Test FILE-NAMESTRING."""
        path = Pathname(os.path.join("home", "user", "file.txt"))
        assert file_namestring(path) == "file.txt"
    
    def test_directory_namestring(self):
        """Test DIRECTORY-NAMESTRING."""
        path = Pathname(os.path.join("home", "user", "file.txt"))
        result = directory_namestring(path)
        # Should have directory info
        assert result is not lisptype.NIL


class TestPathnameTests:
    """Test pathname test functions."""
    
    def test_pathnamep(self):
        """Test PATHNAMEP."""
        path = Pathname("file.txt")
        assert pathnamep(path) == lisptype.T
        assert pathnamep("not a pathname") == lisptype.NIL
    
    def test_absolute_pathname_p(self):
        """Test ABSOLUTE-PATHNAME-P."""
        abs_path = Pathname(os.path.abspath("file.txt"))
        rel_path = Pathname("file.txt")
        
        assert absolute_pathname_p(abs_path) == lisptype.T
        assert absolute_pathname_p(rel_path) == lisptype.NIL
    
    def test_relative_pathname_p(self):
        """Test RELATIVE-PATHNAME-P."""
        abs_path = Pathname(os.path.abspath("file.txt"))
        rel_path = Pathname("file.txt")
        
        assert relative_pathname_p(rel_path) == lisptype.T
        assert relative_pathname_p(abs_path) == lisptype.NIL


class TestPathnameFileOperations:
    """Test pathname file operation functions."""
    
    def test_probe_file_exists(self):
        """Test PROBE-FILE on existing file."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, encoding='utf-8') as f:
            fname = f.name
            f.write("test")
        
        try:
            result = probe_file(fname)
            assert isinstance(result, Pathname)
        finally:
            os.unlink(fname)
    
    def test_probe_file_not_exists(self):
        """Test PROBE-FILE on nonexistent file."""
        result = probe_file(os.path.join("nonexistent", "file.txt"))
        assert result == lisptype.NIL
    
    def test_file_write_date(self):
        """Test FILE-WRITE-DATE."""
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
        """Test FILE-WRITE-DATE on nonexistent file."""
        from fclpy.lispfunc.pathnames import file_write_date
        
        result = file_write_date(os.path.join("nonexistent", "file.txt"))
        assert result == lisptype.NIL


class TestPathnameDirectory:
    """Test DIRECTORY function."""
    
    def test_directory_list(self):
        """Test DIRECTORY listing."""
        from fclpy.lispfunc.pathnames import directory
        
        with tempfile.TemporaryDirectory() as tmpdir:
            # Create test files
            file1 = os.path.join(tmpdir, "file1.txt")
            file2 = os.path.join(tmpdir, "file2.txt")
            
            with open(file1, 'w') as f:
                f.write("test1")
            with open(file2, 'w') as f:
                f.write("test2")
            
            # List directory
            results = directory(tmpdir)
            
            # Should have 2 pathnames
            assert len(results) == 2
            assert all(isinstance(r, Pathname) for r in results)
    
    def test_directory_with_wildcard(self):
        """Test DIRECTORY with wildcard."""
        from fclpy.lispfunc.pathnames import directory
        
        with tempfile.TemporaryDirectory() as tmpdir:
            # Create test files
            file1 = os.path.join(tmpdir, "file1.txt")
            file2 = os.path.join(tmpdir, "file2.txt")
            other = os.path.join(tmpdir, "other.log")
            
            for fname in [file1, file2, other]:
                with open(fname, 'w') as f:
                    f.write("test")
            
            # Search with wildcard
            pattern = os.path.join(tmpdir, "*.txt")
            results = directory(pattern)
            
            # Should only match .txt files
            assert len(results) == 2


class TestPathnameCanonical:
    """Test TRUENAME for canonical pathnames."""
    
    def test_truename(self):
        """Test TRUENAME returns absolute canonical path."""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, encoding='utf-8') as f:
            fname = f.name
            f.write("test")
        
        try:
            result = truename(fname)
            assert isinstance(result, Pathname)
            assert absolute_pathname_p(result) == lisptype.T
        finally:
            os.unlink(fname)
    
    def test_truename_nonexistent(self):
        """Test TRUENAME on nonexistent file raises error."""
        with pytest.raises(FileNotFoundError):
            truename(os.path.join("nonexistent", "very_unlikely_file.txt"))


class TestPathnameIntegration:
    """Integration tests for pathname handling."""
    
    def test_pathname_round_trip(self):
        """Test creating pathname and converting back."""
        original = os.path.join("src", "main.py")
        path = Pathname(original)
        
        assert namestring(path) == original
    
    def test_pathname_with_all_components(self):
        """Test pathname with all components."""
        path = Pathname(os.path.join("home", "user", "documents", "report.pdf"))
        
        # Check directory exists
        result_dir = pathname_directory(path)
        assert result_dir is not lisptype.NIL
        
        assert pathname_name(path) == "report"
        assert pathname_type(path) == "pdf"
        assert file_namestring(path) == "report.pdf"
    
    def test_multiple_dot_filename(self):
        """Test pathname with multiple dots."""
        path = Pathname("archive.tar.gz")
        # Only last extension is captured
        assert pathname_name(path) == "archive.tar"
        assert pathname_type(path) == "gz"
    
    def test_pathname_case_preservation(self):
        """Test pathname preserves case."""
        path = Pathname("MyFile.TXT")
        assert "MyFile.TXT" in path.name
        assert pathname_name(path) == "MyFile"
        assert pathname_type(path) == "TXT"
