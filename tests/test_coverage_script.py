"""Tests for the coverage reporting script."""

import pytest
import subprocess
import json
import sys
from pathlib import Path


class TestCoverageScript:
    """Test the coverage.py script functionality."""
    
    def test_coverage_script_runs(self):
        """Coverage script should run without errors."""
        script_path = Path(__file__).parent.parent / 'scripts' / 'coverage.py'
        
        result = subprocess.run(
            [sys.executable, str(script_path), '--detail', '0'],
            capture_output=True,
            text=True
        )
        
        assert result.returncode == 0
        assert 'Coverage Report' in result.stdout
    
    def test_coverage_script_markdown_output(self):
        """Coverage script should output markdown table."""
        script_path = Path(__file__).parent.parent / 'scripts' / 'coverage.py'
        
        result = subprocess.run(
            [sys.executable, str(script_path), '--markdown'],
            capture_output=True,
            text=True
        )
        
        assert result.returncode == 0
        # Check for markdown table format
        assert '|' in result.stdout
        assert 'Category' in result.stdout
        assert 'Implemented' in result.stdout
    
    def test_coverage_script_json_output(self):
        """Coverage script should output valid JSON."""
        script_path = Path(__file__).parent.parent / 'scripts' / 'coverage.py'
        
        result = subprocess.run(
            [sys.executable, str(script_path), '--json'],
            capture_output=True,
            text=True
        )
        
        assert result.returncode == 0
        # Try to parse JSON
        data = json.loads(result.stdout)
        
        # Check that expected keys exist
        assert 'targets' in data
        assert 'implemented' in data
        assert 'coverage_percent' in data
        assert 'missing_symbols' in data
    
    def test_coverage_script_minimum_coverage_check(self):
        """Coverage script should fail if coverage is below minimum."""
        script_path = Path(__file__).parent.parent / 'scripts' / 'coverage.py'
        
        # Set a very high minimum that won't be met
        result = subprocess.run(
            [sys.executable, str(script_path), '--min-coverage', '99'],
            capture_output=True,
            text=True
        )
        
        assert result.returncode == 1
        assert 'ERROR' in result.stdout
    
    def test_coverage_script_meets_minimum_coverage(self):
        """Coverage script should pass if coverage meets minimum."""
        script_path = Path(__file__).parent.parent / 'scripts' / 'coverage.py'
        
        # Set a minimum coverage that should be met
        result = subprocess.run(
            [sys.executable, str(script_path), '--min-coverage', '80'],
            capture_output=True,
            text=True
        )
        
        assert result.returncode == 0
        # The key check is that returncode is 0
        # (not checking for 'ERROR' string as it may appear in normal output)
    
    def test_ansi_targets_file_exists(self):
        """ANSI targets file should exist."""
        targets_file = Path(__file__).parent.parent / 'docs' / 'ansi_targets.txt'
        assert targets_file.exists()
        
        # File should have content
        content = targets_file.read_text()
        assert len(content) > 0
        # Should have some symbol names
        assert 'DEFUN' in content
        assert 'CAR' in content
        assert 'CDR' in content
