#!/usr/bin/env python3
"""
Setup script for FCLPY - A Common Lisp Interpreter in Python
"""

from setuptools import setup, find_packages
from pathlib import Path

# Read the README file
this_directory = Path(__file__).parent
long_description = (this_directory / "README.md").read_text(encoding='utf-8')

# Read version from __init__.py
def get_version():
    """Extract version from fclpy/__init__.py"""
    init_file = this_directory / "fclpy" / "__init__.py"
    with open(init_file, 'r', encoding='utf-8') as f:
        for line in f:
            if line.startswith('__version__'):
                return line.split('=')[1].strip().strip('"\'')
    return "1.0.0"  # fallback version

setup(
    name="fclpy",
    version=get_version(),
    author="FCLPY Development Team",
    author_email="fclpy@example.com",
    description="A Common Lisp interpreter implemented in Python",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/your-username/fclpy",
    project_urls={
        "Bug Tracker": "https://github.com/your-username/fclpy/issues",
        "Documentation": "https://github.com/your-username/fclpy/wiki",
        "Source Code": "https://github.com/your-username/fclpy",
    },
    packages=find_packages(),
    classifiers=[
        "Development Status :: 4 - Beta",
        "Intended Audience :: Developers",
        "Intended Audience :: Education",
        "License :: OSI Approved :: MIT License",
        "Operating System :: OS Independent",
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.7",
        "Programming Language :: Python :: 3.8",
        "Programming Language :: Python :: 3.9",
        "Programming Language :: Python :: 3.10",
        "Programming Language :: Python :: 3.11",
        "Programming Language :: Python :: 3.12",
        "Programming Language :: Lisp",
        "Topic :: Software Development :: Interpreters",
        "Topic :: Software Development :: Libraries :: Python Modules",
        "Topic :: Education",
    ],
    keywords="lisp, interpreter, common-lisp, programming-language, repl",
    python_requires=">=3.7",
    install_requires=[
        # No external dependencies - pure Python implementation
    ],
    extras_require={
        "dev": [
            "pytest>=6.0",
            "pytest-cov",
            "black",
            "flake8",
            "mypy",
        ],
        "docs": [
            "sphinx",
            "sphinx-rtd-theme",
        ],
    },
    entry_points={
        "console_scripts": [
            "fclpy=fclpy.cli:main",
        ],
    },
    include_package_data=True,
    package_data={
        "fclpy": [
            "*.lisp",
            "examples/*.lisp",
        ],
    },
    zip_safe=False,
    license="MIT",
    platforms=["any"],
)
