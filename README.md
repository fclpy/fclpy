# FCLPY - A Common Lisp Interpreter in Python

![Python Version](https://img.shields.io/badge/python-3.7%2B-blue)
![License](https://img.shields.io/badge/license-MIT-green)
![Tests](https://img.shields.io/badge/tests-54%2F54%20passing-brightgreen)

FCLPY is a Common Lisp interpreter implemented in Python. It provides a complete Lisp environment with an interactive REPL, file evaluation, and a clean API for embedding Lisp functionality into Python applications.

## Features (Current Core vs Planned)

### ✅ Implemented Core (Early Phase 0)
- Arithmetic: `+ - * / = < > <= >=`
- Lists: `CAR CDR CONS LIST APPEND REVERSE` (plus many CADR variants via tests)
- Logic: `NOT AND OR`
- Predicates: `ATOM NULL EQ EQUAL SYMBOLP NUMBERP STRINGP` (some still returning Python bool; normalization in progress)
- Special Forms: `QUOTE IF SETQ LET DEFUN LAMBDA` (macro system not yet active)
- REPL & basic file loading

### 🛠 In Progress (Phase 0 Hygiene)
- Canonical boolean returns (`T` / `NIL` only) – auditing predicates
- Reader/Readtable consolidation planning
- Package and symbol registry groundwork

### 🔜 Upcoming (Phases 1–3 Highlights)
- Symbol slots (`value`, `function`, `plist`) & registry-driven environment
- Package-aware reader + printer round‑trips
- Macroexpansion, special operator dispatcher, backquote

### 📅 Later (Phases 4–8 Highlights)
- Multiple values & condition/restart system
- Unified sequence protocol, arrays, streams, pathnames
- Type system & early CLOS (DEFCLASS, generic dispatch)
- Declarations, coverage metrics & optimization metadata
- Extended numeric tower, FORMAT subset, pretty printer controls

Full phased plan: see `plan.md` and `plans/phase*.md` files.

### 💻 **Interactive REPL**
- Command-line interface similar to CLISP
- Interactive Read-Eval-Print Loop
- Built-in help system and debugging commands
- Support for multi-line expressions

### 📁 **File Evaluation**
- Load and evaluate Lisp files
- Silent loading (standard Lisp behavior)
- Verbose mode for debugging
- Proper error handling and reporting

### 🔧 **Python Integration**
- Clean API for embedding in Python projects
- Programmatic REPL control
- Function evaluation from Python
- Environment management

## Installation

### From Source
```bash
git clone https://github.com/fclpy/fclpy.git
cd fclpy
pip install -e .
```

### Using Pipenv (Development)
```bash
git clone https://github.com/fclpy/fclpy.git
cd fclpy
pipenv install
pipenv shell
```

## Quick Start

### Command Line Usage

#### Start Interactive REPL
```bash
fclpy
```

#### Run a Lisp File
```bash
fclpy script.lisp
```

#### Run with Verbose Output
```bash
fclpy -v script.lisp
```

#### Run Tests
```bash
fclpy --test
```

### Example Lisp Session
```lisp
FCLPY> (+ 1 2 3)
6
FCLPY> (defun square (x) (* x x))
SQUARE
FCLPY> (square 5)
25
FCLPY> (car '(a b c))
A
FCLPY> (cons 'first '(second third))
(FIRST SECOND THIRD)
```

## Python API

### Basic Usage
```python
from fclpy import runtime, lispenv

# Initialize Lisp environment
lispenv.setup_standard_environment()

# Start interactive REPL
runtime.repl()
```

### Load and Evaluate Files
```python
from fclpy import runtime, lispenv

# Set up environment
lispenv.setup_standard_environment()
env = lispenv.current_environment

# Load a Lisp file
success = runtime.load_and_evaluate_file("my_script.lisp", env)
```

### Custom REPL
```python
from fclpy.runtime import FclpyREPL

# Create custom REPL
repl = FclpyREPL(quiet=True, verbose=False)
repl.run()
```

## Language Support

### Data Types
- **Numbers**: Integers and floating-point
- **Strings**: Double-quoted strings with escape sequences
- **Symbols**: Case-insensitive identifiers
- **Lists**: Linked lists using cons cells
- **Booleans**: `T` (true) and `NIL` (false/empty list)

### Special Forms
```lisp
;; Variable assignment
(setq x 42)

;; Conditional expressions
(if (> x 0) 'positive 'negative)

;; Function definition
(defun factorial (n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

;; Local bindings
(let ((x 10) (y 20))
  (+ x y))

;; Lambda expressions
((lambda (x) (* x x)) 5)
```

### Built-in Functions
```lisp
;; Arithmetic
(+ 1 2 3 4)         ; => 10
(* 2 3 4)           ; => 24
(- 10 3)            ; => 7
(/ 15 3)            ; => 5

;; Comparisons
(= 5 5)             ; => T
(< 3 7)             ; => T
(<= 5 5)            ; => T

;; List operations
(cons 1 '(2 3))     ; => (1 2 3)
(car '(a b c))      ; => A
(cdr '(a b c))      ; => (B C)
(list 1 2 3)        ; => (1 2 3)

;; Predicates
(atom 'x)           ; => T
(null '())          ; => T
(symbolp 'hello)    ; => T
(numberp 42)        ; => T
```

## Command Line Options

FCLPY supports many CLISP-compatible command-line options:

```bash
fclpy [options] [files...]

Options:
  -h, --help              Show help message
  -i, --interactive       Enter REPL after processing files
  -q, --quiet            Suppress startup messages
  -v, --verbose          Verbose output
  --test                 Run comprehensive tests
  --version              Show version number
  -E ENCODING            File encoding (default: utf-8)
  -norc                  Do not load init file
  -ansi                  ANSI Common Lisp mode
```

## Examples

### Example 1: Simple Calculator
```lisp
;; calculator.lisp
(defun add (a b) (+ a b))
(defun multiply (a b) (* a b))
(defun square (x) (* x x))

(square (add 3 4))  ; => 49
```

### Example 2: List Processing
```lisp
;; lists.lisp
(defun length (lst)
  (if (null lst)
      0
      (+ 1 (length (cdr lst)))))

(defun reverse-list (lst)
  (if (null lst)
      '()
      (append (reverse-list (cdr lst)) (list (car lst)))))

(length '(a b c d))        ; => 4
(reverse-list '(1 2 3))    ; => (3 2 1)
```

### Example 3: Recursive Functions
```lisp
;; recursion.lisp
(defun factorial (n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(defun fibonacci (n)
  (if (<= n 2)
      1
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

(factorial 5)    ; => 120
(fibonacci 7)    ; => 13
```

## Testing

FCLPY includes a comprehensive test suite with 54 tests covering all major functionality:

```bash
# Run all tests
fclpy --test

# Run tests with verbose output
fclpy -v --test
```

Test categories:
- Function name mapping
- Arithmetic operations
- Basic evaluation
- Special forms
- Environment management
- Metacircular readiness

## Development

### Project Structure
```
fclpy/
├── fclpy/                 # Core interpreter package
│   ├── __init__.py       # Package initialization
│   ├── runtime.py        # Main runtime and REPL
│   ├── lispenv.py        # Environment management
│   ├── lispfunc.py       # Function implementations
│   ├── lispreader.py     # S-expression reader
│   └── lisptype.py       # Data type definitions
├── run.py                # CLI entry point
├── test_comprehensive.py # Test suite
├── setup.py              # Package setup
└── README.md             # This file
```

### Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Run the test suite: `fclpy --test`
5. Submit a pull request

### Running Tests
```bash
# Run comprehensive test suite
python run.py --test

# Run with verbose output
python run.py -v --test
```

## License

MIT License - see LICENSE file for details.

## Compatibility

- **Python**: 3.7+
- **Operating Systems**: Windows, macOS, Linux
- **Dependencies**: None (pure Python implementation)

## Roadmap & Phase Dashboard

Progress is tracked in `plan.md` (authoritative). Snapshot (manual – update as phases advance):

| Phase | Theme | Status | Notes |
|-------|-------|--------|-------|
| 0 | Quick Wins & Hygiene | In Progress | `T` symbol added; predicate audit ongoing |
| 1 | Canonical Core & Registry | Pending | |
| 2 | Reader + Printer + Packages | Pending | |
| 3 | Evaluator + Macros | Pending | |
| 4 | Multiple Values + Conditions | Pending | |
| 5 | Sequences/Arrays/Streams/Pathnames | Pending | |
| 6 | Type System & Early CLOS | Pending | |
| 7 | Declarations & Optimization | Pending | |
| 8 | Extended Numeric & Advanced I/O | Pending | Optional milestone extension |

Milestone "Meaningful ANSI Subset" = Phases 0–5 complete + coverage thresholds (see plan).

### High‑Level Upcoming Work
- Normalize predicate returns (finish Phase 0)
- Introduce registry metadata & symbol slot expansion (Phase 1)
- Implement readtable + tokenizer + basic printer (Phase 2)
- Add macro system & special operators (Phase 3)

### Deferred / Not Yet Implemented
Macro system, condition/restart hierarchy, multiple values semantics, sequences protocol refactor, hash tables (spec subset), CLOS, FORMAT, LOOP, pretty printing controls, advanced pathnames, declarations effect on optimization.

See `docs/spec_deviations.md` (to be created) for intentional omissions once documented.

## Examples and Learning

FCLPY is perfect for:
- Learning Lisp programming
- Teaching interpreter implementation
- Embedding Lisp in Python applications
- Rapid prototyping of symbolic computation
- Educational projects

## Support

- **Issues**: [GitHub Issues](https://github.com/fclpy/fclpy/issues)
- **Documentation**: [Wiki](https://github.com/fclpy/fclpy/wiki)
- **Source**: [GitHub Repository](https://github.com/fclpy/fclpy)

---

**FCLPY** - Bringing the power of Lisp to Python! 🚀
