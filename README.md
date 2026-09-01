# FCLPY — A Common Lisp Interpreter in Python

![Python Version](https://img.shields.io/badge/python-3.10-blue)
![License](https://img.shields.io/badge/license-MIT-green)
![ANSI Compliance](https://img.shields.io/badge/ANSI%20conformance-99.995%25-brightgreen)
![Unit Tests](https://img.shields.io/badge/unit%20tests-2103%20passing-brightgreen)

FCLPY is a Common Lisp interpreter implemented in pure Python, built toward one
goal: **full ANSI Common Lisp compliance**. It is measured against the real
ANSI test suite ([`ansi-test`](https://gitlab.common-lisp.net/ansi-test/ansi-test)),
not a curated subset, and the project treats "the suite passes" as necessary
but not sufficient — see [CLAUDE.md](CLAUDE.md) for what that means in practice.

## Status

The interpreter includes a full reader and printer (readtables, `#`-dispatch,
pretty-printing, `FORMAT`), the evaluator and special forms, a macro system
where every CLHS-specified macro is a real macro (`MACRO-FUNCTION` and
`MACROEXPAND` work on all of them), `SETF`/`PSETF` and the generalized-place
protocol, `LOOP`, the condition/restart system (`HANDLER-BIND`,
`HANDLER-CASE`, `RESTART-CASE`, ...), CLOS (`DEFCLASS`, `DEFGENERIC`,
`DEFMETHOD`, method combination), hash tables, the sequence and array
protocols, packages, pathnames, and streams.

- **[`docs/ansi_checklist.md`](docs/ansi_checklist.md)** — generated, authoritative list of what's still failing and where.
- **[`plan.md`](plan.md)** — current status and the mechanism-by-mechanism plan.
- **[`docs/changelog.md`](docs/changelog.md)** — history of how compliance got here.
- **[`CLAUDE.md`](CLAUDE.md)** — architecture map and the development loop, for anyone (human or agent) working on the implementation.

## Installation

### Using Pipenv (recommended for development)
```bash
git clone https://github.com/fclpy/fclpy.git
cd fclpy
pipenv install --dev
```

### From Source
```bash
git clone https://github.com/fclpy/fclpy.git
cd fclpy
pip install -e .
```

Requires Python 3.10. The interpreter itself has no external dependencies.

## Quick Start

### Interactive REPL
```bash
pipenv run python run.py
```
```lisp
FCLPY> (+ 1 2 3)
6
FCLPY> (loop for i from 1 to 5 collect (* i i))
(1 4 9 16 25)
FCLPY> (format nil "~{~A~^, ~}" '(1 2 3))
1, 2, 3
FCLPY> (defclass point () ((x :initarg :x :accessor point-x)
                            (y :initarg :y :accessor point-y)))
#<STANDARD-CLASS POINT>
FCLPY> (point-x (make-instance 'point :x 3 :y 4))
3
```

### Run a Lisp File
```bash
pipenv run python run.py script.lisp
```

### Command-Line Options
```
run.py [options] [files...]

  -i, --interactive   Enter REPL after processing files
  -q, --quiet         Suppress startup messages
  -v, --verbose       Verbose output
  -t, --timing        Show timing information
  -E ENCODING         File encoding (default: utf-8)
  --version           Show version number
```
`-norc` and `-ansi` are accepted for `clisp` compatibility but are currently
no-ops.

If the package is installed (`pip install -e .`), the same interface is also
available as the `fclpy` console script.

### Python API
```python
from fclpy import runtime, lispenv

lispenv.setup_standard_environment()
env = lispenv.current_environment

runtime.load_and_evaluate_file("my_script.lisp", env)
runtime.repl()
```

## Testing

FCLPY has two separate test layers:

**Unit tests** (`tests/`) — fast regression coverage for individual
functions, forms, and internal mechanisms:
```bash
pipenv run pytest -q
```

**ANSI conformance** — the actual compliance measurement, run against the
[`ansi-test`](https://gitlab.common-lisp.net/ansi-test/ansi-test) suite
checked out as a sibling directory (`../ansi-test`). A full run takes on the
order of 2 hours, so day-to-day development uses a targeted runner instead:
```bash
# One directory or file, in seconds:
pipenv run python scripts/run_ansi.py packages
pipenv run python scripts/run_ansi.py numbers/deposit-field.lsp

# The full suite (~2 hours) — moves the official scoreboard:
pipenv run python run_all_tests.py
```
See [CLAUDE.md](CLAUDE.md)'s "development loop" for how these fit together.

## Project Structure

```
fclpy/
├── fclpy/                  # Core interpreter package
│   ├── lispfunc/           # Evaluator, special forms, registry, all builtins
│   ├── classes.py          # CLOS object model
│   ├── typespec.py         # Type specifiers / SUBTYPEP / TYPEP lattice
│   ├── readtable.py        # Reader macro characters, #-dispatch, syntax types
│   ├── lispreader.py       # Token → form reader
│   ├── tokenizer.py        # Character-level tokenizer
│   ├── lisptype.py         # Core data types (symbols, cons, conditions, ...)
│   ├── lispenv.py          # Standard environment bootstrap
│   └── runtime.py          # REPL and file evaluation
├── tests/                  # Unit test suite (pytest)
├── scripts/                # Development tooling (run_ansi.py, gate.py, ...)
├── docs/                   # Generated checklist, changelog, reference docs
├── run.py                  # CLI entry point
├── run_all_tests.py        # Full ANSI suite runner
├── plan.md                 # Compliance plan and status
└── CLAUDE.md               # Architecture map and contributor guide
```

## Contributing

Read [CLAUDE.md](CLAUDE.md) first — it documents the architecture, the
development loop (how a fix gets made and verified without a 2-hour full
run), and the project's standing rules (fix the mechanism, not the test; no
expected-failure exemptions; one shared implementation per operator).

1. Fork the repository
2. Create a feature branch
3. Make your change and verify it per CLAUDE.md's development loop
4. `pipenv run python scripts/gate.py` — the cheap regression gate
5. Submit a pull request

## License

MIT License — see [LICENSE.txt](LICENSE.txt) for details.
Copyright (c) 2019-2025 Ralph Ritoch.

## Compatibility

- **Python**: 3.10
- **Operating Systems**: Windows, macOS, Linux
- **Dependencies**: None (pure Python implementation)
