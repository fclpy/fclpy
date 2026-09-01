# FCLpy Reader Implementation Documentation

## Overview

The FCLpy reader module provides Common Lisp expression parsing functionality. This document describes the current implementation, deferred features, and known limitations.

## Implemented Features

### Core Reading
- ✅ Integer literals (positive, negative, zero)
- ✅ Float literals (decimal numbers)
- ✅ String literals with escape sequences (`\n`, `\t`, `\r`, `\\`, `\"`)
- ✅ Symbol parsing with case insensitivity (all symbols normalized to uppercase)
- ✅ Keyword literals (`:FOO` syntax)
- ✅ Character literals (`#\A`, `#\Space`, etc.)
- ✅ List structures `(a b c)`
- ✅ Dotted lists `(a . b)`
- ✅ Nested lists
- ✅ Empty list `()` (represents NIL)

### Dispatch Macros
- ✅ `'` - Quote (QUOTE form)
- ✅ `` ` `` - Backquote (QUASIQUOTE form)
- ✅ `,` - Unquote (UNQUOTE form)
- ✅ `#'` - Function quote (FUNCTION form)
- ✅ `#()` - Vector literals (represented as (VECTOR element1 element2 ...))
- ✅ `#|...|#` - Block comments with nesting support
- ✅ `;` - Line comments

### Package System
- ✅ Symbol interning into current package
- ✅ Keyword interning into KEYWORD package
- ✅ Package-aware symbol identity (same symbol name = same object)
- ✅ *PACKAGE* dynamic variable support

### Error Handling
- ✅ UnexpectedEOF exception for premature end of input
- ✅ UnbalancedParen exception for mismatched parentheses
- ✅ InvalidNumber exception base class (reserved for future use)
- ✅ ReaderError base exception class
- ✅ Conversion of tokenizer errors to ReaderError subclasses

### Printing
- ✅ `prin1()` - Print in readable form
- ✅ `princ()` - Print in canonical form
- ✅ Proper quote form printing
- ✅ String escaping
- ✅ Symbol case preservation (prints uppercase)
- ✅ Keyword printing with colon prefix
- ✅ Character literal printing with `#\` prefix

### Round-Trip Capability
- ✅ Read → Print → Read round-trips work for all basic types
- ✅ 100% success rate on comprehensive test corpus
- ✅ Symbol identity preserved across round-trips
- ✅ Keyword identity preserved across round-trips

## Deferred Features (Not Yet Implemented)

### Reader Features
- ⏳ `unread-char` - Return a character to the input stream
- ⏳ `read-base` - Variable to control radix (decimal, hex, octal, binary)
- ⏳ Radix notation (`#xAB`, `#o77`, `#b1010`, `#r36Z`)
- ⏳ `read-from-string` with position tracking
- ⏳ Read macros (user-definable reader extensions)
- ⏳ Complex number literals
- ⏳ Ratio support (parse `3/4` as rational number type, not symbols)

### Printer Features
- ⏳ `write` function (like `prin1` but with more options)
- ⏳ `write-to-string` function
- ⏳ Printer dispatch table (customizable printing)
- ⏳ Pretty-printing with indentation
- ⏳ Circular structure detection and printing

### Readtable Features
- ⏳ Custom readtable creation and modification
- ⏳ Dispatch macro tables
- ⏳ Reader case modes (UPCASE, DOWNCASE, PRESERVE, INVERT)
- ⏳ Macro character case sensitivity

### Advanced Features
- ⏳ Stream-based reading (currently string-only)
- ⏳ File I/O reading
- ⏳ Read-eval loop (`REPL`)
- ⏳ Interactive mode with error recovery

## Known Limitations

### Syntactic
1. **Ratio Literals**: `3/4` is currently parsed as `3` (integer) followed by `/4` (division symbol), not as a ratio object
   - Workaround: Use symbolic representation `(RATIO 3 4)`
   - Will be fixed when proper ratio type is implemented

2. **Vector Literals**: `#(1 2 3)` is represented internally as `(VECTOR 1 2 3)` rather than a proper vector type
   - This is sufficient for reading but not ideal for evaluation
   - Will be improved when Vector type is added to lisptype.py

3. **Quote Forms**: Quote forms are expanded to their functional equivalents
   - Example: `'foo` becomes `(QUOTE FOO)`
   - This is correct but may not preserve the original syntax in round-trips

### Error Handling
1. **Error Messages**: Currently relatively minimal
   - Future: Add line/column information to error messages
   - Future: Add context snippets in error messages

2. **Recovery**: Reader does not support error recovery
   - First error stops reading
   - Future: May add optional error recovery mode

### Performance
1. **Tokenization**: Two-pass approach (tokenize all, then parse)
   - Could be optimized to single-pass if needed
   - Current approach is adequate for most use cases

## Testing Status

- **Total Tests**: 405+ tests passing
- **Reader Tests**: 44 tests covering basic reading
- **Package/Symbol Tests**: Tests for symbol interning and identity
- **Error Tests**: 38 tests for error handling
- **Round-trip Tests**: 41 tests with 100% success rate on test corpus
- **Printer Tests**: 54 tests covering all output formats

## Architecture Notes

### Reader Module Structure
```
reader.py
├── Reader class - Main reader implementation
│   ├── read() - Read single object
│   ├── read_all() - Read multiple objects
│   └── _read_*() - Internal token-specific readers
├── Exception classes
│   ├── ReaderError (base)
│   ├── UnexpectedEOF
│   ├── UnbalancedParen
│   └── InvalidNumber
└── Public functions
    ├── read(text, package=None)
    └── read_all(text, package=None)
```

### Printer Module Structure
```
printer.py
├── prin1(obj) - Print readable form
├── princ(obj) - Print canonical form
├── print_object(obj) - Internal printer
└── Helpers
    ├── _print_string()
    ├── _print_symbol()
    ├── _print_keyword()
    ├── _print_character()
    ├── _print_list()
    └── _needs_quoting()
```

### Integration Points
- **Tokenizer** (`tokenizer.py`): Provides tokens for reader
- **Package System** (`lisptype.py`): Symbol interning
- **Dynamic Variables** (`state.py`): Stores current package context

## Future Improvements

### High Priority
1. Implement proper Ratio type
2. Implement proper Vector type
3. Add stream-based reading (StringIO support)
4. Better error messages with position information

### Medium Priority
1. Read-eval loop (REPL)
2. User-defined read macros
3. Radix notation support
4. Pretty printing with indentation

### Low Priority
1. Performance optimization
2. Error recovery mode
3. Customizable printer dispatch table
4. Advanced metaprogramming features

## Compatibility Notes

### Differences from ANSI CL
1. **Ratio Parsing**: Not yet implemented as ratio objects
2. **Vector Syntax**: Internally represented as symbolic forms
3. **Read Macros**: User-defined read macros not yet supported
4. **Streams**: Only string input supported (no file/stream I/O)
5. **Print Dispatch**: No customizable printer (always default printing)

## Testing Guide

Run tests for reader functionality:
```bash
# All reader tests
pipenv run pytest tests/test_reader_and_packages.py -v

# Error handling tests
pipenv run pytest tests/test_reader_errors.py -v

# Printer tests
pipenv run pytest tests/test_printer.py -v

# Round-trip tests
pipenv run pytest tests/test_roundtrip.py -v

# All tests
pipenv run pytest -q
```

## Maintenance

This document should be updated whenever:
- New features are added to the reader or printer
- Known limitations are resolved
- New deferred features are identified
- Bugs are discovered and documented

Last Updated: Phase 2 completion
Status: Ready for Phase 3 (Evaluation)
