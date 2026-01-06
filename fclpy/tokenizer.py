"""
Comprehensive tokenizer for Common Lisp syntax.
This module provides tokenization functionality for breaking Lisp source code
into tokens that can be parsed and evaluated.
"""

import re
from typing import List, Optional, Tuple, Iterator
from enum import Enum


class TokenType(Enum):
    """Enumeration of token types."""
    # Basic tokens
    INTEGER = "INTEGER"
    FLOAT = "FLOAT"
    RATIO = "RATIO"
    SYMBOL = "SYMBOL"
    KEYWORD = "KEYWORD"
    STRING = "STRING"
    CHARACTER = "CHARACTER"
    
    # Radix numbers
    HEX_NUMBER = "HEX_NUMBER"      # #xFF
    BINARY_NUMBER = "BINARY_NUMBER"  # #b1010
    OCTAL_NUMBER = "OCTAL_NUMBER"    # #o17
    
    # Delimiters
    LPAREN = "LPAREN"
    RPAREN = "RPAREN"
    LBRACKET = "LBRACKET"
    RBRACKET = "RBRACKET"
    
    # Special markers
    DOT = "DOT"
    QUOTE = "QUOTE"
    BACKQUOTE = "BACKQUOTE"
    COMMA = "COMMA"
    COMMA_AT = "COMMA_AT"
    HASH_QUOTE = "HASH_QUOTE"
    HASH_LPAREN = "HASH_LPAREN"
    HASH_PLUS = "HASH_PLUS"      # #+feature
    HASH_MINUS = "HASH_MINUS"    # #-feature
    UNINTERNED_SYMBOL = "UNINTERNED_SYMBOL"  # #:name
    
    # Comments and whitespace
    COMMENT = "COMMENT"
    WHITESPACE = "WHITESPACE"
    
    # Special
    EOF = "EOF"


class Token:
    """Represents a single token in Lisp source code."""
    
    def __init__(self, token_type: TokenType, value: str, line: int = 1, column: int = 1):
        """Initialize a token.
        
        Args:
            token_type: The type of token
            value: The actual text of the token
            line: Line number where token appears
            column: Column number where token appears
        """
        self.type = token_type
        self.value = value
        self.line = line
        self.column = column
    
    def __repr__(self):
        return f"Token({self.type.name}, {self.value!r}, line={self.line}, col={self.column})"
    
    def __eq__(self, other):
        if not isinstance(other, Token):
            return False
        return (self.type == other.type and 
                self.value == other.value and 
                self.line == other.line and 
                self.column == other.column)


class CharacterStream:
    """Stream abstraction for reading characters with peek/unread support."""
    
    def __init__(self, text: str):
        """Initialize stream with text.
        
        Args:
            text: The text to tokenize
        """
        self.text = text
        self.position = 0
        self.line = 1
        self.column = 1
    
    def peek(self, offset: int = 0) -> Optional[str]:
        """Peek at a character without advancing.
        
        Args:
            offset: How many characters ahead to peek (0 = next char)
            
        Returns:
            The character, or None if at EOF
        """
        pos = self.position + offset
        if pos >= len(self.text):
            return None
        return self.text[pos]
    
    def advance(self) -> Optional[str]:
        """Read and advance past the next character.
        
        Returns:
            The character, or None if at EOF
        """
        if self.position >= len(self.text):
            return None
        
        char = self.text[self.position]
        self.position += 1
        
        if char == '\n':
            self.line += 1
            self.column = 1
        else:
            self.column += 1
        
        return char
    
    def skip_whitespace(self):
        """Skip all whitespace characters."""
        while self.peek() and self.peek() in ' \t\n\r':
            self.advance()
    
    def at_eof(self) -> bool:
        """Check if at end of file."""
        return self.position >= len(self.text)
    
    def current_position(self) -> Tuple[int, int]:
        """Get current line and column."""
        return self.line, self.column


class Tokenizer:
    """Tokenizes Common Lisp source code."""
    
    def __init__(self, text: str):
        """Initialize tokenizer.
        
        Args:
            text: The source code to tokenize
        """
        self.stream = CharacterStream(text)
        self.tokens: List[Token] = []
    
    def tokenize(self) -> List[Token]:
        """Tokenize the entire input.
        
        Returns:
            List of tokens
        """
        self.tokens = []
        
        while not self.stream.at_eof():
            # Save position for token
            start_line, start_col = self.stream.current_position()
            
            # Skip whitespace
            if self._peek() in ' \t\n\r':
                self.stream.skip_whitespace()
                continue
            
            # Comments
            if self._peek() == ';':
                self._read_comment()
                continue
            
            # Block comments
            if self._peek() == '#' and self._peek_ahead(1) == '|':
                self._read_block_comment()
                continue
            
            # Try to read a token
            token = self._read_token(start_line, start_col)
            if token:
                self.tokens.append(token)
        
        # Add EOF token
        line, col = self.stream.current_position()
        self.tokens.append(Token(TokenType.EOF, "", line, col))
        
        return self.tokens
    
    def tokenize_filtered(self) -> List[Token]:
        """Tokenize and filter out comments, whitespace, and EOF tokens.
        
        Returns:
            List of non-whitespace, non-comment, non-EOF tokens
        """
        all_tokens = self.tokenize()
        return [t for t in all_tokens 
                if t.type not in (TokenType.COMMENT, TokenType.WHITESPACE, TokenType.EOF)]
    
    def _peek(self, offset: int = 0) -> Optional[str]:
        """Peek at next character."""
        return self.stream.peek(offset)
    
    def _peek_ahead(self, offset: int) -> Optional[str]:
        """Peek ahead by offset."""
        return self.stream.peek(offset)
    
    def _advance(self) -> Optional[str]:
        """Advance to next character."""
        return self.stream.advance()
    
    def _read_token(self, start_line: int, start_col: int) -> Optional[Token]:
        """Read a single token.
        
        Args:
            start_line: Line number where token starts
            start_col: Column number where token starts
            
        Returns:
            The token, or None
        """
        char = self._peek()
        
        if not char:
            return None
        
        # Single character delimiters
        if char == '(':
            self._advance()
            return Token(TokenType.LPAREN, '(', start_line, start_col)
        elif char == ')':
            self._advance()
            return Token(TokenType.RPAREN, ')', start_line, start_col)
        elif char == '[':
            self._advance()
            return Token(TokenType.LBRACKET, '[', start_line, start_col)
        elif char == ']':
            self._advance()
            return Token(TokenType.RBRACKET, ']', start_line, start_col)
        elif char == "'":
            self._advance()
            return Token(TokenType.QUOTE, "'", start_line, start_col)
        elif char == '`':
            self._advance()
            return Token(TokenType.BACKQUOTE, "`", start_line, start_col)
        elif char == ',':
            self._advance()
            if self._peek() == '@':
                self._advance()
                return Token(TokenType.COMMA_AT, ",@", start_line, start_col)
            return Token(TokenType.COMMA, ",", start_line, start_col)
        elif char == '.':
            # Check if it's a DOT (for dotted pairs) or part of a number
            if self._peek(1) and self._peek(1).isdigit():
                # It's a float like .5
                return self._read_number(start_line, start_col)
            else:
                # It's a dot (standalone)
                self._advance()
                return Token(TokenType.DOT, '.', start_line, start_col)
        
        # Hash dispatch characters
        elif char == '#':
            return self._read_hash_token(start_line, start_col)
        
        # Strings
        elif char == '"':
            return self._read_string(start_line, start_col)
        
        # Numbers and symbols
        elif char.isdigit() or (char in '+-' and self._peek(1) and self._peek(1).isdigit()):
            return self._read_number(start_line, start_col)
        elif char == '+' or char == '-':
            # Could be part of a number or a symbol
            return self._read_symbol_or_number(start_line, start_col)
        
        # Keywords and symbols
        elif char == ':':
            return self._read_keyword(start_line, start_col)
        elif char.isalpha() or char in '_':
            return self._read_symbol(start_line, start_col)
        elif char == '|':
            return self._read_escaped_symbol(start_line, start_col)
        else:
            # Unknown character, try to treat as symbol
            return self._read_symbol(start_line, start_col)
    
    def _read_number(self, start_line: int, start_col: int) -> Optional[Token]:
        """Read a number token (integer, float, or ratio)."""
        num_str = ""
        is_float = False
        
        # Handle leading sign
        if self._peek() in '+-':
            num_str += self._advance()
        
        # Read digits
        while self._peek() and self._peek().isdigit():
            num_str += self._advance()
        
        # Check for ratio (/)
        if self._peek() == '/' and self._peek(1) and self._peek(1).isdigit():
            num_str += self._advance()  # consume '/'
            while self._peek() and self._peek().isdigit():
                num_str += self._advance()
            return Token(TokenType.RATIO, num_str, start_line, start_col)
        
        # Check for decimal point (float)
        if self._peek() == '.' and self._peek(1) and self._peek(1).isdigit():
            num_str += self._advance()  # consume '.'
            while self._peek() and self._peek().isdigit():
                num_str += self._advance()
            is_float = True
        
        # Check for exponent notation (e.g., 1E10, 1.5D2, 3.14f0)
        # Common Lisp exponent markers: E (default), D (double), F (single), S (short), L (long)
        if self._peek() and self._peek() in 'eEfFdDsSlL':
            num_str += self._advance()
            if self._peek() in '+-':
                num_str += self._advance()
            while self._peek() and self._peek().isdigit():
                num_str += self._advance()
            is_float = True
        
        if is_float:
            return Token(TokenType.FLOAT, num_str, start_line, start_col)
        
        return Token(TokenType.INTEGER, num_str, start_line, start_col)
    
    def _read_symbol_or_number(self, start_line: int, start_col: int) -> Optional[Token]:
        """Read a symbol that might start with + or -."""
        # This is a + or - that isn't followed by a digit, so it's a symbol
        return self._read_symbol(start_line, start_col)
    
    def _read_symbol(self, start_line: int, start_col: int) -> Optional[Token]:
        """Read a symbol token."""
        sym = ""
        
        # Handle pipe-quoted symbols
        if self._peek() == '|':
            return self._read_escaped_symbol(start_line, start_col)
        
        # Regular symbol: letters, digits, and special chars
        # In Common Lisp, backslash escapes the next character, making it part of the symbol
        # Dots are also allowed in symbol names (e.g., format.:*.2)
        # Use a placeholder (\x00) for escaped colons so package parsing doesn't split on them
        while self._peek():
            char = self._peek()
            
            # Handle backslash escape
            if char == '\\':
                self._advance()  # consume backslash
                next_char = self._peek()
                if next_char:
                    escaped_char = self._advance()
                    # Use placeholder for escaped colons to prevent package parsing
                    if escaped_char == ':':
                        sym += '\x00'  # Placeholder for escaped colon
                    else:
                        sym += escaped_char  # add the escaped character (without backslash)
                else:
                    raise ValueError("Backslash at end of input")
            # Check for symbol constituent characters
            elif char.isalnum() or char in '_-+*/<>=!?@#$%^&.:':
                sym += self._advance()
            # Check for terminating characters (whitespace, parens, etc.)
            elif char in ' \t\n\r()[]{};\'"`,':
                break
            else:
                # Unknown character, stop reading
                break
        
        if not sym:
            return None
        
        return Token(TokenType.SYMBOL, sym, start_line, start_col)
    
    def _read_escaped_symbol(self, start_line: int, start_col: int) -> Optional[Token]:
        """Read a symbol enclosed in pipes |...|."""
        self._advance()  # consume opening |
        sym = ""
        
        while self._peek() and self._peek() != '|':
            if self._peek() == '\\':
                self._advance()
                if self._peek():
                    sym += self._advance()
            else:
                sym += self._advance()
        
        if self._peek() == '|':
            self._advance()  # consume closing |
        else:
            raise ValueError("Unterminated escaped symbol")
        
        return Token(TokenType.SYMBOL, sym, start_line, start_col)
    
    def _read_keyword(self, start_line: int, start_col: int) -> Optional[Token]:
        """Read a keyword token starting with ':'."""
        kw = self._advance()  # consume ':'
        
        # Read the rest of the keyword
        while self._peek() and (self._peek().isalnum() or self._peek() in '_-+*/<>=!?@#$%^&'):
            kw += self._advance()
        
        return Token(TokenType.KEYWORD, kw, start_line, start_col)
    
    def _read_string(self, start_line: int, start_col: int) -> Optional[Token]:
        """Read a string token."""
        self._advance()  # consume opening "
        string = ""
        
        while self._peek() and self._peek() != '"':
            if self._peek() == '\\':
                self._advance()
                if self._peek():
                    # Handle escape sequences
                    esc_char = self._advance()
                    if esc_char == 'n':
                        string += '\n'
                    elif esc_char == 't':
                        string += '\t'
                    elif esc_char == 'r':
                        string += '\r'
                    elif esc_char == '\\':
                        string += '\\'
                    elif esc_char == '"':
                        string += '"'
                    else:
                        string += esc_char
            else:
                string += self._advance()
        
        if self._peek() == '"':
            self._advance()  # consume closing "
        else:
            raise ValueError(f"Unterminated string at line {start_line}, col {start_col}")
        
        return Token(TokenType.STRING, string, start_line, start_col)
    
    def _read_hash_token(self, start_line: int, start_col: int) -> Optional[Token]:
        """Read tokens starting with #."""
        self._advance()  # consume '#'
        
        next_char = self._peek()
        
        # Character literals: #\A, #\Space, etc.
        if next_char == '\\':
            return self._read_character(start_line, start_col)
        
        # Function quote: #'
        elif next_char == "'":
            self._advance()
            return Token(TokenType.HASH_QUOTE, "#'", start_line, start_col)
        
        # Vector literals: #(...)
        elif next_char == '(':
            self._advance()
            return Token(TokenType.HASH_LPAREN, "#(", start_line, start_col)
        
        # Block comments: #|...|#
        elif next_char == '|':
            # This should have been handled in tokenize(), but handle it here too
            self._read_block_comment()
            return None
        
        # Hexadecimal numbers: #x...
        elif next_char and next_char.upper() == 'X':
            return self._read_radix_number(start_line, start_col, 16)
        
        # Binary numbers: #b...
        elif next_char and next_char.upper() == 'B':
            return self._read_radix_number(start_line, start_col, 2)
        
        # Octal numbers: #o...
        elif next_char and next_char.upper() == 'O':
            return self._read_radix_number(start_line, start_col, 8)
        
        # Feature conditionals: #+feature, #-feature
        elif next_char == '+':
            self._advance()
            return Token(TokenType.HASH_PLUS, "#+", start_line, start_col)
        
        elif next_char == '-':
            self._advance()
            return Token(TokenType.HASH_MINUS, "#-", start_line, start_col)
        
        # Uninterned symbol: #:name
        elif next_char == ':':
            self._advance()  # consume ':'
            # Read the symbol name (without the #: prefix)
            name = ""
            while self._peek() and self._peek() not in ' \t\n\r()[]':
                name += self._advance()
            if not name:
                raise ValueError("Empty symbol name after #:")
            # Return as UNINTERNED_SYMBOL token (name only, no #: prefix)
            return Token(TokenType.UNINTERNED_SYMBOL, name.upper(), start_line, start_col)
        
        # Other dispatch macros would go here
        else:
            # For now, just treat # followed by something as a symbol
            hash_str = "#"
            while self._peek() and self._peek() not in ' \t\n\r()[]':
                hash_str += self._advance()
            return Token(TokenType.SYMBOL, hash_str, start_line, start_col)
    
    def _read_radix_number(self, start_line: int, start_col: int, radix: int) -> Token:
        """Read a number in the specified radix (base).
        
        Args:
            start_line: Line number where token starts
            start_col: Column number where token starts
            radix: The base (2 for binary, 8 for octal, 16 for hex)
            
        Returns:
            Token with the parsed integer value
        """
        # Consume the radix indicator (x, b, or o)
        self._advance()
        
        # Define valid digit characters for each radix
        if radix == 2:
            valid_chars = '01'
            token_type = TokenType.BINARY_NUMBER
        elif radix == 8:
            valid_chars = '01234567'
            token_type = TokenType.OCTAL_NUMBER
        elif radix == 16:
            valid_chars = '0123456789abcdefABCDEF'
            token_type = TokenType.HEX_NUMBER
        else:
            valid_chars = '0123456789'
            token_type = TokenType.INTEGER
        
        # Check for sign
        num_str = ""
        if self._peek() in '+-':
            num_str += self._advance()
        
        # Read digits
        while self._peek() and self._peek() in valid_chars:
            num_str += self._advance()
        
        if not num_str or num_str in '+-':
            raise ValueError(f"No digits found for radix-{radix} number at line {start_line}")
        
        # Parse and store the integer value directly
        try:
            # Handle potential sign prefix
            if num_str.startswith('-'):
                value = -int(num_str[1:], radix)
            elif num_str.startswith('+'):
                value = int(num_str[1:], radix)
            else:
                value = int(num_str, radix)
            return Token(token_type, str(value), start_line, start_col)
        except ValueError:
            raise ValueError(f"Invalid radix-{radix} number: {num_str}")
    
    def _read_character(self, start_line: int, start_col: int) -> Optional[Token]:
        """Read a character literal #\\X."""
        char_str = "#"
        # Consume the backslash and add it to the token value
        if self._peek() == '\\':
            char_str += self._advance()  # Add the backslash
        else:
            # If no backslash, it's malformed
            raise ValueError(f"Expected backslash after # at line {start_line}, col {start_col}")
        
        char_str += self._advance()  # First character after backslash
        
        # Check for named characters (like Space, Newline, etc.)
        if self._peek() and self._peek().isalpha():
            while self._peek() and self._peek().isalpha():
                char_str += self._advance()
        
        return Token(TokenType.CHARACTER, char_str, start_line, start_col)
    
    def _read_comment(self):
        """Skip a line comment starting with ;."""
        while self._peek() and self._peek() != '\n':
            self._advance()
        if self._peek() == '\n':
            self._advance()
    
    def _read_block_comment(self):
        """Skip a block comment #|...|#."""
        self._advance()  # consume #
        self._advance()  # consume |
        
        depth = 1
        while depth > 0 and not self.stream.at_eof():
            if self._peek() == '#' and self._peek(1) == '|':
                depth += 1
                self._advance()
                self._advance()
            elif self._peek() == '|' and self._peek(1) == '#':
                depth -= 1
                self._advance()
                self._advance()
            else:
                self._advance()
        
        if depth > 0:
            raise ValueError("Unterminated block comment")
