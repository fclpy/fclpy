"""
Comprehensive tests for the tokenizer module.
Tests character streams, tokenization, and all token types.
"""

import pytest
from fclpy.tokenizer import (
    CharacterStream, Tokenizer, Token, TokenType
)


class TestCharacterStream:
    """Test the CharacterStream class."""
    
    def test_create_stream(self):
        """Test stream creation."""
        stream = CharacterStream("hello")
        assert stream is not None
        assert not stream.at_eof()
    
    def test_peek_character(self):
        """Test peeking at characters."""
        stream = CharacterStream("abc")
        assert stream.peek() == 'a'
        assert stream.peek() == 'a'  # peek doesn't advance
        assert stream.peek(1) == 'b'
        assert stream.peek(2) == 'c'
    
    def test_advance_character(self):
        """Test advancing through stream."""
        stream = CharacterStream("abc")
        assert stream.advance() == 'a'
        assert stream.advance() == 'b'
        assert stream.advance() == 'c'
        assert stream.advance() is None
    
    def test_eof_detection(self):
        """Test EOF detection."""
        stream = CharacterStream("a")
        assert not stream.at_eof()
        stream.advance()
        assert stream.at_eof()
    
    def test_peek_at_eof(self):
        """Test peeking at EOF."""
        stream = CharacterStream("a")
        stream.advance()
        assert stream.peek() is None
    
    def test_line_tracking(self):
        """Test line number tracking."""
        stream = CharacterStream("a\nb\nc")
        line, col = stream.current_position()
        assert line == 1 and col == 1
        
        stream.advance()  # 'a'
        line, col = stream.current_position()
        assert col == 2
        
        stream.advance()  # '\n'
        line, col = stream.current_position()
        assert line == 2 and col == 1
    
    def test_skip_whitespace(self):
        """Test whitespace skipping."""
        stream = CharacterStream("   \t\n  a")
        stream.skip_whitespace()
        assert stream.peek() == 'a'


class TestTokenType:
    """Test Token and TokenType."""
    
    def test_token_creation(self):
        """Test creating tokens."""
        token = Token(TokenType.INTEGER, "42", 1, 1)
        assert token.type == TokenType.INTEGER
        assert token.value == "42"
        assert token.line == 1
        assert token.column == 1
    
    def test_token_equality(self):
        """Test token equality."""
        t1 = Token(TokenType.INTEGER, "42", 1, 1)
        t2 = Token(TokenType.INTEGER, "42", 1, 1)
        t3 = Token(TokenType.INTEGER, "43", 1, 1)
        
        assert t1 == t2
        assert t1 != t3
    
    def test_token_repr(self):
        """Test token representation."""
        token = Token(TokenType.SYMBOL, "FOO", 1, 5)
        repr_str = repr(token)
        assert "SYMBOL" in repr_str
        assert "FOO" in repr_str


class TestTokenizerBasics:
    """Test basic tokenizer functionality."""
    
    def test_empty_input(self):
        """Test tokenizing empty input."""
        tokenizer = Tokenizer("")
        tokens = tokenizer.tokenize_filtered()
        assert len(tokens) == 0
    
    def test_whitespace_only(self):
        """Test tokenizing whitespace only."""
        tokenizer = Tokenizer("   \t\n  ")
        tokens = tokenizer.tokenize_filtered()
        assert len(tokens) == 0
    
    def test_simple_list(self):
        """Test tokenizing a simple list."""
        tokenizer = Tokenizer("(a b c)")
        tokens = tokenizer.tokenize_filtered()
        
        assert len(tokens) == 5
        assert tokens[0].type == TokenType.LPAREN
        assert tokens[1].type == TokenType.SYMBOL
        assert tokens[2].type == TokenType.SYMBOL
        assert tokens[3].type == TokenType.SYMBOL
        assert tokens[4].type == TokenType.RPAREN


class TestNumbers:
    """Test number tokenization."""
    
    def test_integer(self):
        """Test integer tokenization."""
        tokenizer = Tokenizer("42")
        tokens = tokenizer.tokenize_filtered()
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.INTEGER
        assert tokens[0].value == "42"
    
    def test_negative_integer(self):
        """Test negative integer."""
        tokenizer = Tokenizer("-123")
        tokens = tokenizer.tokenize_filtered()
        assert tokens[0].type == TokenType.INTEGER
        assert tokens[0].value == "-123"
    
    def test_float(self):
        """Test float tokenization."""
        tokenizer = Tokenizer("3.14")
        tokens = tokenizer.tokenize_filtered()
        assert tokens[0].type == TokenType.FLOAT
        assert tokens[0].value == "3.14"
    
    def test_float_with_exponent(self):
        """Test float with exponent."""
        tokenizer = Tokenizer("1e10")
        tokens = tokenizer.tokenize_filtered()
        assert tokens[0].type == TokenType.FLOAT
    
    def test_ratio(self):
        """Test ratio tokenization."""
        tokenizer = Tokenizer("1/2")
        tokens = tokenizer.tokenize_filtered()
        assert tokens[0].type == TokenType.RATIO
        assert tokens[0].value == "1/2"
    
    def test_plus_as_symbol(self):
        """Test that + alone is a symbol, not a number prefix."""
        tokenizer = Tokenizer("+")
        tokens = tokenizer.tokenize_filtered()
        assert tokens[0].type == TokenType.SYMBOL
        assert tokens[0].value == "+"


class TestSymbols:
    """Test symbol tokenization."""
    
    def test_simple_symbol(self):
        """Test simple symbol."""
        tokenizer = Tokenizer("FOO")
        tokens = tokenizer.tokenize_filtered()
        assert tokens[0].type == TokenType.SYMBOL
        assert tokens[0].value == "FOO"
    
    def test_symbol_with_dash(self):
        """Test symbol with dashes."""
        tokenizer = Tokenizer("MY-SYMBOL")
        tokens = tokenizer.tokenize_filtered()
        assert tokens[0].type == TokenType.SYMBOL
        assert tokens[0].value == "MY-SYMBOL"
    
    def test_special_symbols(self):
        """Test special operator symbols."""
        tokenizer = Tokenizer("+ - * / < > = ! ? @")
        tokens = tokenizer.tokenize_filtered()
        # Count actual symbols (! and ? might be part of other tokens or handled differently)
        symbols = [t for t in tokens if t.type == TokenType.SYMBOL]
        # We expect: +, -, *, /, <, >, =, !, ?, @
        assert len(symbols) == 10
        symbol_values = [s.value for s in symbols]
        assert '+' in symbol_values
        assert '-' in symbol_values
        assert '*' in symbol_values
    
    def test_escaped_symbol(self):
        """Test pipe-quoted escaped symbol."""
        tokenizer = Tokenizer("|My Symbol|")
        tokens = tokenizer.tokenize_filtered()
        assert tokens[0].type == TokenType.SYMBOL
        assert tokens[0].value == "My Symbol"
    
    def test_escaped_symbol_with_escape_char(self):
        """Test escaped symbol with backslash."""
        tokenizer = Tokenizer(r"|My\|Symbol|")
        tokens = tokenizer.tokenize_filtered()
        assert tokens[0].type == TokenType.SYMBOL
        assert "|" in tokens[0].value


class TestKeywords:
    """Test keyword tokenization."""
    
    def test_simple_keyword(self):
        """Test simple keyword."""
        tokenizer = Tokenizer(":foo")
        tokens = tokenizer.tokenize_filtered()
        assert tokens[0].type == TokenType.KEYWORD
        assert tokens[0].value == ":foo"
    
    def test_keyword_with_dash(self):
        """Test keyword with dashes."""
        tokenizer = Tokenizer(":my-keyword")
        tokens = tokenizer.tokenize_filtered()
        assert tokens[0].type == TokenType.KEYWORD
        assert tokens[0].value == ":my-keyword"
    
    def test_keyword_uppercase(self):
        """Test uppercase keyword."""
        tokenizer = Tokenizer(":MY-KEYWORD")
        tokens = tokenizer.tokenize_filtered()
        assert tokens[0].type == TokenType.KEYWORD
        assert tokens[0].value == ":MY-KEYWORD"


class TestStrings:
    """Test string tokenization."""
    
    def test_simple_string(self):
        """Test simple string."""
        tokenizer = Tokenizer('"hello"')
        tokens = tokenizer.tokenize_filtered()
        assert tokens[0].type == TokenType.STRING
        assert tokens[0].value == "hello"
    
    def test_string_with_spaces(self):
        """Test string with spaces."""
        tokenizer = Tokenizer('"hello world"')
        tokens = tokenizer.tokenize_filtered()
        assert tokens[0].value == "hello world"
    
    def test_string_with_escape_sequences(self):
        """Test string with escape sequences."""
        tokenizer = Tokenizer(r'"hello\nworld"')
        tokens = tokenizer.tokenize_filtered()
        assert tokens[0].value == "hello\nworld"
    
    def test_string_with_escaped_quote(self):
        """Test string with escaped quote."""
        tokenizer = Tokenizer(r'"Say \"hi\""')
        tokens = tokenizer.tokenize_filtered()
        assert 'Say "hi"' in tokens[0].value


class TestCharacters:
    """Test character literal tokenization."""
    
    def test_character_literal(self):
        """Test character literal."""
        tokenizer = Tokenizer("#\\A")
        tokens = tokenizer.tokenize_filtered()
        assert tokens[0].type == TokenType.CHARACTER
    
    def test_named_character(self):
        """Test named character."""
        tokenizer = Tokenizer("#\\Space")
        tokens = tokenizer.tokenize_filtered()
        assert tokens[0].type == TokenType.CHARACTER
        assert "Space" in tokens[0].value


class TestSpecialTokens:
    """Test special token types."""
    
    def test_delimiters(self):
        """Test parentheses and brackets."""
        tokenizer = Tokenizer("( ) [ ]")
        tokens = tokenizer.tokenize_filtered()
        assert tokens[0].type == TokenType.LPAREN
        assert tokens[1].type == TokenType.RPAREN
        assert tokens[2].type == TokenType.LBRACKET
        assert tokens[3].type == TokenType.RBRACKET
    
    def test_quote(self):
        """Test quote mark."""
        tokenizer = Tokenizer("'foo")
        tokens = tokenizer.tokenize_filtered()
        assert tokens[0].type == TokenType.QUOTE
        assert tokens[1].type == TokenType.SYMBOL
    
    def test_backquote(self):
        """Test backquote."""
        tokenizer = Tokenizer("`foo")
        tokens = tokenizer.tokenize_filtered()
        assert tokens[0].type == TokenType.BACKQUOTE
    
    def test_comma(self):
        """Test comma."""
        tokenizer = Tokenizer(",foo")
        tokens = tokenizer.tokenize_filtered()
        assert tokens[0].type == TokenType.COMMA
    
    def test_comma_at(self):
        """Test comma-at."""
        tokenizer = Tokenizer(",@foo")
        tokens = tokenizer.tokenize_filtered()
        assert tokens[0].type == TokenType.COMMA_AT
    
    def test_dot(self):
        """Test dot for dotted pairs."""
        tokenizer = Tokenizer("(a . b)")
        tokens = tokenizer.tokenize_filtered()
        assert tokens[2].type == TokenType.DOT
        assert tokens[2].value == "."
    
    def test_hash_quote(self):
        """Test #' function quote."""
        tokenizer = Tokenizer("#'foo")
        tokens = tokenizer.tokenize_filtered()
        assert tokens[0].type == TokenType.HASH_QUOTE
    
    def test_hash_paren(self):
        """Test #( vector literal."""
        tokenizer = Tokenizer("#(1 2 3)")
        tokens = tokenizer.tokenize_filtered()
        assert tokens[0].type == TokenType.HASH_LPAREN


class TestComments:
    """Test comment handling."""
    
    def test_line_comment(self):
        """Test line comment."""
        tokenizer = Tokenizer(";comment\nfoo")
        tokens = tokenizer.tokenize_filtered()
        assert len(tokens) == 1
        assert tokens[0].value == "foo"
    
    def test_multiple_line_comments(self):
        """Test multiple line comments."""
        tokenizer = Tokenizer(";first\n;second\nfoo")
        tokens = tokenizer.tokenize_filtered()
        assert len(tokens) == 1
        assert tokens[0].value == "foo"
    
    def test_block_comment(self):
        """Test block comment."""
        tokenizer = Tokenizer("#|comment|#foo")
        tokens = tokenizer.tokenize_filtered()
        assert len(tokens) == 1
        assert tokens[0].value == "foo"
    
    def test_nested_block_comments(self):
        """Test nested block comments."""
        tokenizer = Tokenizer("#|outer #|inner|# outer|#foo")
        tokens = tokenizer.tokenize_filtered()
        assert len(tokens) == 1
        assert tokens[0].value == "foo"


class TestComplexExpressions:
    """Test tokenizing complex expressions."""
    
    def test_list_with_multiple_types(self):
        """Test list with various token types."""
        tokenizer = Tokenizer('(defun foo (x) (+ x 1))')
        tokens = tokenizer.tokenize_filtered()
        
        # Check we have the right tokens
        assert tokens[0].type == TokenType.LPAREN
        assert tokens[1].value == "defun"
        assert tokens[2].value == "foo"
        # ... etc
    
    def test_quoted_list(self):
        """Test quoted list."""
        tokenizer = Tokenizer("'(a b c)")
        tokens = tokenizer.tokenize_filtered()
        assert tokens[0].type == TokenType.QUOTE
        assert tokens[1].type == TokenType.LPAREN
    
    def test_dotted_pair(self):
        """Test dotted pair notation."""
        tokenizer = Tokenizer("(a . b)")
        tokens = tokenizer.tokenize_filtered()
        
        assert tokens[0].type == TokenType.LPAREN
        assert tokens[1].type == TokenType.SYMBOL
        assert tokens[2].type == TokenType.DOT
        assert tokens[3].type == TokenType.SYMBOL
        assert tokens[4].type == TokenType.RPAREN
    
    def test_vector(self):
        """Test vector literal."""
        tokenizer = Tokenizer("#(1 2 3)")
        tokens = tokenizer.tokenize_filtered()
        
        assert tokens[0].type == TokenType.HASH_LPAREN
        assert tokens[1].type == TokenType.INTEGER
        assert tokens[2].type == TokenType.INTEGER
        assert tokens[3].type == TokenType.INTEGER
        assert tokens[4].type == TokenType.RPAREN


class TestEdgeCases:
    """Test edge cases and error conditions."""
    
    def test_unterminated_string(self):
        """Test that unterminated strings raise an error."""
        tokenizer = Tokenizer('"unterminated')
        with pytest.raises(ValueError):
            tokenizer.tokenize_filtered()
    
    def test_unterminated_escaped_symbol(self):
        """Test unterminated escaped symbol."""
        tokenizer = Tokenizer('|unterminated')
        with pytest.raises(ValueError):
            tokenizer.tokenize_filtered()
    
    def test_unterminated_block_comment(self):
        """Test unterminated block comment."""
        tokenizer = Tokenizer('#|unterminated')
        with pytest.raises(ValueError):
            tokenizer.tokenize_filtered()
    
    def test_whitespace_preservation_in_escaped_symbol(self):
        """Test that whitespace is preserved in escaped symbols."""
        tokenizer = Tokenizer('|My  Symbol|')
        tokens = tokenizer.tokenize_filtered()
        assert "  " in tokens[0].value
    
    def test_numbers_with_leading_zeros(self):
        """Test numbers with leading zeros."""
        tokenizer = Tokenizer("007")
        tokens = tokenizer.tokenize_filtered()
        assert tokens[0].type == TokenType.INTEGER
        assert tokens[0].value == "007"
