"""
Comprehensive tests for Common Lisp dispatch macros in tokenizer.
Tests #' (function shorthand), #() (vector literals), #| |# (block comments with nesting).
"""

import pytest
from fclpy.tokenizer import Tokenizer, Token, TokenType


class TestHashQuote:
    """Test #' function quote dispatch macro."""
    
    def test_hash_quote_token(self):
        """Test that #' produces a HASH_QUOTE token."""
        tokenizer = Tokenizer("#'foo")
        tokens = tokenizer.tokenize_filtered()
        
        assert len(tokens) == 2
        assert tokens[0].type == TokenType.HASH_QUOTE
        assert tokens[0].value == "#'"
        assert tokens[1].type == TokenType.SYMBOL
        assert tokens[1].value == "foo"
    
    def test_hash_quote_with_symbol(self):
        """Test #' followed by a complex symbol."""
        tokenizer = Tokenizer("#'my-function")
        tokens = tokenizer.tokenize_filtered()
        
        assert tokens[0].type == TokenType.HASH_QUOTE
        assert tokens[1].value == "my-function"
    
    def test_hash_quote_with_list(self):
        """Test #' followed by a list."""
        tokenizer = Tokenizer("#'(lambda (x) x)")
        tokens = tokenizer.tokenize_filtered()
        
        assert tokens[0].type == TokenType.HASH_QUOTE
        assert tokens[1].type == TokenType.LPAREN
    
    def test_multiple_hash_quotes(self):
        """Test multiple #' in expression."""
        tokenizer = Tokenizer("#'foo #'bar")
        tokens = tokenizer.tokenize_filtered()
        
        assert tokens[0].type == TokenType.HASH_QUOTE
        assert tokens[2].type == TokenType.HASH_QUOTE
    
    def test_hash_quote_representation(self):
        """Test that HASH_QUOTE token prints correctly."""
        token = Token(TokenType.HASH_QUOTE, "#'", 1, 1)
        assert repr(token) == 'Token(HASH_QUOTE, "#\'", line=1, col=1)'


class TestVectorLiterals:
    """Test #() vector literal dispatch macro."""
    
    def test_empty_vector(self):
        """Test empty vector literal."""
        tokenizer = Tokenizer("#()")
        tokens = tokenizer.tokenize_filtered()
        
        assert len(tokens) == 2
        assert tokens[0].type == TokenType.HASH_LPAREN
        assert tokens[0].value == "#("
        assert tokens[1].type == TokenType.RPAREN
    
    def test_vector_with_integers(self):
        """Test vector with integer elements."""
        tokenizer = Tokenizer("#(1 2 3)")
        tokens = tokenizer.tokenize_filtered()
        
        assert tokens[0].type == TokenType.HASH_LPAREN
        assert tokens[1].type == TokenType.INTEGER
        assert tokens[2].type == TokenType.INTEGER
        assert tokens[3].type == TokenType.INTEGER
        assert tokens[4].type == TokenType.RPAREN
    
    def test_vector_with_symbols(self):
        """Test vector with symbol elements."""
        tokenizer = Tokenizer("#(a b c)")
        tokens = tokenizer.tokenize_filtered()
        
        assert tokens[0].type == TokenType.HASH_LPAREN
        assert tokens[1].type == TokenType.SYMBOL
        assert tokens[1].value == "a"
        assert tokens[2].value == "b"
        assert tokens[3].value == "c"
    
    def test_vector_with_mixed_types(self):
        """Test vector with mixed element types."""
        tokenizer = Tokenizer("#(1 foo :bar)")
        tokens = tokenizer.tokenize_filtered()
        
        assert tokens[0].type == TokenType.HASH_LPAREN
        assert tokens[1].type == TokenType.INTEGER
        assert tokens[2].type == TokenType.SYMBOL
        assert tokens[3].type == TokenType.KEYWORD
    
    def test_vector_with_strings(self):
        """Test vector with string elements."""
        tokenizer = Tokenizer('#("hello" "world")')
        tokens = tokenizer.tokenize_filtered()
        
        assert tokens[0].type == TokenType.HASH_LPAREN
        assert tokens[1].type == TokenType.STRING
        assert tokens[1].value == "hello"
        assert tokens[2].type == TokenType.STRING
        assert tokens[2].value == "world"
    
    def test_vector_with_characters(self):
        """Test vector with character literals."""
        tokenizer = Tokenizer("#(#\\A #\\Space)")
        tokens = tokenizer.tokenize_filtered()
        
        assert tokens[0].type == TokenType.HASH_LPAREN
        assert tokens[1].type == TokenType.CHARACTER
        assert tokens[2].type == TokenType.CHARACTER
    
    def test_nested_vector_in_list(self):
        """Test vector inside a list."""
        tokenizer = Tokenizer("(list #(1 2 3))")
        tokens = tokenizer.tokenize_filtered()
        
        assert tokens[0].type == TokenType.LPAREN
        assert tokens[1].type == TokenType.SYMBOL
        assert tokens[2].type == TokenType.HASH_LPAREN
        assert tokens[3].type == TokenType.INTEGER
    
    def test_vector_length_preserved(self):
        """Test that vector preserves element count."""
        tokenizer = Tokenizer("#(a b c d e)")
        tokens = tokenizer.tokenize_filtered()
        
        # Count elements (excluding #( and ))
        elements = [t for t in tokens[1:-1] if t.type == TokenType.SYMBOL]
        assert len(elements) == 5


class TestBlockComments:
    """Test #|...|# block comment dispatch macro."""
    
    def test_simple_block_comment(self):
        """Test simple block comment."""
        tokenizer = Tokenizer("#|comment|#foo")
        tokens = tokenizer.tokenize_filtered()
        
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.SYMBOL
        assert tokens[0].value == "foo"
    
    def test_block_comment_with_content(self):
        """Test block comment with multiline content."""
        tokenizer = Tokenizer("""#|This is a comment
        with multiple lines
        |#foo""")
        tokens = tokenizer.tokenize_filtered()
        
        assert len(tokens) == 1
        assert tokens[0].value == "foo"
    
    def test_multiple_block_comments(self):
        """Test multiple block comments."""
        tokenizer = Tokenizer("#|first|# foo #|second|# bar")
        tokens = tokenizer.tokenize_filtered()
        
        assert len(tokens) == 2
        assert tokens[0].value == "foo"
        assert tokens[1].value == "bar"
    
    def test_nested_block_comments(self):
        """Test nested block comments."""
        tokenizer = Tokenizer("#|outer #|inner|# outer|#foo")
        tokens = tokenizer.tokenize_filtered()
        
        assert len(tokens) == 1
        assert tokens[0].value == "foo"
    
    def test_deeply_nested_block_comments(self):
        """Test deeply nested block comments."""
        tokenizer = Tokenizer("#|level1 #|level2 #|level3|# l2|# l1|#foo")
        tokens = tokenizer.tokenize_filtered()
        
        assert len(tokens) == 1
        assert tokens[0].value == "foo"
    
    def test_block_comment_with_special_chars(self):
        """Test block comment containing special characters."""
        tokenizer = Tokenizer("#|comment with (parens) and [brackets] and 'quotes'|#foo")
        tokens = tokenizer.tokenize_filtered()
        
        assert len(tokens) == 1
        assert tokens[0].value == "foo"
    
    def test_block_comment_around_code(self):
        """Test block comments surrounding code."""
        tokenizer = Tokenizer("""#|start comment|#
        (defun foo ()
        #|middle comment|#
        42)
        #|end comment|#""")
        tokens = tokenizer.tokenize_filtered()
        
        # Should have: ( defun foo ( ) 42 )
        assert tokens[0].type == TokenType.LPAREN
        assert tokens[1].value == "defun"
        assert tokens[2].value == "foo"
    
    def test_unterminated_block_comment(self):
        """Test that unterminated block comments raise an error."""
        tokenizer = Tokenizer("#|unclosed comment")
        with pytest.raises(ValueError):
            tokenizer.tokenize_filtered()
    
    def test_nested_with_extra_text(self):
        """Test nested comments where outer comment has more text after inner closes."""
        # After inner comment closes with |#, outer comment continues until next |#
        tokenizer = Tokenizer("#|outer #|inner|# extra|# foo")
        tokens = tokenizer.tokenize_filtered()
        
        # Should tokenize foo
        assert len(tokens) == 1
        assert tokens[0].value == "foo"


class TestLineComments:
    """Test ; line comment."""
    
    def test_simple_line_comment(self):
        """Test simple line comment."""
        tokenizer = Tokenizer(";comment\nfoo")
        tokens = tokenizer.tokenize_filtered()
        
        assert len(tokens) == 1
        assert tokens[0].value == "foo"
    
    def test_comment_at_end_of_line(self):
        """Test comment at end of line."""
        tokenizer = Tokenizer("foo ;comment")
        tokens = tokenizer.tokenize_filtered()
        
        assert len(tokens) == 1
        assert tokens[0].value == "foo"
    
    def test_multiple_line_comments(self):
        """Test multiple line comments."""
        tokenizer = Tokenizer(";first\nfoo\n;second\nbar")
        tokens = tokenizer.tokenize_filtered()
        
        assert len(tokens) == 2
        assert tokens[0].value == "foo"
        assert tokens[1].value == "bar"
    
    def test_comment_with_special_chars(self):
        """Test line comment with special characters."""
        tokenizer = Tokenizer("foo ;this (is) [a] comment!")
        tokens = tokenizer.tokenize_filtered()
        
        assert len(tokens) == 1
        assert tokens[0].value == "foo"


class TestMixedDispatchMacros:
    """Test combinations of dispatch macros."""
    
    def test_hash_quote_and_vector(self):
        """Test #' and #() in same expression."""
        tokenizer = Tokenizer("#'foo #(1 2)")
        tokens = tokenizer.tokenize_filtered()
        
        assert tokens[0].type == TokenType.HASH_QUOTE
        assert tokens[2].type == TokenType.HASH_LPAREN
    
    def test_vector_with_comments(self):
        """Test vector with block comments inside."""
        tokenizer = Tokenizer("#(1 #|comment|# 2 3)")
        tokens = tokenizer.tokenize_filtered()
        
        assert tokens[0].type == TokenType.HASH_LPAREN
        assert tokens[1].type == TokenType.INTEGER
        assert tokens[2].type == TokenType.INTEGER
        assert tokens[3].type == TokenType.INTEGER
    
    def test_block_comment_preserves_code(self):
        """Test that block comments don't affect surrounding code."""
        tokenizer = Tokenizer("(foo #|comment|# bar)")
        tokens = tokenizer.tokenize_filtered()
        
        assert len(tokens) == 4
        assert tokens[0].type == TokenType.LPAREN
        assert tokens[1].value == "foo"
        assert tokens[2].value == "bar"
        assert tokens[3].type == TokenType.RPAREN
    
    def test_complex_expression(self):
        """Test complex expression with multiple dispatch macros."""
        tokenizer = Tokenizer("""
        #|main comment|#
        (defun test ()
            #'my-func
            #(1 2 3) ;vector
            "string" ;comment
        )
        """)
        tokens = tokenizer.tokenize_filtered()
        
        # Should have all the key tokens
        types = [t.type for t in tokens]
        assert TokenType.LPAREN in types
        assert TokenType.SYMBOL in types  # defun
        assert TokenType.HASH_QUOTE in types
        assert TokenType.HASH_LPAREN in types
        assert TokenType.STRING in types


class TestDispatchMacroEdgeCases:
    """Test edge cases for dispatch macros."""
    
    def test_hash_quote_at_eof(self):
        """Test #' at end of file."""
        tokenizer = Tokenizer("foo #'bar")
        tokens = tokenizer.tokenize_filtered()
        
        assert tokens[-1].type == TokenType.SYMBOL
        assert tokens[-2].type == TokenType.HASH_QUOTE
    
    def test_empty_block_comment(self):
        """Test empty block comment."""
        tokenizer = Tokenizer("#||#foo")
        tokens = tokenizer.tokenize_filtered()
        
        assert len(tokens) == 1
        assert tokens[0].value == "foo"
    
    def test_block_comment_with_pipe(self):
        """Test block comment containing a single pipe (not the close marker)."""
        tokenizer = Tokenizer("#|has | pipe|#foo")
        tokens = tokenizer.tokenize_filtered()
        
        # The single | should not close the comment
        assert len(tokens) == 1
        assert tokens[0].value == "foo"
    
    def test_vector_empty_then_full(self):
        """Test both empty and full vectors."""
        tokenizer = Tokenizer("#() #(1 2 3)")
        tokens = tokenizer.tokenize_filtered()
        
        # Empty vector: #( )
        assert tokens[0].type == TokenType.HASH_LPAREN
        assert tokens[1].type == TokenType.RPAREN
        
        # Full vector: #( 1 2 3 )
        assert tokens[2].type == TokenType.HASH_LPAREN
        assert tokens[3].type == TokenType.INTEGER
    
    def test_hash_quote_with_keyword(self):
        """Test #' with keyword."""
        tokenizer = Tokenizer("#':foo")
        tokens = tokenizer.tokenize_filtered()
        
        assert tokens[0].type == TokenType.HASH_QUOTE
        assert tokens[1].type == TokenType.KEYWORD
    
    def test_block_comment_with_quoted_content(self):
        """Test block comment containing quoted content."""
        tokenizer = Tokenizer("#|'foo \"bar\" `baz |#qux")
        tokens = tokenizer.tokenize_filtered()
        
        assert len(tokens) == 1
        assert tokens[0].value == "qux"
