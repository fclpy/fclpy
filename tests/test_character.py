"""
Tests for Common Lisp CHARACTER type and character literals.
"""

import pytest
from fclpy.lisptype import Character
from fclpy.character import parse_character_token, tokenize_with_characters
from fclpy.tokenizer import Token, TokenType


class TestCharacterType:
    """Test the Character class."""
    
    def test_character_creation(self):
        """Test creating a character."""
        char = Character('A')
        assert char.char == 'A'
        assert char.code == ord('A')
    
    def test_character_invalid_input(self):
        """Test that invalid input raises errors."""
        with pytest.raises(TypeError):
            Character('AB')  # More than one character
        
        with pytest.raises(TypeError):
            Character('')  # Empty string
    
    def test_character_equality(self):
        """Test character equality."""
        c1 = Character('A')
        c2 = Character('A')
        c3 = Character('B')
        
        assert c1 == c2
        assert c1 != c3
        assert c1 != 'A'  # Characters are not equal to strings
    
    def test_character_hash(self):
        """Test that characters can be hashed."""
        c1 = Character('A')
        c2 = Character('A')
        
        # Both should hash the same
        assert hash(c1) == hash(c2)
        
        # Can use in sets/dicts
        char_set = {c1, c2}
        assert len(char_set) == 1
    
    def test_character_repr(self):
        """Test character representation."""
        c = Character('A')
        assert repr(c) == "#\\A"
    
    def test_character_str(self):
        """Test character string conversion."""
        c = Character('A')
        assert str(c) == "#\\A"


class TestNamedCharacters:
    """Test named character support."""
    
    def test_space_character(self):
        """Test space character."""
        c = Character(' ')
        assert repr(c) == "#\\Space"
        assert c.code == ord(' ')
    
    def test_newline_character(self):
        """Test newline character."""
        c = Character('\n')
        assert repr(c) == "#\\Newline"
        assert c.code == ord('\n')
    
    def test_tab_character(self):
        """Test tab character."""
        c = Character('\t')
        assert repr(c) == "#\\Tab"
        assert c.code == ord('\t')
    
    def test_return_character(self):
        """Test carriage return character."""
        c = Character('\r')
        assert repr(c) == "#\\Return"
        assert c.code == ord('\r')
    
    def test_from_name_space(self):
        """Test creating character from name."""
        c = Character.from_name("Space")
        assert c.char == ' '
        assert c.code == ord(' ')
    
    def test_from_name_newline(self):
        """Test creating newline from name."""
        c = Character.from_name("Newline")
        assert c.char == '\n'
    
    def test_from_name_case_insensitive(self):
        """Test that from_name is case-insensitive."""
        c1 = Character.from_name("SPACE")
        c2 = Character.from_name("Space")
        c3 = Character.from_name("space")
        
        assert c1 == c2
        assert c2 == c3
    
    def test_from_name_unknown(self):
        """Test that unknown names raise errors."""
        with pytest.raises(ValueError):
            Character.from_name("Unknown")
    
    def test_from_code(self):
        """Test creating character from code point."""
        c = Character.from_code(65)  # 'A'
        assert c.char == 'A'
        assert c.code == 65
    
    def test_from_code_unicode(self):
        """Test Unicode character from code."""
        c = Character.from_code(0x03B1)  # Greek alpha
        assert c.char == 'α'
        assert c.code == 0x03B1


class TestCharacterParsing:
    """Test parsing character tokens."""
    
    def test_parse_simple_character(self):
        """Test parsing simple character token."""
        token = Token(TokenType.CHARACTER, "#\\A", 1, 1)
        char = parse_character_token(token)
        assert char.char == 'A'
    
    def test_parse_space_character(self):
        """Test parsing space character token."""
        token = Token(TokenType.CHARACTER, "#\\Space", 1, 1)
        char = parse_character_token(token)
        assert char.char == ' '
    
    def test_parse_newline_character(self):
        """Test parsing newline character token."""
        token = Token(TokenType.CHARACTER, "#\\Newline", 1, 1)
        char = parse_character_token(token)
        assert char.char == '\n'
    
    def test_parse_named_character_case_insensitive(self):
        """Test parsing named characters with various cases."""
        token1 = Token(TokenType.CHARACTER, "#\\SPACE", 1, 1)
        token2 = Token(TokenType.CHARACTER, "#\\Space", 1, 1)
        token3 = Token(TokenType.CHARACTER, "#\\space", 1, 1)
        
        c1 = parse_character_token(token1)
        c2 = parse_character_token(token2)
        c3 = parse_character_token(token3)
        
        assert c1 == c2
        assert c2 == c3
    
    def test_parse_invalid_token_type(self):
        """Test that non-CHARACTER tokens raise errors."""
        token = Token(TokenType.SYMBOL, "foo", 1, 1)
        with pytest.raises(ValueError):
            parse_character_token(token)
    
    def test_parse_unknown_named_character(self):
        """Test that unknown named characters raise errors."""
        token = Token(TokenType.CHARACTER, "#\\Unknown", 1, 1)
        with pytest.raises(ValueError):
            parse_character_token(token)


class TestTokenizeWithCharacters:
    """Test tokenizing text that includes character literals."""
    
    def test_tokenize_simple_characters(self):
        """Test tokenizing simple character literals."""
        text = "#\\A #\\B"
        tokens = tokenize_with_characters(text)
        
        assert len(tokens) == 2
        assert isinstance(tokens[0], Character)
        assert isinstance(tokens[1], Character)
        assert tokens[0].char == 'A'
        assert tokens[1].char == 'B'
    
    def test_tokenize_named_characters(self):
        """Test tokenizing named character literals."""
        text = "#\\Space #\\Newline"
        tokens = tokenize_with_characters(text)
        
        assert len(tokens) == 2
        assert tokens[0].char == ' '
        assert tokens[1].char == '\n'
    
    def test_tokenize_mixed_tokens(self):
        """Test tokenizing mixed tokens including characters."""
        text = "(list #\\A #\\B)"
        tokens = tokenize_with_characters(text)
        
        # Should have: LPAREN, SYMBOL, CHARACTER, CHARACTER, RPAREN
        assert tokens[0].type == TokenType.LPAREN
        assert tokens[1].type == TokenType.SYMBOL
        assert isinstance(tokens[2], Character)
        assert isinstance(tokens[3], Character)
        assert tokens[4].type == TokenType.RPAREN
    
    def test_tokenize_character_in_list(self):
        """Test character literals in a list context."""
        text = "'(#\\A #\\Space #\\Newline)"
        tokens = tokenize_with_characters(text)
        
        # Find the characters
        chars = [t for t in tokens if isinstance(t, Character)]
        assert len(chars) == 3
        assert chars[0].char == 'A'
        assert chars[1].char == ' '
        assert chars[2].char == '\n'


class TestCharacterRoundTrip:
    """Test round-trip conversion of characters."""
    
    def test_roundtrip_simple_char(self):
        """Test round-trip for simple character."""
        c1 = Character('A')
        repr_str = repr(c1)
        assert repr_str == "#\\A"
        
        # Parse it back
        token = Token(TokenType.CHARACTER, repr_str, 1, 1)
        c2 = parse_character_token(token)
        assert c1 == c2
    
    def test_roundtrip_space(self):
        """Test round-trip for space character."""
        c1 = Character(' ')
        repr_str = repr(c1)
        
        # Parse it back
        token = Token(TokenType.CHARACTER, repr_str, 1, 1)
        c2 = parse_character_token(token)
        assert c1 == c2
    
    def test_roundtrip_all_named_characters(self):
        """Test round-trip for all named characters."""
        named_chars = ['Space', 'Newline', 'Tab', 'Return']
        
        for name in named_chars:
            c1 = Character.from_name(name)
            repr_str = repr(c1)
            
            token = Token(TokenType.CHARACTER, repr_str, 1, 1)
            c2 = parse_character_token(token)
            assert c1 == c2


class TestCharacterIntegration:
    """Test Character integration with other types."""
    
    def test_character_not_equal_to_symbol(self):
        """Test that characters are not equal to symbols."""
        from fclpy.lisptype import LispSymbol
        
        char = Character('A')
        sym = LispSymbol('A')
        
        assert char != sym
        assert sym != char
    
    def test_character_not_equal_to_number(self):
        """Test that characters are not equal to numbers."""
        char = Character('5')
        
        assert char != 5
        assert char != '5'  # Different from a string
    
    def test_character_with_special_chars(self):
        """Test characters with special symbols."""
        special_chars = ['!', '@', '#', '$', '%', '&', '*', '(', ')', '-', '+', '=']
        
        for ch in special_chars:
            c = Character(ch)
            assert c.char == ch
            assert c.code == ord(ch)


class TestCharacterEdgeCases:
    """Test edge cases for Character type."""
    
    def test_null_character(self):
        """Test null character."""
        c = Character('\x00')
        assert c.code == 0
        assert c.char == '\x00'
    
    def test_high_unicode_characters(self):
        """Test high Unicode characters."""
        # Greek letter alpha
        c = Character('α')
        assert c.code == ord('α')
        assert c.char == 'α'
    
    def test_emoji_character(self):
        """Test emoji characters."""
        emoji = '😀'
        c = Character(emoji)
        assert c.char == emoji
        assert c.code == ord(emoji)
    
    def test_character_printability(self):
        """Test character printing behavior."""
        # Printable character
        c1 = Character('A')
        assert str(c1) == "#\\A"
        
        # Space character (printable but special)
        c2 = Character(' ')
        assert str(c2) == "#\\Space"
        
        # Newline (non-printable but named)
        c3 = Character('\n')
        assert str(c3) == "#\\Newline"
