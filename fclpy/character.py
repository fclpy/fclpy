"""
Character literal parsing and support for Common Lisp.
"""

from fclpy.tokenizer import Token, TokenType, Tokenizer
from fclpy.lisptype import Character


def parse_character_token(token: Token) -> Character:
    """Parse a CHARACTER token into a Character object.
    
    Args:
        token: A Token of type CHARACTER
        
    Returns:
        A Character object
        
    Raises:
        ValueError: If the token cannot be parsed as a character
    """
    if token.type != TokenType.CHARACTER:
        raise ValueError(f"Expected CHARACTER token, got {token.type}")
    
    value = token.value
    
    # Handle #\X format
    if value.startswith('#\\'):
        rest = value[2:]
        
        # Single character: #\A
        if len(rest) == 1:
            return Character(rest)
        
        # Named character: #\Space, #\Newline, etc.
        else:
            try:
                return Character.from_name(rest)
            except ValueError:
                raise ValueError(f"Unknown character literal: {token.value}")
    
    raise ValueError(f"Invalid character literal: {token.value}")


def tokenize_with_characters(text: str) -> list:
    """Tokenize text and parse CHARACTER tokens into Character objects.
    
    Args:
        text: Lisp source code
        
    Returns:
        List of tokens with CHARACTER tokens converted to Character objects
    """
    tokenizer = Tokenizer(text)
    tokens = tokenizer.tokenize_filtered()
    
    result = []
    for token in tokens:
        if token.type == TokenType.CHARACTER:
            try:
                char_obj = parse_character_token(token)
                # Keep the token but with a reference to the Character object
                # For now, we'll store it as a special marker
                result.append(char_obj)
            except ValueError as e:
                raise ValueError(f"Error parsing character at line {token.line}, col {token.column}: {e}")
        else:
            result.append(token)
    
    return result
