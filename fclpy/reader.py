"""
Lisp Reader - converts text to Lisp objects.

This module integrates the tokenizer with the package system to read
Common Lisp expressions, properly interning symbols into packages.
"""

import fclpy.state as state
import fclpy.lisptype as lisptype
from fclpy.tokenizer import Tokenizer, TokenType
from fclpy.character import parse_character_token


class ReaderError(Exception):
    """Base exception for reader errors."""
    pass


class UnexpectedEOF(ReaderError):
    """EOF encountered when more input was expected."""
    pass


class UnbalancedParen(ReaderError):
    """Unbalanced parentheses in input."""
    pass


class InvalidNumber(ReaderError):
    """Invalid numeric literal."""
    pass


class Reader:
    """Main Lisp reader class.
    
    Reads tokens from source code and assembles them into Lisp objects,
    properly interning symbols into the current package.
    """
    
    def __init__(self, readtable=None, package=None):
        """Initialize the reader.
        
        Args:
            readtable: Optional readtable for macro processing
            package: Optional package to intern symbols into (defaults to state.current_package)
        """
        self.readtable = readtable or lisptype.COMMON_LISP_USER_PACKAGE
        self.package = package or (state.current_package or lisptype.COMMON_LISP_USER_PACKAGE)
        
    def read(self, text):
        """Read a single Lisp object from text.
        
        Args:
            text: String containing Lisp code
            
        Returns:
            Parsed Lisp object
            
        Raises:
            ReaderError and subclasses for malformed input
        """
        try:
            tokenizer = Tokenizer(text)
            tokens = tokenizer.tokenize_filtered()
        except ValueError as e:
            # Convert tokenizer ValueError to ReaderError
            error_msg = str(e)
            if "Unterminated string" in error_msg:
                raise UnexpectedEOF(f"Unexpected EOF: {error_msg}")
            elif "Unterminated block comment" in error_msg:
                raise UnexpectedEOF(f"Unexpected EOF in block comment")
            else:
                raise ReaderError(f"Tokenization error: {error_msg}")
        
        if not tokens or tokens[-1].type == TokenType.EOF:
            raise UnexpectedEOF("No tokens to read")
        
        self.tokens = tokens
        self.position = 0
        
        obj = self._read_object()
        return obj
    
    def read_all(self, text):
        """Read all Lisp objects from text.
        
        Args:
            text: String containing Lisp code
            
        Returns:
            List of parsed Lisp objects
        """
        try:
            tokenizer = Tokenizer(text)
            tokens = tokenizer.tokenize_filtered()
        except ValueError as e:
            # Convert tokenizer ValueError to ReaderError
            error_msg = str(e)
            if "Unterminated string" in error_msg:
                raise UnexpectedEOF(f"Unexpected EOF: {error_msg}")
            elif "Unterminated block comment" in error_msg:
                raise UnexpectedEOF(f"Unexpected EOF in block comment")
            else:
                raise ReaderError(f"Tokenization error: {error_msg}")
        
        self.tokens = tokens
        self.position = 0
        objects = []
        
        while self.position < len(self.tokens):
            token = self.tokens[self.position]
            if token.type == TokenType.EOF:
                break
            objects.append(self._read_object())
        
        return objects
    
    def _peek_token(self):
        """Peek at the current token without consuming it."""
        if self.position < len(self.tokens):
            return self.tokens[self.position]
        return None
    
    def _consume_token(self):
        """Consume and return the current token."""
        token = self._peek_token()
        if token:
            self.position += 1
        return token
    
    def _read_object(self):
        """Read a single object from current position."""
        token = self._peek_token()
        
        if token is None or token.type == TokenType.EOF:
            raise UnexpectedEOF("Unexpected EOF while reading")
        
        if token.type == TokenType.LPAREN:
            return self._read_list()
        elif token.type == TokenType.RPAREN:
            raise UnbalancedParen("Unexpected ')'")
        elif token.type == TokenType.INTEGER:
            self._consume_token()
            return int(token.value)
        elif token.type == TokenType.FLOAT:
            self._consume_token()
            return float(token.value)
        elif token.type == TokenType.RATIO:
            self._consume_token()
            # For now, return ratio as a symbolic form (will be improved later)
            # Parse ratio like "3/4" and create a list #<ratio 3/4>
            parts = token.value.split('/')
            # Could also just return the string representation for now
            return lisptype.lispCons(
                self.package.intern_symbol('RATIO'),
                lisptype.lispCons(
                    int(parts[0]),
                    lisptype.lispCons(int(parts[1]))
                )
            )
        elif token.type == TokenType.STRING:
            self._consume_token()
            return token.value
        elif token.type == TokenType.CHARACTER:
            self._consume_token()
            return parse_character_token(token)
        elif token.type == TokenType.SYMBOL:
            self._consume_token()
            # Intern symbol into current package
            return self.package.intern_symbol(token.value)
        elif token.type == TokenType.KEYWORD:
            self._consume_token()
            # Keywords intern into KEYWORD package
            name = token.value
            if name.startswith(':'):
                name = name[1:]
            return lisptype.intern_keyword(name)
        elif token.type == TokenType.HASH_QUOTE:
            # #' (function quote)
            self._consume_token()
            obj = self._read_object()
            # Return a function quote form: (quote <obj>)
            quote_sym = self.package.intern_symbol('FUNCTION')
            return lisptype.lispCons(quote_sym, lisptype.lispCons(obj))
        elif token.type == TokenType.HASH_LPAREN:
            # #() vector literal
            return self._read_vector()
        elif token.type == TokenType.QUOTE:
            # Regular quote '
            self._consume_token()
            obj = self._read_object()
            quote_sym = self.package.intern_symbol('QUOTE')
            return lisptype.lispCons(quote_sym, lisptype.lispCons(obj))
        elif token.type == TokenType.BACKQUOTE:
            # Backquote `
            self._consume_token()
            obj = self._read_object()
            backquote_sym = self.package.intern_symbol('QUASIQUOTE')
            return lisptype.lispCons(backquote_sym, lisptype.lispCons(obj))
        elif token.type == TokenType.COMMA:
            # Comma (unquote)
            self._consume_token()
            obj = self._read_object()
            unquote_sym = self.package.intern_symbol('UNQUOTE')
            return lisptype.lispCons(unquote_sym, lisptype.lispCons(obj))
        elif token.type == TokenType.COMMA_AT:
            # Comma-at (unquote-splicing)
            self._consume_token()
            obj = self._read_object()
            unquote_splicing_sym = self.package.intern_symbol('UNQUOTE-SPLICING')
            return lisptype.lispCons(unquote_splicing_sym, lisptype.lispCons(obj))
        elif token.type == TokenType.DOT:
            # Bare dot is an error
            raise ReaderError("Unexpected '.' outside of list context")
        else:
            raise ReaderError(f"Unexpected token type: {token.type}")
    
    def _read_list(self):
        """Read a list starting with '('."""
        self._consume_token()  # consume '('
        
        elements = []
        
        while True:
            token = self._peek_token()
            
            if token is None or token.type == TokenType.EOF:
                raise UnexpectedEOF("Unexpected EOF in list")
            
            if token.type == TokenType.RPAREN:
                self._consume_token()
                break
            
            if token.type == TokenType.DOT:
                # Dotted list: (a b . c)
                self._consume_token()
                if not elements:
                    raise ReaderError("Unexpected '.' at start of list")
                tail = self._read_object()
                
                token = self._peek_token()
                if token is None or token.type != TokenType.RPAREN:
                    raise ReaderError("Expected ')' after dotted tail")
                self._consume_token()  # consume ')'
                
                # Assemble list with dotted tail
                result = lisptype.NIL
                for elem in reversed(elements):
                    result = lisptype.lispCons(elem, result)
                # Replace last NIL with tail
                if isinstance(result, lisptype.lispCons):
                    # Find the last cons and set its cdr
                    current = result
                    while isinstance(current.cdr, lisptype.lispCons):
                        current = current.cdr
                    current.cdr = tail
                return result
            
            elements.append(self._read_object())
        
        # Assemble proper list
        result = lisptype.NIL
        for elem in reversed(elements):
            result = lisptype.lispCons(elem, result)
        return result
    
    def _read_vector(self):
        """Read a vector starting with '#('."""
        self._consume_token()  # consume '#('
        
        elements = []
        
        while True:
            token = self._peek_token()
            
            if token is None or token.type == TokenType.EOF:
                raise UnexpectedEOF("Unexpected EOF in vector")
            
            if token.type == TokenType.RPAREN:
                self._consume_token()
                break
            
            elements.append(self._read_object())
        
        # Return a vector as a Python list wrapped in a cons structure
        # This represents #(...) as (VECTOR element1 element2 ...)
        result = lisptype.NIL
        for elem in reversed(elements):
            result = lisptype.lispCons(elem, result)
        vector_sym = self.package.intern_symbol('VECTOR')
        return lisptype.lispCons(vector_sym, result)


def read(text, package=None):
    """Convenience function to read a single object.
    
    Args:
        text: String containing Lisp code
        package: Optional package to intern symbols into
        
    Returns:
        Parsed Lisp object
    """
    reader = Reader(package=package)
    return reader.read(text)


def read_all(text, package=None):
    """Convenience function to read all objects.
    
    Args:
        text: String containing Lisp code
        package: Optional package to intern symbols into
        
    Returns:
        List of parsed Lisp objects
    """
    reader = Reader(package=package)
    return reader.read_all(text)
