"""
Lisp Reader - converts text to Lisp objects.

This module integrates the tokenizer with the package system to read
Common Lisp expressions, properly interning symbols into packages.
"""

import fclpy.state as state
import fclpy.lisptype as lisptype
from fclpy.tokenizer import Tokenizer, TokenType
from fclpy.character import parse_character_token


# Sentinel object used to mark feature conditionals that should be skipped
class _FeatureSkipMarker:
    """Marker object returned when a feature conditional skips a form."""
    def __repr__(self):
        return "#<FEATURE-SKIP-MARKER>"

_FEATURE_SKIP_MARKER = _FeatureSkipMarker()


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
            obj = self._read_object()
            # Skip feature markers at top level
            if not isinstance(obj, _FeatureSkipMarker):
                objects.append(obj)
        
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
            # Normalize Common Lisp exponent markers (D, F, S, L) to E for Python
            # 1.5D2 -> 1.5E2, 3.14F0 -> 3.14E0, etc.
            normalized = token.value
            for marker in 'dDfFsSsLl':
                normalized = normalized.replace(marker, 'e')
            return float(normalized)
        elif token.type in (TokenType.HEX_NUMBER, TokenType.BINARY_NUMBER, TokenType.OCTAL_NUMBER):
            # Radix numbers are already converted to integers by the tokenizer
            self._consume_token()
            return int(token.value)
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
            return lisptype.LispString(token.value)
        elif token.type == TokenType.CHARACTER:
            self._consume_token()
            return parse_character_token(token)
        elif token.type == TokenType.SYMBOL:
            self._consume_token()
            # Intern symbol into current package
            return self.package.intern_symbol(token.value)
        elif token.type == TokenType.UNINTERNED_SYMBOL:
            # #:name - uninterned symbol (not in any package)
            self._consume_token()
            # Create a fresh symbol with no package
            return lisptype.LispSymbol(token.value, package=None)
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
        elif token.type == TokenType.HASH_PLUS:
            # #+feature form - include form if feature is present
            return self._read_feature_conditional(True)
        elif token.type == TokenType.HASH_MINUS:
            # #-feature form - include form if feature is NOT present
            return self._read_feature_conditional(False)
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
                # Skip feature markers in tail
                if isinstance(tail, _FeatureSkipMarker):
                    tail = lisptype.NIL
                
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
            
            obj = self._read_object()
            # Skip feature markers (forms that were feature-conditional and skipped)
            if not isinstance(obj, _FeatureSkipMarker):
                elements.append(obj)
        
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
            
            obj = self._read_object()
            # Skip feature markers
            if not isinstance(obj, _FeatureSkipMarker):
                elements.append(obj)
        
        # Return a vector as a Python list wrapped in a cons structure
        # This represents #(...) as (VECTOR element1 element2 ...)
        result = lisptype.NIL
        for elem in reversed(elements):
            result = lisptype.lispCons(elem, result)
        vector_sym = self.package.intern_symbol('VECTOR')
        return lisptype.lispCons(vector_sym, result)
    
    def _read_feature_conditional(self, positive):
        """Read a feature conditional: #+feature form or #-feature form.
        
        Args:
            positive: True for #+, False for #-
            
        Returns:
            The form if feature test passes, otherwise a special marker.
        """
        self._consume_token()  # consume #+ or #-
        
        # Read the feature expression
        feature = self._read_object()
        
        # Read the form (regardless of whether we use it)
        form = self._read_object()
        
        # Check if feature is present
        if self._check_feature(feature, positive):
            return form
        else:
            # Feature test failed, return a special marker to indicate "nothing"
            # This marker will be filtered out by the caller
            return _FEATURE_SKIP_MARKER
    
    def _check_feature(self, feature, positive):
        """Check if a feature expression is satisfied.
        
        Args:
            feature: The feature expression (keyword, or compound with AND/OR/NOT)
            positive: True for #+ (include if present), False for #- (include if absent)
            
        Returns:
            True if the form should be included, False otherwise.
        """
        import fclpy.state as state
        
        # Get *FEATURES* list from environment
        features_sym = lisptype.COMMON_LISP_USER_PACKAGE.intern_symbol('*FEATURES*')
        features_list = lisptype.NIL
        if state.current_environment:
            features_list = state.current_environment.find_variable(features_sym)
            if features_list is None:
                features_list = lisptype.NIL
        
        # Check if feature is satisfied
        feature_present = self._eval_feature_expression(feature, features_list)
        
        # For #+, include if present; for #-, include if absent
        return feature_present if positive else not feature_present
    
    def _eval_feature_expression(self, feature, features_list):
        """Evaluate a feature expression against the features list.
        
        Supports:
          - Simple keyword: :FOO
          - (AND f1 f2 ...)
          - (OR f1 f2 ...)
          - (NOT f)
        
        Args:
            feature: The feature expression
            features_list: The *FEATURES* list
            
        Returns:
            True if the feature is present/satisfied.
        """
        # Handle keyword
        if isinstance(feature, lisptype.LispSymbol):
            # Check if symbol is in features list
            return self._symbol_in_list(feature, features_list)
        
        # Handle compound expression (list)
        if isinstance(feature, lisptype.lispCons):
            op = feature.car
            args = feature.cdr
            
            if isinstance(op, lisptype.LispSymbol):
                op_name = op.name.upper()
                
                if op_name == 'AND':
                    # All features must be present
                    current = args
                    while isinstance(current, lisptype.lispCons):
                        if not self._eval_feature_expression(current.car, features_list):
                            return False
                        current = current.cdr
                    return True
                
                elif op_name == 'OR':
                    # At least one feature must be present
                    current = args
                    while isinstance(current, lisptype.lispCons):
                        if self._eval_feature_expression(current.car, features_list):
                            return True
                        current = current.cdr
                    return False
                
                elif op_name == 'NOT':
                    # Feature must NOT be present
                    if isinstance(args, lisptype.lispCons):
                        return not self._eval_feature_expression(args.car, features_list)
                    return True
        
        # Unknown feature format - assume not present
        return False
    
    def _symbol_in_list(self, symbol, lst):
        """Check if a symbol is in a list (by name comparison for keywords)."""
        current = lst
        while isinstance(current, lisptype.lispCons):
            item = current.car
            if isinstance(item, lisptype.LispSymbol):
                # Compare by name (case-insensitive)
                if item.name.upper() == symbol.name.upper():
                    return True
            current = current.cdr
        return False


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
