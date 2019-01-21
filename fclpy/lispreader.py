

import sys
import re as _re
from fclpy.lisptype import LispSymbol

class LispStream():
    def __init__(self, fh):
        self.fh = fh
        self.tokens = []
        self.buff = []
    def unread_char(self, y):
        self.buff.append(y)
    def push_token(self, token):
        self.tokens.append(token)
    def has_token(self,token):
        return token in self.tokens
    def pop_token(self):
        return self.tokens.pop()
    def read_char(self):
        if len(self.buff) > 0:
            return self.buff.pop()
        return self.fh.read(1)
    def eof(self):
        return False

STDIN = LispStream(sys.stdin)

class LispReader():
    
    def __init__(self, get_macro_character, stream = STDIN):
        self.stream = stream
        self.get_macro_character = get_macro_character
    
    def read_1(self):
        toss = True
        while(toss):
            toss = False
            x = self.stream.read_char()
            if self.stream.eof():
                return None
            elif (not self.valid_char(x)):
                raise Exception("reader-error")
            elif self.whitespace_char(x):
                toss = True
            elif self.macro_character(x):
                return self.get_macro_character(x)(x,self.stream)
            elif self.single_escape_character(x):
                y = self.stream.read_char()
                if self.stream.eof():
                    raise Exception("reader-error")
                return self.read_8(y.upper())
            elif self.multiple_escape_character(x):
                return self.read_9("")
            else:
                return self.read_8(x.upper())
    def read_8(self, token):
        more = True
        while(more):
            y = self.stream.read_char()
            if self.terminating_macro_character(y):
                self.stream.unread_char(y)
                more = False
            elif self.whitespace_char(y):
                more = False
            else:
                token = token + y.upper()
        return self.read_10(token)
    
    
    def read_10(self, token):
        if _re.match("[0-9].*",token):
            return token
        return LispSymbol(token)
    def valid_char(self,c):
        return c == c
    
    def whitespace_char(self,c):
        return c in [" ","\t","\n","\r"]
       
    def eof(self,c):
        return c != c
    def macro_character(self,c ):
        return c in ["(",")","'",";"]
    def terminating_macro_character(self,c):
        return c in [")"]
    
    def non_terminating_macro_character(self,c):
        return c != c
    def single_escape_character(self,c):
        return c == "\\"
    def multiple_escape_character(self,c):
        return c == "\""
    

