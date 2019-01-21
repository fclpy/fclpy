

import importlib

import common_lisp.lispfunc

from common_lisp.lispenv import *
from common_lisp.lispfunc import *
import random

current_environment.read_module(common_lisp.lispfunc)

def parenthesis(char,stream):
    if char == "(":
        token = random.randint(1,32767)
        stream.push_token(token)
        lst = None
        while(stream.has_token(token)):
            value = read(stream)
            if value != None:
                if lst == None:
                    lst = cons(value,None)
                else:
                    lst = append(lst,cons(value,None))
        return lst
    if char == ")":
        stream.pop_token()
    return None

set_dispatch_macro_character("(",parenthesis)
set_dispatch_macro_character(")",parenthesis)


def __reload():
    importlib.reload(common_lisp.lisptype)
    importlib.reload(common_lisp.lispenv)
    importlib.reload(common_lisp.lispfunc)

    
