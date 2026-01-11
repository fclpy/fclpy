#!/usr/bin/env python3
import sys
import io
sys.path.insert(0, '.')
from fclpy import lispenv
import fclpy.state as state
from fclpy.lispreader import LispReader, LispStream
from fclpy.readtable import get_current_readtable
from fclpy.lispfunc.evaluation_core import eval

lispenv.setup_standard_environment()
env = state.current_environment

# Parse the code
code = '(DEFUN ADD-N (N) "Add N to argument" (+ DUMMY N))'
rt = get_current_readtable()
s = io.StringIO(code)
reader = LispReader(rt.get_macro_character, LispStream(s))
form = reader.read_1()

# Check what the docstring is
from fclpy.lispfunc.core import car, cdr, _consp_internal
args = cdr(form)
param_list = car(cdr(args))
body = cdr(cdr(args))

if _consp_internal(body):
    first_form = car(body)
    print(f'first_form type: {type(first_form)}')
    print(f'first_form value: {first_form!r}')
    print(f'isinstance str: {isinstance(first_form, str)}')
    
    # Try to get the actual string
    from fclpy.lisptype_basic import LispString
    print(f'isinstance LispString: {isinstance(first_form, LispString)}')
