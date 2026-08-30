import sys
sys.path.insert(0,'.')
from fclpy import lispenv
from fclpy.lispfunc import eval_string

lispenv.setup_standard_environment()

# Test ERROR with bare TYPE-ERROR
try:
    result = eval_string('(error type-error :datum 1 :expected-type (quote integer))')
    print('Result with bare TYPE-ERROR:', result)
except Exception as e:
    print(f'Error with bare TYPE-ERROR: {type(e).__name__}: {e}')

# Test ERROR with quoted TYPE-ERROR
try:
    result = eval_string("(error 'type-error :datum 1 :expected-type '(quote integer))")
    print('Result with quoted TYPE-ERROR:', result)
except Exception as e:
    print(f'Error with quoted TYPE-ERROR: {type(e).__name__}: {e}')
