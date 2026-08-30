import sys
sys.path.insert(0,'.')
from fclpy import lispenv
from fclpy.lispfunc import eval_string

lispenv.setup_standard_environment()

# Test different ways to call ERROR
print("Test 1: ERROR with list form")
try:
    result = eval_string("(error (list 'type-error :datum 1 :expected-type '(quote integer)))")
except Exception as e:
    print(f"  {type(e).__name__}: {e}")

print()
print("Test 2: ERROR with make-condition")
try:
    result = eval_string("(error (make-condition 'type-error :datum 1 :expected-type '(quote integer)))")
except Exception as e:
    print(f"  {type(e).__name__}: {e}")
