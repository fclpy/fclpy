import sys
sys.path.insert(0,'.')
from fclpy import lispenv
from fclpy.lispfunc import eval_string

lispenv.setup_standard_environment()

# Test evaluating a keyword
print("Test 1: Evaluate keyword :DATUM")
try:
    result = eval_string("(eval ':datum)")
    print(f"  Result: {result}, Type: {type(result)}")
except Exception as e:
    print(f"  Error: {type(e).__name__}: {e}")

print()
print("Test 2: Use keyword in list")
try:
    result = eval_string("(list ':datum 1 :other 2)")
    print(f"  Result: {result}")
except Exception as e:
    print(f"  Error: {type(e).__name__}: {e}")
