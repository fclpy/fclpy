import sys
sys.path.insert(0,'.')
from fclpy import lispenv
from fclpy.lispfunc import eval_string

lispenv.setup_standard_environment()

# Expand CCASE to see the ERROR form
expansion = eval_string('(macroexpand-1 (quote (ccase x)))')
print('CCASE with no clauses:')
print(expansion)
print()

# Extract the ERROR form from the expansion
# It should be in the T clause of the COND
print('Testing the extracted ERROR form...')

# Simpler: just evaluate a CCASE that signals
try:
    result = eval_string('(let ((x 1)) (ccase x))')
    print('Result:', result)
except Exception as e:
    print(f'Error: {type(e).__name__}: {e}')
