"""Test loading init.lsp"""
from fclpy import lispenv
from fclpy import runtime

lispenv.setup_standard_environment()
env = lispenv.current_environment

print('Loading init.lsp...')
try:
    runtime.load_and_evaluate_file('../ansi-test/init.lsp', env, verbose=False)
    print('SUCCESS: init.lsp loaded without errors!')
except Exception as e:
    print(f'ERROR: {e}')

# Check if DEFTEST is now accessible
from fclpy import lisptype
cl_test = lisptype.find_package('CL-TEST')
if cl_test:
    print(f'\nCL-TEST package exists')
    print(f'CL-TEST USE-packages: {[p.name for p in cl_test.use_packages]}')
    # Check if DEFTEST is accessible
    deftest = cl_test.find_symbol('DEFTEST')
    print(f'DEFTEST in CL-TEST: {deftest}')
else:
    print('\nCL-TEST package not found')
