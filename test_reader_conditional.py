"""Test reader conditionals #+ and #-"""
from fclpy import lisptype, lispenv, state
from fclpy import runtime
from fclpy.readtable import get_current_readtable, Readtable

lispenv.setup_standard_environment()
env = lispenv.current_environment

rt = get_current_readtable()

# Test reading feature conditional
test = "'(#-wcl :cl #+wcl :lisp)"
print('Reading:', repr(test))
try:
    result = rt.read_from_string(test)
    print('Result:', result)
except Exception as e:
    print('Error:', e)

# Check *FEATURES*
features_sym = lisptype.COMMON_LISP_USER_PACKAGE.intern_symbol('*FEATURES*')
features = env.find_variable(features_sym)
print('\n*FEATURES*:', features)

# Now test loading rt-package
print('\n--- Loading rt.lsp and rt-package.lsp ---')
runtime.load_and_evaluate_file('../ansi-test/rt.lsp', env, verbose=False)
runtime.load_and_evaluate_file('../ansi-test/rt-package.lsp', env, verbose=False)

rt_pkg = lisptype.find_package('REGRESSION-TEST')
print('RT package:', rt_pkg)
print('RT USE-packages:', [p.name for p in rt_pkg.use_packages] if rt_pkg else None)
print('DEFTEST exported:', 'DEFTEST' in rt_pkg.external_symbols if rt_pkg else False)
