import sys
sys.path.insert(0, 'fclpy')

from fclpy import lispenv
# Explicitly call setup to register functions
lispenv.setup_standard_environment()

from fclpy import lispfunc, lispreader
from fclpy.readtable import get_current_readtable
import io

env = lispenv.current_environment

# Test 6 first: Check if COERCE is external in COMMON-LISP directly
from fclpy import lisptype
cl_pkg = lisptype.COMMON_LISP_PACKAGE
print(f'6. COMMON-LISP package: {cl_pkg}')
print(f'6a. COERCE in cl_pkg.symbols: {"COERCE" in cl_pkg.symbols}')
print(f'6b. COERCE in cl_pkg.external_symbols: {"COERCE" in cl_pkg.external_symbols}')
print(f'6c. CAR in cl_pkg.symbols: {"CAR" in cl_pkg.symbols}')
print(f'6d. First 10 symbols: {list(cl_pkg.symbols.keys())[:10]}')
print(f'6e. Number of externals: {len(cl_pkg.external_symbols)}')

# Test 1: Create package and check use-list
text = '(make-package :cl-test :use (list :common-lisp))'
stream = lispreader.LispStream(io.StringIO(text))
rt = get_current_readtable()
reader = lispreader.LispReader(rt.get_macro_character, stream)
expr = reader.read_1()
result = lispfunc.eval(expr, env)
print(f'1. Created package: {result}')

# Test 2: Check use-list
text2 = '(package-use-list (find-package :cl-test))'
stream2 = lispreader.LispStream(io.StringIO(text2))
reader2 = lispreader.LispReader(rt.get_macro_character, stream2)
expr2 = reader2.read_1()
result2 = lispfunc.eval(expr2, env)
print(f'2. Use-list: {result2}')

# Test 3: Find COERCE in CL-TEST
text3 = '(find-symbol "COERCE" :cl-test)'
stream3 = lispreader.LispStream(io.StringIO(text3))
reader3 = lispreader.LispReader(rt.get_macro_character, stream3)
expr3 = reader3.read_1()
result3 = lispfunc.eval(expr3, env)
print(f'3. Find COERCE in CL-TEST: {result3}')

# Test 4: Find COERCE in COMMON-LISP
text4 = '(find-symbol "COERCE" :common-lisp)'
stream4 = lispreader.LispStream(io.StringIO(text4))
reader4 = lispreader.LispReader(rt.get_macro_character, stream4)
expr4 = reader4.read_1()
result4 = lispfunc.eval(expr4, env)
print(f'4. Find COERCE in COMMON-LISP: {result4}')

# Test 5: Try using COERCE in CL-TEST package
try:
    text5 = '(progn (in-package :cl-test) (coerce \'(1 2 3) \'vector))'
    stream5 = lispreader.LispStream(io.StringIO(text5))
    reader5 = lispreader.LispReader(rt.get_macro_character, stream5)
    expr5 = reader5.read_1()
    result5 = lispfunc.eval(expr5, env)
    print(f'5. COERCE result in CL-TEST: {result5}')
except Exception as e:
    print(f'5. Error using COERCE: {e}')
