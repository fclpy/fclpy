import sys
sys.path.insert(0, '.')
from fclpy import lispenv
from fclpy.lispfunc import eval_string
lispenv.setup_standard_environment()
forms = [
    '(let ((*print-base* 24) (*print-escape* t)) (write-to-string nil))',
    '(let ((*print-base* 24) (*print-readably* t)) (write-to-string nil))',
    '(let ((*print-base* 30) (*print-readably* t)) (write-to-string t))',
    '(let ((*print-base* 24) (*print-readably* t)) (write-to-string t))',
    '(let ((*print-base* 23) (*print-readably* t)) (write-to-string nil))',
    '(let ((*print-base* 24) (*print-escape* t)) (write-to-string (list nil t 1)))',
    # round trip
    '(let ((*print-base* 24) (*print-radix* t) (*read-base* 24) (*print-readably* t)) (let ((s (write-to-string (list nil t 905)))) (list s (read-from-string s))))',
    # escape off must stay bare
    '(let ((*print-base* 24)) (princ-to-string nil))',
    # base 10 unchanged
    '(prin1-to-string nil)',
    '(prin1-to-string t)',
]
for f in forms:
    try:
        print(f, '=>', eval_string(f))
    except Exception as e:
        print(f, '=> ERROR', type(e).__name__, e)
