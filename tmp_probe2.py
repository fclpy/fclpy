import sys
sys.path.insert(0, '.')
from fclpy import lispenv
from fclpy.lispfunc import eval_string
from fclpy.lispfunc.io_read import read_from_string as _rfs  # may not exist; fallback below
lispenv.setup_standard_environment()

def roundtrip(form):
    s = eval_string(form)
    print(' printed:', s)
    return s

forms = [
    '(let ((x (list 1 2))) (write-to-string (cons x x) :circle t :readably t))',
    '(let ((s (make-symbol "X"))) (write-to-string (cons s s) :circle t))',
    '(let ((s1 (make-symbol "X")) (s2 (make-symbol "X"))) (write-to-string (list s1 s2 s1 s2) :circle t))',
    '(let ((a (list 17 nil))) (setf (cdr a) a) (write-to-string a :circle t :escape nil))',
    '(let ((x (list 1 2))) (write-to-string (list x (list x)) :circle t))',
    '(write-to-string (let ((a (list 1)) (b (list 2))) (setf (cdr a) b (cdr b) a) a) :circle t)',
    '(write-to-string (let ((a (list 1)) (b (list 2)) (c (list 3))) (setf (car a) b (car b) c (car c) a (cdr c) b) a) :circle t :readably t)',
    '(write-to-string (let ((x (list 1 2))) (list x x x)) :circle t)',
    '(write-to-string (let ((x (list 1 2))) (cons 5 x)) :circle t)',
    '(write-to-string (let ((v (vector 1 2))) (list v v)) :circle t)',
    '(write-to-string (let ((v (vector 1 2))) (cons v v)) :circle t)',
]
for f in forms:
    try:
        roundtrip(f)
    except Exception as e:
        print(f, '=> ERROR', type(e).__name__, e)
