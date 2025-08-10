;; FCLPY Quick Start Examples
;; This file demonstrates the basic capabilities of FCLPY

;; Arithmetic operations
(+ 1 2 3 4 5)          ; Addition: 15
(* 2 3 4)              ; Multiplication: 24
(- 10 3)               ; Subtraction: 7
(/ 20 4)               ; Division: 5

;; Comparisons
(= 5 5)                ; Equality: T
(< 3 7)                ; Less than: T
(<= 5 5)               ; Less than or equal: T

;; List operations
(cons 1 '(2 3))        ; Create list: (1 2 3)
(car '(a b c))         ; First element: A
(cdr '(a b c))         ; Rest of list: (B C)
(list 1 2 3 4)         ; Create list: (1 2 3 4)

;; Function definition
(defun square (x)
  (* x x))

(defun factorial (n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

;; Test the functions
(square 5)             ; 25
(factorial 3)          ; 6

;; Variable assignment
(setq x 42)
(setq message "Hello, FCLPY!")

;; Conditional expressions
(if (> x 0)
    'positive
    'negative)

;; Local bindings
(let ((a 10) (b 20))
  (+ a b))             ; 30

;; Lambda expressions
((lambda (x y) (+ x y)) 3 4)  ; 7

;; Predicates
(atom 'symbol)         ; T
(null '())             ; T
(symbolp 'hello)       ; T
(numberp 42)           ; T

;; Logic operations
(and T T)              ; T
(or T nil)             ; T
(not nil)              ; T

;; Nested expressions
(+ (* 2 3) (- 10 5))   ; 11

;; Recursive list processing
(defun my-length (lst)
  (if (null lst)
      0
      (+ 1 (my-length (cdr lst)))))

(my-length '(a b c d))  ; 4
