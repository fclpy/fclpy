
;; Metacircular Evaluator - A Lisp interpreter written in Lisp
;; Based on the classic SICP implementation

;; Main evaluator function
(defun eval (exp env)
  (cond ((self-evaluating-p exp) exp)
        ((variable-p exp) (lookup-variable-value exp env))
        ((quoted-p exp) (text-of-quotation exp))
        ((assignment-p exp) (eval-assignment exp env))
        ((definition-p exp) (eval-definition exp env))
        ((if-p exp) (eval-if exp env))
        ((lambda-p exp) (make-procedure (lambda-parameters exp)
                                        (lambda-body exp)
                                        env))
        ((begin-p exp) (eval-sequence (begin-actions exp) env))
        ((cond-p exp) (eval (cond->if exp) env))
        ((application-p exp) (apply (eval (operator exp) env)
                                   (list-of-values (operands exp) env)))
        (t (error "Unknown expression type -- EVAL" exp))))

;; Apply function
(defun apply (procedure arguments)
  (cond ((primitive-procedure-p procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure-p procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (t (error "Unknown procedure type -- APPLY" procedure))))

;; Helper functions for evaluation
(defun list-of-values (exps env)
  (if (no-operands-p exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(defun eval-if (exp env)
  (if (true-p (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(defun eval-sequence (exps env)
  (cond ((last-exp-p exps) (eval (first-exp exps) env))
        (t (eval (first-exp exps) env)
           (eval-sequence (rest-exps exps) env))))

(defun eval-assignment (exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(defun eval-definition (exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

;; Expression type predicates
(defun self-evaluating-p (exp)
  (cond ((numberp exp) t)
        ((stringp exp) t)
        (t nil)))

(defun variable-p (exp) (symbolp exp))

(defun quoted-p (exp)
  (tagged-list-p exp 'quote))

(defun text-of-quotation (exp) (cadr exp))

(defun tagged-list-p (exp tag)
  (if (consp exp)
      (eq (car exp) tag)
      nil))

(defun assignment-p (exp)
  (tagged-list-p exp 'set!))

(defun assignment-variable (exp) (cadr exp))

(defun assignment-value (exp) (caddr exp))

(defun definition-p (exp)
  (tagged-list-p exp 'define))

(defun definition-variable (exp)
  (if (symbolp (cadr exp))
      (cadr exp)
      (caadr exp)))

(defun definition-value (exp)
  (if (symbolp (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; formal parameters
                   (cddr exp)))) ; body

(defun lambda-p (exp) (tagged-list-p exp 'lambda))

(defun lambda-parameters (exp) (cadr exp))

(defun lambda-body (exp) (cddr exp))

(defun make-lambda (parameters body)
  (cons 'lambda (cons parameters body)))

(defun if-p (exp) (tagged-list-p exp 'if))

(defun if-predicate (exp) (cadr exp))

(defun if-consequent (exp) (caddr exp))

(defun if-alternative (exp)
  (if (not (null (cdddr exp)))
      (cadddr exp)
      'false))

(defun make-if (predicate consequent alternative)
  (list 'if predicate consequent alternative))

(defun begin-p (exp) (tagged-list-p exp 'begin))

(defun begin-actions (exp) (cdr exp))

(defun last-exp-p (seq) (null (cdr seq)))

(defun first-exp (seq) (car seq))

(defun rest-exps (seq) (cdr seq))

(defun sequence->exp (seq)
  (cond ((null seq) seq)
        ((last-exp-p seq) (first-exp seq))
        (t (make-begin seq))))

(defun make-begin (seq) (cons 'begin seq))

(defun application-p (exp) (consp exp))

(defun operator (exp) (car exp))

(defun operands (exp) (cdr exp))

(defun no-operands-p (ops) (null ops))

(defun first-operand (ops) (car ops))

(defun rest-operands (ops) (cdr ops))

(defun cond-p (exp) (tagged-list-p exp 'cond))

(defun cond-clauses (exp) (cdr exp))

(defun cond-else-clause-p (clause)
  (eq (cond-predicate clause) 'else))

(defun cond-predicate (clause) (car clause))

(defun cond-actions (clause) (cdr clause))

(defun cond->if (exp)
  (expand-clauses (cond-clauses exp)))

(defun expand-clauses (clauses)
  (if (null clauses)
      'false                    ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause-p first)
            (if (null rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF" clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

;; Environment operations
(defun lookup-variable-value (var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null vars)
             (env-loop (enclosing-environment env)))
            ((eq var (car vars))
             (car vals))
            (t (scan (cdr vars) (cdr vals)))))
    (if (eq env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(defun set-variable-value! (var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null vars)
             (env-loop (enclosing-environment env)))
            ((eq var (car vars))
             (set-car! vals val))
            (t (scan (cdr vars) (cdr vals)))))
    (if (eq env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(defun define-variable! (var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null vars)
             (add-binding-to-frame! var val frame))
            ((eq var (car vars))
             (set-car! vals val))
            (t (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

;; Procedure operations
(defun make-procedure (parameters body env)
  (list 'procedure parameters body env))

(defun compound-procedure-p (p)
  (tagged-list-p p 'procedure))

(defun procedure-parameters (p) (cadr p))

(defun procedure-body (p) (caddr p))

(defun procedure-environment (p) (cadddr p))

(defun primitive-procedure-p (proc)
  (tagged-list-p proc 'primitive))

(defun primitive-implementation (proc) (cadr proc))

(defun apply-primitive-procedure (proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

;; Environment structure
(defun enclosing-environment (env) (cdr env))

(defun first-frame (env) (car env))

(defvar the-empty-environment '())

(defun make-frame (variables values)
  (cons variables values))

(defun frame-variables (frame) (car frame))

(defun frame-values (frame) (cdr frame))

(defun add-binding-to-frame! (var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(defun extend-environment (vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

;; Utility predicates
(defun true-p (x)
  (not (eq x 'false)))

(defun false-p (x)
  (eq x 'false))

;; Setup primitive procedures
(defun setup-environment ()
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                            (primitive-procedure-objects)
                            the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(defun primitive-procedure-names ()
  '(car cdr cons null eq atom not
    + - * / = < > <= >=
    numberp symbolp stringp consp listp
    cadr caddr cadddr cddr cdddr
    first second third fourth rest
    last length append reverse
    print read error gensym
    apply-in-underlying-scheme))

(defun primitive-procedure-objects ()
  (map (lambda (proc) (list 'primitive proc))
       (list car cdr cons null eq atom not
             + - * / = < > <= >=
             numberp symbolp stringp consp listp  
             cadr caddr cadddr cddr cdddr
             first second third fourth rest
             last length append reverse
             print read error gensym
             apply)))

(defvar the-global-environment (setup-environment))

;; Driver loop
(defun driver-loop ()
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(defvar input-prompt ";;; M-Eval input:")
(defvar output-prompt ";;; M-Eval value:")

(defun prompt-for-input (string)
  (newline) (newline) (display string) (newline))

(defun announce-output (string)
  (newline) (display string) (newline))

(defun user-print (object)
  (if (compound-procedure-p object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))
