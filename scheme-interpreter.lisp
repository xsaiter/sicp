(defvar true t)
(defvar false nil)

(defun true? (x)
    (not (equal x false)))

(defun false? (x)
  (not (true? x)))

(defun identity (x)
  x)

(defun pair? (x)
  (not (atom x)))

(defun s-eval (exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (s-apply (s-eval (operator exp) env)
                (list-of-values (operands exp) env)))
	(t (error "EVAL: unknown expression type" exp))))

(defun s-apply (procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
	((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (t (error "APPLY: unknow procedure type" procedure))))

(defun list-of-values (exps env)
  (if (no-operands? exps)
    '()
    (cons (eval (first-operand exps) env)
                (list-of-values (rest-operands exps) env))))

(defun eval-if (exp env)
  (if (true? (s-eval (if-predicate exp) env))
    (s-eval (if-consequent exp) env)
    (s-eval (if-alternative exp) env)))

(defun eval-sequence (exps env)
  (cond ((last-exp? exps) (s-eval (first-exp exps) env))
        (t (s-eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(defun eval-assignment (exp env)
  (set-variable-value! (assignment-variable exp)
                       (s-eval (assignment-value exp) env)
                       env)
  'ok)

(defun eval-definition (exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

(defun self-evaluating? (exp)
  (cond ((numberp exp) true)
        ((stringp exp) true)
        (t false)))

(defun variable? (exp)
  (symbolp exp))

(defun quoted? (exp)
  (tagged-list? exp 'quote))

(defun text-of-quotation (exp)
  (cadr exp))

(defun tagged-list? (exp tag)
  (if (pair? exp)
    (equal (car exp) (string-upcase tag))
    false))

(defun assignment? (exp)
  (tagged-list? exp 'set!))

(defun assignment-variable (exp)
  (cadr exp))

(defun assignment-value (exp)
  (caddr exp))

(defun definition? (exp)
  (tagged-list? exp 'define))

(defun definition-variable (exp)
  (if (symbolp (cadr exp))
    (cadr exp)
    (caadr exp)))

(defun definition-value (exp)
  (if (symbolp (cadr exp))
    (caddr exp)
    (make-lambda (cdadr exp)
                 (cddr exp))))

(defun lambda? (exp)
  (tagged-list? exp 'lambda))

(defun lambda-parameters (exp)
  (cadr exp))

(defun lambda-body (exp)
  (cddr exp))

(defun make-lambda (parameters body)
  (cons 'lambda (cons parameters body)))

(defun if? (exp)
  (tagged-list? exp 'if))

(defun if-predicate (exp)
  (cadr exp))

(defun if-consequent (exp)
  (caddr exp))

(defun if-alternative (exp)
  (if (not (null (cdddr exp)))
      (cadddr exp)
      false))

(defun make-if (predicate consequent alternative)
  (list 'if predicate consequent alternative))

(defun begin? (exp)
  (tagged-list? exp 'begin))

(defun begin-actions (exp)
  (cdr exp))

(defun last-exp? (seq)
  (null (cdr seq)))

(defun first-exp (seq)
  (car seq))

(defun rest-exps (seq)
  (cdr seq))

(defun sequence->exp (seq)
  (cond ((null seq) seq)
        ((last-exp? seq) (first-exp seq))
        (t (make-begin seq))))

(defun make-begin (seq)
  (cons 'begin seq))

(defun application? (exp)
  (pair? exp))

(defun operator (exp)
  (car exp))

(defun operands (exp)
  (cdr exp))

(defun no-operands? (ops)
  (null ops))

(defun first-operand (ops)
  (car ops))

(defun rest-operands (ops)
  (cdr ops))

(defun cond? (exp)
  (tagged-list? exp 'cond))

(defun cond-clauses (exp)
  (cdr exp))

(defun cond-else-clause? (clause)
  (equal (cond-predicate clause) 't))

(defun cond-predicate (clause)
  (car clause))

(defun cond-actions (clause)
  (cdr clause))

(defun cond->if (exp)
  (expand-clauses (cond-clauses exp)))

(defun expand-clauses (clauses)
  (if (null clauses)
      'false                         
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

(defun make-application (operator operands)
  (cons operator operands))

(defun make-define (name parameters body)
  (list 'define name (make-lambda parameters body)))

(defun make-procedure (parameters body env)
  (list 'procedure parameters body env))

(defun compound-procedure? (p)
  (tagged-list? p 'procedure))

(defun procedure-parameters (p)
  (cadr p))

(defun procedure-body (p)
  (caddr p))

(defun procedure-environment (p)
  (cadddr p))

(defun enclosing-environment (env)
  (cdr env))

(defun first-frame (env)
  (car env))

(defun the-empty-environment () '())

(defun make-frame (variables values)
  (cons variables values))

(defun frame-variables (frame)
  (car frame))

(defun frame-values (frame)
  (cdr frame))

(defun add-binding-to-the-frame! (var val frame)  
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(defun extend-environment (vars vals base-env)
  (if (= (length vars) (length vals))
    (cons (make-frame vars vals) base-env)
    (if (< (length vars) (length vals))
      (error "many arguments" vars vals)
      (error "few arguments" vars vals))))

(defun lookup-variable-value (var env)
  (defun env-loop (env)
    (defun scan (vars vals)
      (cond ((null vars)
             (env-loop (enclosing-environment env)))
            ((equal var (car vars))
             (car vals))
            (t (scan (cdr vars) (cdr vals)))))
    (if (equal env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values frame)))))
  (env-loop env))

(defun set-variable-value! (var val env)
  (defun env-loop (env)
    (defun (scan vars vals)
      (cond ((null vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (t (scan (cdr vars) (cdr vals)))))
    (if (equal env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(defun define-variable! (var val env)
  (let ((frame (first-frame env)))
    (defun scan (vars vals)
      (cond ((null vars)
             (add-binding-to-the-frame! var val frame))
            ((equal var (car vars))
             (set-car! vals val))
            (t (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

(defun primitive-procedure? (proc)    
    (tagged-list? proc 'primitive))

(defun primitive-implementation (proc)
  (cadr proc))

(defun primitive-procedures ()
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null null)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)))

(defun primitive-procedure-names ()
  (map car primitive-procedures))

(defun primitive-procedure-objects ()
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(defun setup-environment ()
  (let ((initial-env
          (extend-environment (primitive-procedure-names)
                              (primitive-procedure-objects)
                              the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(defvar the-global-environment (setup-environment))

(defun apply-primitive-procedure (proc args)
  (apply (primitive-implementation proc) args))

(defvar input-prompt ";;; M-Eval input:")
(defvar output-prompt ";;; M-Eval value:")

(defun driver-loop ()
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(defun prompt-for-input (string)
  (newline) (newline) (display string) (newline))

(defun announce-output (string)
  (newline) (display string) (newline))

(defun user-print (object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))
