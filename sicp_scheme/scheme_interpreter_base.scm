
#lang sicp

(define apply-in-underlying-scheme apply) ; from footnote on page 520

;; https://groups.csail.mit.edu/mac/ftpdir/scheme-7.4/doc-html/scheme_2.html#SEC24

;; A Scheme expression is a construct that returns a value. An expression may be a:
;;    1. literal,
;;    2. a variable reference,
;;    3. a special form,
;;    4. or a procedure call.

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)                                ; #1 Literal
        ((variable?        exp) (lookup-variable-value exp env))    ; #2 Variable Reference
        ((quoted?          exp) (text-of-quotation exp))            ; #3 Special Form
        ((assignment?      exp) (eval-assignment exp env))          ; #3 Special Form
        ((definition?      exp) (eval-definition exp env))          ; #3 Special Form
        ((if?              exp) (eval-if exp env))                  ; #3 Special Form
        ((lambda?          exp) (make-procedure                     ; #3 Special Form
                                 (lambda-parameters exp)
                                 (lambda-body exp)
                                 env))
        ((begin?           exp) (eval-sequence                      ; #3 Special Form
                                 (begin-actions exp) env))
        ((cond?            exp) (eval (cond->if exp) env))          ; #3 Special Form
        ((and?             exp) (eval-and exp env))                 ; #3 Special Form
        ((or?              exp) (eval-or exp env))                  ; #3 Special Form
        ((let?             exp) (eval (let->combination exp) env))  ; #3 Special Form
        ((application?     exp) (my-apply (eval (operator exp) env) ; #4 Procedure Call
                                          (list-of-values
                                           (operands exp) env)))
        (else
         (error "Unknown expression type: FAIL" exp))))

(define (my-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type: APPLY" procedure))))



(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (eval (first-exp exps) env))
        (else
         (eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

;; Exercise 4.1 (page 500-501)

(define (list-of-values-left-to-right exps env)
  (if (no-operands? exps)
      '()
      (let ((fst (eval (first-operand exps) env)))
        (cons fst
              (list-of-values (rest-operands exps) env)))))

;; End of Exercise 4.1

(define (self-evaluating? exp)
  (cond ((number?  exp) true)
        ((string?  exp) true)
        ;((boolean? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)      ; formal parameters
                   (cddr exp))))    ; body

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false ; no else clause

      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last: COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))


;; (cond ((assoc 'b '((a 1) (b 2))) => cadr)
;;       (else false))

(define (true? x) (not (eq? x false))) ; enables: eval-if; if? clause in eval
(define (false? x) (eq? x false))

(define (make-procedure parameters body env) ; enables: lambda? clause in eval
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))  ; at this point need to switch to #sicp
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env) ; enables: variable? clause of eval
  (define (env-loop env)
    (define (scan vars vals) ; :(
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env) ; enables: eval-assignment;
  (define (env-loop env)                  ;          assignment? clause of eval
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env) ; enables: eval-definition
  (let ((frame (first-frame env)))     ;          definition? clause of eval
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame) (frame-values frame))))

(define (primitive-procedure? proc) ; helps enable apply
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '/ /)
        ;;⟨ more primitives ⟩
        ))
(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define input-prompt  ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))
(define the-global-environment (setup-environment))




;; actual solution

(define (and? exp) (tagged-list? exp 'and))
(define (or? exp)  (tagged-list? exp 'or))




;; sighs will come back after 4.1.4
;; (not knowing what to pass as env should have been a clue)

(define (eval-or tag-and-exps env)
  (define (iter exps)
    (cond ((null? exps) false)
          ((true? (eval (car exps) env)) true)
          (else (iter (cdr exps)))))
  (iter (cdr tag-and-exps)))


(define (eval-and tag-and-exps env)
  (define (iter exps)
    (cond ((null? exps) true)
          ((false? (eval (car exps) env)) false)
          (else (iter (cdr exps)))))
  (iter (cdr tag-and-exps)))


;; let* expression
(define (let*? expr) (tagged-list? expr 'let*))
(define (let*-body expr) (caddr expr))
(define (let*-inits expr) (cadr expr))
(define (let*->nested-lets expr)
  (let ((inits (let*-inits expr))
        (body (let*-body expr)))
    (define (make-lets exprs)
      (if (null? exprs)
          body
          (list 'let (list (car exprs)) (make-lets (cdr exprs)))))
    (make-lets inits)))


(define (named-let? expr) (and (let? expr) (symbol? (cadr expr))))
(define (named-let-func-name expr) (cadr expr))
(define (named-let-func-body expr) (cadddr expr))
(define (named-let-func-parameters expr) (map car (caddr expr)))
(define (named-let-func-inits expr) (map cadr (caddr expr)))
(define (named-let->func expr)
  (list 'define
        (cons (named-let-func-name expr) (named-let-func-parameters expr))
        (named-let-func-body expr)))
;; (define (let->combination expr)
;;   (if (named-let? expr)
;;       (sequence->exp
;;        (list (named-let->func expr)
;;              (cons (named-let-func-name expr) (named-let-func-inits expr))))
;;       (cons (make-lambda (let-vars expr)
;;                          (list (let-body expr)))
;;             (let-inits expr))))
;;(let*->nested-lets '(let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (* x z)))
(define (let? exp) (tagged-list? exp 'let))

(named-let? '(let ((a 1) (b 0) (count n))))
(define (let->combination exp)
  (let* ((bindings       (cadr exp))
         (binding-names  (map car bindings))
         (binding-values (map cadr bindings))
         (body     (cddr exp)))
    (cons (make-lambda binding-names
                       body)
          binding-values)))


(define (while? exp) (tagged-list? exp 'while))

(define (while->combination exp)
  (let ((predicate (cadr exp))
        (body      (cddr exp)))
    (sequence->exp
     (list (list 'define
                 (list 'while-iter)
                 (make-if predicate
                          (sequence->exp
                           (append body (list (list 'while-iter))))
                          'true))
           (list 'while-iter)))))

;;(driver-loop)

(cadr '((assoc 'b '((a 1) (b 2))) => cadr))
