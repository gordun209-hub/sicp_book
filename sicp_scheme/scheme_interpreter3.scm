#lang sicp


(define apply-in-underlying-scheme apply)
(define (sicp-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((application? exp)
         (apply (sicp-eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type :EVAL" exp))))


(define (analyze exp)
  (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((application? exp) (analyze-application exp))
        (else (error "Unknowon expression type : ANALYZE" exp))))
(define (analyze-self-evaluating exp)
  (lambda () exp))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda () qval)))

(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))
(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))
(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env) (if (true? (pproc env))
                   (cproc env)
                   (aproc env)))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env) (make-procedure vars bproc env))))

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs) (error "Empty sequence: ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application
       (fproc env)
       (map (lambda (aproc) (aproc env))
            aprocs)))))
(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment
           (procedure-parameters proc)
           args
           (procedure-environment proc))))
        (else
         (error "Unknown procedure type: EXECUTE-APPLICATION" proc))))


(define (sicp-apply procedure arguments)
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
          "Unknwon procedure type : APPLY" procedure))))


;; PROCEDURE ARGUMENTS
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (sicp-eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))


;; CONDITIONALS
(define (eval-if exp env)
  (if (true? (sicp-eval (if-predicate exp) env))
      (sicp-eval (if-consequent exp) env)
      (sicp-eval (if-alternative exp) env)))


;; SEQUENCES
(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (sicp-eval (first-exp exps) env))
        (else
         (sicp-eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) env))))



;; ASSIGNMENTS AND DEFINITIONS
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (sicp-eval (assignment-value exp) env)
                       env)
  'ok)


(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (sicp-eval (definition-value exp) env)
    env)
  'ok)


;; REPRESENTING EXPRESSIONS


(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
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
      (make-lambda (cdadr exp)
                   (cddr exp))))



(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))


(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))


(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cddr exp)))
      (cadddr exp)
      'false))


(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))



(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (first-operand ops) (car ops))


(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (rest-operands ops) (cdr ops))
(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))


;; (define (make-procedure parameters body env)
;;   (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))





;; ENVIRONMENTS BURA ONEMLI AGA  ANLAYMAIOM MQ

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame  (cons val (cdr frame))))
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))


(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (if (eq? '*unassigned* (car vals))
                 (error "Variable Unassigned -- LOOKUP-VARIABLE-VALUE" var)
                 (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))


(define (make-let bindings body)
  (cons 'let (cons bindings body)))

(define (make-assignment var exp)
  (list 'set! var exp))

(define (scan-out-defines body)
  (define (collect seq defs exps)
    (if (null? seq)
        (cons defs exps)
        (if (definition? (car seq))
            (collect (cdr seq) (cons (car seq) defs) exps)
            (collect (cdr seq) defs (cons (car seq) exps)))))
  (let ((pair (collect body '() '())))
    (let ((defs (car pair)) (exps (cdr pair)))
      (make-let (map (lambda (def)
                       (list (definition-variable def)
                             '*unassigned*))
                     defs)
                (append
                 (map (lambda (def)
                        (make-assignment (definition-variable def)
                                         (definition-value def)))
                      defs)
                 exps)))))

; make-procedure is better because we can easily explore other transformations
; along with the fact of repeated calculation everytime when procedure-body is accessed

;; a
;; letrec expression
(define (letrec? expr) (tagged-list? expr 'letrec))
(define (letrec-inits expr) (cadr expr))
(define (letrec-body expr) (cddr expr))
(define (declare-variables expr)
  (map (lambda (x) (list (car x) '*unassigned*)) (letrec-inits expr)))
(define (set-variables expr)
  (map (lambda (x) (list 'set! (car x) (cadr x))) (letrec-inits expr)))
(define (letrec->let expr)
  (list 'let (declare-variables expr)
        (make-begin (append (set-variables expr) (letrec-body expr)))))

(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))


(define (make-procedure-with-transformation transformation)
  (define (make-procedure parameters body env)
    (list 'procedure parameters (transformation body) env))
  make-procedure)

(define (set-variable-value! var val env)
  (define (env-loop env)
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

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame) (frame-values frame))))


(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))


(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))


(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list '+ +)
        (list 'null? null?)))

(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (sicp-eval input the-global-environment)))
      (announce-output output-prompt)))
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

(define the-global-environment (setup-environment))
(driver-loop)
