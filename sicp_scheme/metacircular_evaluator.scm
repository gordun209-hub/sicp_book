#lang sicp
;; metacircular evaluator is essentially a scheme formulation of
;; the environment model of evaluation.

;; 1. to evaluate a combination, evaluate the subexpressions and then
;; apply the value of the operator subexpression to the values of the
;; operand subexpressions

;;2. to apply a compound procedure to a set of arguments, evaluate the body of
;; the procedure in a new environment. To construct this environment, extend
;; the environment part of the procedure object by a fromae in which the formal
;; parameters of the procedure are bound to the arguments to whihc procedure is
;; applied


;; evaluiation process can be described as the interplay between two proc
;; eval and apply
;; eval takes as arguments an expression and an environment. It classifies
;; the espression and directs its evaluation
(define (list-of-values exps env)
  ;; eger ki operand yoksa nil ver
  (if (no-operands? exps)
      '()
      ;; operand var ise  ilkini evalle ve geri kalaniyla consla
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))


;; COnditionals
(define (eval-if exp env)
  ;; if predicate is true eval first, if false eval second
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

;; Sequences
(define (eval-sequence exps env)
  ;; last exps ise eval first that is also last
  (cond ((last-exp? exps)
         (eval (first-exp exps) env))
        (else
         ;; else eval first and eval-sequence to rest
         (eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) env))))
;; Assignment and definitions

(define (eval-assignment exp env)
  ;; use set-variable-value! for assigning
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

;; for integers and strings that are self-evaluating
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

;; values are represented by symbols
(define (variable? exp) (symbol? exp))

;; quotiations have the form (quote <text-of-quotation>)

(define (quoted? exp) (tagged-list? exp 'quote))

(define (text-of-quotiation exp) (cadr exp))
;; tagged listi sole buluyoz -> if pair ise ve ilk elementi 'quote ise
;; true deilse false
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;; assignments have the form (set! <var> <value>)
;; if assignment ise tag i set! olmasi lasm
(define (assignment? exp) (tagged-list? exp 'set!))
;; variablesini almak icin cadr ini almak lasm
(define (assignment-variable exp) (cadr exp))
;; valuesi icin ise caddr ini almak lasm
(define (assignment-value exp) (caddr exp))

;; definitions have the form
;; (define <var> <value>)
;; or
;; (define <var>
;;   (lambda (<parameter1) ... <parameter n>)
;;      <body>))
;; tag i 'define ise definitiondur
(define (definition? exp) (tagged-list? exp 'define))

;; burai tam anlamadm?
;; sanrm sole bise ('define (laa 2)) gibi ise
;; yani cadr i exp ise exp deilse oteki tarafi wercn
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caar exp)))
;;burada ise cadr symbol ise
;; caddr exp yi weriyoz yani ?

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      ;; deilse lambda olusturuyoz yeniden
      (make-lambda (cdadr exp) ;; formal parameters
                   (cddr exp))));; body

;; lambda expressions are list that begin with the symbol lambda
;; tagged listi lambdami bak lambdaysa true
(define (lambda? exp) (tagged-list? exp 'lambda))
;; parametreleri cadr yani sonraki listin car i
(define (lambda-parameters exp) (cadr exp))
;; bodysi ise cddr yani 2. list
(define (lambda-body exp) (cddr exp))

;; we also provide a constructor for lambda expressions, which is
;; used by definition-value, above
(define (make-lambda parameters body)
  ;; tag with 'lambda and cons other parameters and body
  (cons 'lambda (cons parameters body)))


;; conditionals begin with if and have a predicate, a consequent and
;; an (optional) alternative. If the expression has no alternative part
;; we provide false as the alternative

;; look for tag if it is 'if
(define (if? exp) (tagged-list? exp 'if))
;; take predicate with cadr
(define (if-predicate exp) (cadr exp))
;; take consequent with caddr
(define (if-consequent exp) (caddr exp))
;; if not null that cdddr exp, return cadddr exp else false
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

;; we also provide a constructor for if expressions to be used by
;; cond->if to transform cond expressions into if expressions
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

;; begin packages a seq of exp into a single expression.

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))


;;  we also include a constructor sequence->exp
;; (for use by cond->if) that transforms a sequence into a single
;; expression using begin if necessary

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

;; A procedure application is any compound expression that is not one of
;; above expression types. the car of the exp is operator and cdr is list
;; of operands

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

;; evaluator data structures
;; Testing predicates

;; for conditionals, we accept anything to be true that is not
;; explicit false object
(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))


;; Compound procedures are constructed from parameters, procedure bodies,
;; and environments using the constructor make-procedure
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

;; Operations on environments

;; (lookup-variable-value <var> <env>)
;; returns the value that is bound to the symbol <var> in env <env>
;; or signals an error if variable is unbound

;; (extend-environment <variables> <values> <base-env>)
;; Returns a new env, consisting of a new frame which the symbols
;; in the list <variables> are bound to the corresponding elements
;; in the list <values>, where the enclosing env is <base-env>


;; (define-variable! <var> <value> <env>)
;; adds to the first frame in the environment <env> a new binding
;; that associates the variable <var> with the value <value>

;; (set-variable-value! <var> <value> <env>)
;; changes the binding of the variable <var> in the environment <env>
;; so that the variable is now bound to the value <value>, or
;; signals an error if variable is unbound


;; to implement these ops we represent an env as a list of frames
;; the enclosing env of an env is the cdr of the list
;; the empty env is simply the empty list

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

;; each frame of an environment is represented as a pair of lists
;; a list of the variables bound in that frame and a list of assoc values
(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      ;; if vars is null look enclosing env
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ;; if find it return val
            ((eq? var (car vars)) (car vals))
            ;; else scan more
            (else (scan (cdr vars) (cdr vals)))))
    ;; if env is emptry env return error
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        ;; choose first frame
        (let ((frame (first-frame env)))
          ;; scan trough variables and values
          (scan (frame-variables frame)
                (frame-values frame)))))
  ;; start loop
  (env-loop env))

;; to set a variable to a new value in specified env, we scan for
;; the variable, just as in lookup-variable-value and change the
;; corresponding value when we find it

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


;; to define a var, we search and change if exists or adjoin
(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame) (frame-values frame))))

;; we include snytax procedures that extract the parts of a cond exp
;; and a procedure cond->if that transforms cond expressions into if exp
;; A case analysis begins with cond and has a list of predicate-action
;; clauses.A clause is an else clause if its predicate is the symbol else
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause)
  (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isnt last : COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))



(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        ))
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

(define (apply procedure arguments)
  ;; primitive ise apply with apply-primitive-procedure
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ;; compound ise first eval sequence ??
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error "Unknown procedure type: APPLY"))))

(define apply-in-underlying-scheme apply)

(define (eval exp env)
  ;; self eval ise kendisini wer
  (cond ((self-evaluating? exp) exp)
        ;; variable ise variable icin env e bak
        ((variable? exp) (lookup-variable-value exp env))
        ;; quoted ise textini ver
        ((quoted? exp) (text-of-quotiation exp))
        ;; assignment ise env e bak
        ((assignment? exp) (eval-assignment exp env))
        ;; definition ise gne env e bak
        ((definition? exp) (eval-definition exp env))
        ;; if ise ozel case uygula
        ((if? exp) (eval-if exp env))
        ;; lambda ise lambdayi ac (procedure yap)
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ;; begin ise sequence ile uygula
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ;; cond ise if e cevir
        ((cond? exp) (eval (cond->if exp) env))
        ;; application ise apply?
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type: EVAL" exp))))



(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))
(define the-global-environment (setup-environment))

(driver-loop)


