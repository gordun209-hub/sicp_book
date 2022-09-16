
#lang racket
(require rnrs/mutable-pairs-6)
(require compatibility/mlist)

; 4.1.1 The Core of the Evaluator

;; Primitive Expressions =>
;; for self-evaluating expressions, such as numbers, eval returns
;; the self expression itself
;; eval must look up variables in the environment to find their values

;; Special Forms
;; for quoted expressions, eval returns the expression that was quoted
;; an assignment to (or a definition of) a variable must recursively
;; call eval to compute the nev value to be associated with the variable
;; The environment must be modified to change (or create) the binding
;; of  the variable

;; An if expression requires special processing of its parts, so as to
;; evaluate the consequent if the predicate is true, and otherwise to
;; evaluate the alternative.

;; A lambda expression must be transformed into an applicable procedure
;; by packaging  together the parameters and body specified by lambda
;; expression with the env of evaluation

;; A begin expression requires evaluating its sequence of expressions
;; in the order in which they appear.

;; A case analysis (cond) is transformed into a nest of if expressions
;; and then evaluated


;; Combinations
;; for a procedure application, eval must recursively evaluate the
;; operator part and the operands of the combination. The resulting
;; procedure and arguments are passed to apply,  which handles the actual
;; procedure application
(define (sicp-eval exp env)
  ;; self eval ise wer direkt
  (cond ((self-evaluating? exp) exp)
        ;; variable ise env e bak
        ((variable? exp) (lookup-variable-value exp env))
        ;; quoted ise ver quote olmadan
        ((quoted? exp) (text-of-quotation exp))
        ;; asiggnment ise eval-assignmenti cair?
        ((assignment? exp) (eval-assignment exp env))
        ;; ayni sekil
        ((definition? exp) (eval-definition exp env))
        ((let? exp) (sicp-eval (let->combination exp) env))
        ((let*? exp) (sicp-eval (let*->nested-lets exp) env))
        ;; if ise if i evalle
        ((and? exp) (eval-and exp env))
        ((or? exp) (eval-or exp env))
        ((if? exp) (eval-if exp env))
        ;; lambda ise procedure yap
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ;; begin? ise sequence olarak evalle
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ;; cond ise evalle
        ((cond? exp) (eval-cond exp env))
        ;; application ise apply i cagir,
        ((application? exp)
         (sicp-apply (sicp-eval (operator exp) env)
                     (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type: EVAL" exp))))


;; Apply
;; Apply takes two arguments, a procedure and a list of arguments to which
;; the procedure should be applied. apply classifies procedures into
;; two kinds: it calls apply-primitive-procedure to apply primitives
;; it applies compound procedures by sequentially evaluating the
;; expressions that make up the body of the procedure. The environment
;; for the evaluation of the body of a compound procedure is constructed
;; by extending the base environment carried by the procedure to include
;; a frame that binds the parameters of the procedure to the arguments to
;; which the procedure is to be applied.
(define (sicp-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         ;; that is basic apply built in
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           ;; env e proc parametrelerini ekle
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type: APPLY" procedure))))


(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (sicp-eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))


;; If selectors and constructors ;;
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (eval-if exp env)
  (if (true? (sicp-eval (if-predicate exp) env))
      (sicp-eval (if-consequent exp) env)
      (sicp-eval (if-alternative exp) env)))

;; bakalm napio
;; bunun returnu bildigin deger aga
;; Sequence evaluator selectors and constrcutors ;;
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (eval-sequence exps env)
  ;; last exp ise eval bildigin
  (cond ((last-exp? exps)
         (sicp-eval (first-exp exps) env))
        (else
         ;; daha warsa ilkini evalle ve recurla rest
         (sicp-eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) env))))


;; evaluation of set!
(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))


(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (sicp-eval (assignment-value exp) env)
                       env)
  'ok)
;; evaluation of define
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) ; formla
                   (cddr exp)))) ; body
(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (eval-definition exp env)
  ;; bu env e katiyo sanrm bakam bi
  (define-variable! (definition-variable exp)
    (sicp-eval (definition-value exp) env)
    env)
  'ok)

; 4.1.2 Representing Expressions
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))
(define (variable? exp) (symbol? exp))

;; quotation
(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))
;; selector
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;; lambda constructor and selectors
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (or? exp) (tagged-list? exp 'or))
(define (and? exp) (tagged-list? exp 'and))
(define (or-body exp) (cdr exp))
(define (first-predicate exp) (cadr exp))

(define (predicates exp) (cdr exp))
(define (eval-and exp env)
  (cond ((null? (predicates exp)) true)
        ((false? (first-predicate exp)) #f)
        (else (eval-and (predicates exp) env))))


(define (eval-or exp env)
  (cond ((null? (predicates exp)) #f)
        ((true? (first-predicate exp)) #t)
        (else (eval-or (predicates exp) env))))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
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
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (eval-cond exp env)
  (let ((clauses (cdr exp)))
    (define (imply-clause? clause) (eq? (cadr clause)  '=>))
    (define (rec-eval clauses)
      (if (null? clauses) 'false; checked all, no else-clause
          (let ((first-clause (car clauses)))
            (cond ((cond-else-clause? first-clause) (eval-sequence (cond-actions first-clause) env))
                  ((imply-clause? first-clause) (let ((evaluated (sicp-eval (cond-predicate first-clause) env)))
                                                  (if (true? evaluated)
                                                      (sicp-apply (sicp-eval (caddr first-clause) env)
                                                                  (list evaluated))
                                                      'false)))
                  (else (if (true? (sicp-eval (cond-predicate first-clause) env))
                            (eval-sequence (cond-actions first-clause) env)
                            'false))))))
    (rec-eval clauses)))

;; LET ============================================
(define (let-bindings exp)
  (cadr exp))
(define (let-body exp)
  (cddr exp))

(define (bindings->params bindings)
  (if (null? bindings)
      bindings
      (cons
       (caar bindings)
       (bindings->params (cdr bindings)))))

(define (bindings->args bindings)
  (if (null? bindings)
      bindings
      (cons
       (cadar bindings)
       (bindings->args (cdr bindings)))))
;; let expression
(define (let? expr) (tagged-list? expr 'let))
(define (let-vars expr) (map car (cadr expr)))
(define (let-inits expr) (map cadr (cadr expr)))

; 4.1.3 Evaluat
;; (define (let->combination exp)
;;   (cons
;;    (make-lambda
;;     (bindings->params (let-bindings exp))
;;     (let-body exp))
;;    (bindings->args (let-bindings exp))))
(define (let->combination expr)
  (if (named-let? expr)
      (sequence->exp
       (list (named-let->func expr)
             (cons (named-let-func-name expr) (named-let-func-inits expr))))
      (cons (make-lambda (let-vars expr)
                         (list (let-body expr)))
            (let-inits expr))))
(define (named-let? expr) (and (let? expr) (symbol? (cadr expr))))

(define (named-let-func-name expr) (cadr expr))

(define (named-let-func-body expr) (cadddr expr))

(define (named-let-func-parameters expr) (map car (caddr expr)))

(define (named-let-func-inits expr) (map cadr (caddr expr)))

(define (named-let->func expr)
  (list 'define
        (cons (named-let-func-name expr) (named-let-func-parameters expr))
        (named-let-func-body expr)))

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
;; ============================
(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

(define (make-procedure parameters body env)
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
  (mcons (list->mlist variables) (list->mlist values)))
(define (frame-variables frame) (mcar frame))
(define (frame-values frame) (mcdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (mcons var (mcar frame)))
  (set-cdr! frame (mcons val (mcdr frame))))

;; basicly cons aga
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

;; find bounded variables if exissts
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ;; bulunca returnla
            ((eq? var (mcar vars)) (mcar vals))
            (else (scan (mcdr vars) (mcdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))
;; set! assignment?
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (mcar vars)) (set-car! vals val))
            (else (scan (mcdr vars) (mcdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))
;; definiiton
(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (mcar vars)) (set-car! vals val))
            (else (scan (mcdr vars) (mcdr vals)))))
    (scan (frame-variables frame) (frame-values frame))))

;; ; (while <predicate> <body>)
;; ;; pick while
;; (define (while? exp) (tagged-list? exp 'while))
;; ;; predicate is cadr (second)
;; (define (while-predicate exp) (cadr exp))
;; ;; body is third arg
;; (define (while-body exp) (cddr exp))

;; TODO bunu anlamak lasm
;; (define (make-procedure-definition name parameters body)
;;   (cons 'define  (cons (cons name parameters) body)))
;; (define (make-procedure-application procedure arguments)
;;   (cons procedure arguments))
;;
;; (define (while->combination exp)
;;   (define (while->procedure-def procedure-name)
;;     (make-procedure-definition
;;      procedure-name
;;      '()
;;      (make-if
;;       (while-predicate exp)
;;       (sequence->exp
;;        (append (while-body exp)
;;                (make-procedure-application procedure-name '()))))))
;;   ; wrap the procedure definition in a lambda to contrain its scope
;;   (make-procedure-application
;;    (make-lambda
;;     '()
;;     (list (while->procedure-def 'while-procedure)
;;           (make-procedure-application 'while-procedure '())))
;;    '()))
;;

; the whole thing will look like this:
;; ((lambda ()
;;     (define (while-procedure)
;;         (if (< i 100)
;;             (begin
;;                 (display i)
;;                 (set! (+ i 1)
;;                 (while-procedure)))))
;;     (while-procedure)))

; 4.1.4 Running the Evaluator as a Program

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'display display)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list 'eq? eq?)
        (list '= =)
        (list 'format format)
        (list 'add2 +)
        (list 'lt <)))


(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply
   (primitive-implementation proc) args))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))
(define the-global-environment (setup-environment))

(define input-prompt ";;; alicheme ")
(define output-prompt ";;; alicheme ")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (sicp-eval input the-global-environment)))
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

the-global-environment
(driver-loop)
