#lang sicp

(define (sicp-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((quoted? exp) (text-of-quotation exp))
        ((application? exp)
         (sicp-apply (sicp-eval (operator exp) env)
                     (list-of-values (operands exp) env)))
        ))

(define apply-in-underlying-scheme apply)


(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))
(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))
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
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (last-exp? seq) (null? (cdr seq)))
(define (procedure-environment p) (cadddr p))
(define (eval-sequence exps env)
  ;; last exp ise eval bildigin
  (cond ((last-exp? exps)
         (sicp-eval (first-exp exps) env))
        (else
         ;; daha warsa ilkini evalle ve recurla rest
         (sicp-eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) env))))
(define (primitive-implementation proc) (cadr proc))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))
(define (announce-output string)
  (newline) (display string) (newline))
(define output-prompt "<-<")
(define input-prompt ">->")
(define the-empty-environment '())


(define (frame-variables frame) (car frame))
;; cdr is the values
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  ;; cons var and val (mutate)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (first-frame env) (car env))
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

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list '+ +)
        (list '- -) (list '/ /)
        (list 'eq? eq?)
        (list 'null? null?)))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (make-frame variables values)
  (cons variables values))
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "too many args supplied" vars vals)
          (error "too few args supplied" vars vals))))
(define the-global-environment (setup-environment))

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (sicp-eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))
the-global-environment
(driver-loop)
