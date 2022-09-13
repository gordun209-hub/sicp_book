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
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))
;; COnditionals
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

;; Sequences 
(define (eval-sequence exps env)
  (cond ((last-exp exps)
         (eval (first-exp exps) env))
        (else 
          (eval (first-exp exps) env)
          (eval-sequence (rest-exps exps) env))))
;; Assignment and definitions 

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

(define (eval exp env)
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
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type: EVAL" exp))))



(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error "Unknown procedure type: APPLY"))))


