#lang racket/base
(require racket/trace)
;; Helpers
;; check if equal number
(define (=number? exp num) (and (number? exp) (= exp num)))
;; check if variable
(define (variable? x) (symbol? x))
;; check if same variable
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
;; check if sum
(define (sum? x) (and (pair? x) (eq? (car x) '+)))
;; if product
(define (product? x) (and (pair? x) (eq? (car x) '*)))
;; exponentiation
(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '**)))
;; Constructors

;; Construct sum
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0 ) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))
;; Construct product
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))
;; construct exponentiation
(define (make-exponentiation base exp)
  (cond ((=number? base 1) 1)
        ((=number? exp 1) base)
        ((=number? exp 0) 1)
        (else
         (list '** base exp))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
;; Selectors
;; select addend
(define (addend s)
  (cadr s))
;; select augend
(define (augend s)
  (accumulate make-sum 0 (cddr s)))

(define (multiplicand p)
  (accumulate make-product 1 (cddr  p)))
(define (augendd s)
  (caddr s))
;; select multiplier
(define (multiplier p)
  (cadr p))
;; select multiplicand
(define (multiplicandd p)
  (caddr p))
;; exponent
(define (exponent exp)
  (caddr exp))
;; base
(define (base exp)
  (cadr exp))
;-------------------------------------------------------------------

;; main func
(define (deriv expr var)
  (cond ((number? expr) 0)
        ((variable? expr) (if (same-variable? expr var) 1 0))
        ((sum? expr) (make-sum (deriv (addend expr) var)
                               (deriv (augend expr) var)))
        ((product? expr) (let ((m1 (multiplier expr))
                               (m2 (multiplicand expr)))

                           ;; Abstract dusun aga :D
                           (make-sum (make-product (deriv m1 var) m2)
                                     (make-product m1 (deriv m2 var)))))
        ((and (exponentiation? expr) (=number? (deriv (exponent expr) var) 0))
         (let ((b (base expr)) (e (exponent expr)))
           (make-product (deriv b var)
                         (make-product e (make-exponentiation b (make-sum e -1))))))
        (else (list 'deriv expr var))))

(augend '(* x y (+ x 3)))
(augendd '(* x y (+ x 3)))

;; Tam anlamadm
