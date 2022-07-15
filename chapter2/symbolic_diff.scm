#lang sicp
; addend first term augend second term

; (variable? e) Is e variable?

; (same-variable? v1 v2) Are v1 and v2 the same variable?

; (sum? e) Is e a sum?

;(addend e) Addend of the sum e.

;(augend e) Augend of the sum e.

;(make-sum a1 a2) Construct the sum of a1 and a2.

;(product? e) Is e a product?

;(multipliere ) Multiplier of the product e.

;(multiplicand e) Multiplicand of the product e.

;(make-product m1 m2) Construct the product of m1 and m2.

(define (=number? exp num) (and (number? exp) (= exp num)))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

; Sums and products are constructed as lists:
;; (define (make-sum a1 a2) (list '+ a1 a2))

(define (make-sum a1 a2)
  (cond
    ((=number? a1 0) a2)
    ((=number? a2 0) a1)
    ((and (number? a1) (number? a2))
     (+ a1 a2))
    (else (list '+ a1 a2))))

;; (define (make-product m1 m2) (list '* m1 m2))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))
; A sum is  a list whose first element is the symbol +:

(define (sum? x) (and (pair? x) (eq? (car x) '+)))

;Addend is the second item of the sum list:

(define (addend s) (cadr s))

;The augend is the third item of the sum list:

(define (augend s) (caddr s))

; A product is a list whose first element is the symbol *:

(define (product? x) (and (pair? x) (eq? (car x) '*)))

; The multiplier is the second item of the product list:
(define (multiplier p) (cadr p))

;The multiplicand is the third item of the product list:
(define (multiplicand p) (caddr p))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "unknown expression type:DERIV" exp))))


(deriv '(+ x 3) 'x) ; => (+ 1 0)

(deriv '(* x y) 'x) ; => (+ (* x 0) (* 1 y))

(deriv '(* (* x y) (+ x 3)) 'x)
; (* xy x+3 ) = (x2y + 3xy)
;; (+ (* (* x y) (+ 1 0))
;;    (* (+ (* x 0) (* 1 y))
;;       (+ x 3)))

;; (define (element-of-set? x set)
;;   (cond ((null? set) false)
;;         ((equal? x (car set)) true)
;;         (else (element-of-set? x (cdr set)))))

(define (adjoint-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

;; (define (intersection-set set1 set2)
;;   (cond ((or (null? set1) (null? set2)) '())
;;         ((element-of-set? (car set1) set2)
;;          (cons (car set1) (intersection-set (cdr set1) set2)))
;;         (else (intersection-set (cdr set1) set2))))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set (cdr set1)
                                          (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))


