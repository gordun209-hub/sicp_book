#lang sicp
(define (tag-check e sym)
  (and (pair? e) (eq? (car e) sym)))
(define (sum? e) (tag-check e 'plus*))

(define (eval exp)
  (cond 
    ((number? exp) exp)
    ((sum? exp) (eval-sum exp))
    (else 
      (error "unknown expression " exp))))

(define (eval-sum exp)
  (+ (eval (cadr exp)) (eval (caddr exp))))

(define thirty-five '(plus* 24 (plus* 5 (plus* 2 3))))

(eval thirty-five) 

;; (eval '(plus* 24 (plus 5 4)))
;; (eval-sum '(plus* 24 (plus* 5 4)))
;; (+ (eval 24) (eval (plus* 5 4)))
;; (+ (24 (eval-sum (plus* 5 4))))
;; (+ (24 (+ (eval 5) (eval 4))))
;; (+ 24 (+ 5 4))
(eval 4)
