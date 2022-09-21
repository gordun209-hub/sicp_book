#lang sicp

;; Ben bitdidle has invented a test to deremine whether the interpreter
;; has is faced with is using applicative order or normal order
;; He defines the following two procedures

(define (p) (p))

(define (test x y)
  (if (= x 0) 0 y))

;; then evaluates

(test 0 (p))

;; What behavior will ben observe with an interpreter that uses 
;; applicative order? and normal-order

;; Applicative 

;; (test 0 (p))
;; evaluates (p)
;; (test 0 (p))
;; evaluates p 
;; and so on 

;; Normal order 

;; (test 0 (p))
;; (if (= 0 0) 0 (p))
;; 0

