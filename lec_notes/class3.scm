#lang sicp
;; 1. Special Forms
;; (a) begin (begn exps)
;; Evaluate each expression in order and return the value of the last expression.

(define abs
  (lambda (n)
    (cond
      ((< n 0) (- n (+ n n)))
      ((> n 0) n)
      ((= n 0) 0))))
;; (abs 5)
;;
;; (abs -5)



(define sum-to-n
  (lambda (n)
    (cond
      ((= n 0) 0)
      (else
       (+ n (sum-to-n (- n 1)))))))

(sum-to-n 3)

;; (define infinity-procedure
;;   (lambda (x)
;;     (cond
;;       ((+ x (infinity-procedure x))))))
;;
;; (infinity-procedure 2)


; (gcd 206 40)
; 2
(define gcd
  (lambda (a b)
    (cond
      ((= b 0) 0)
      ((= (remainder a b) 0) b)
      (else
       (gcd b (remainder a b))))))

(gcd 30 12)

(gcd 206 40)





(define (foo x)
  (+ x 3))

foo ; procedure

(foo 5) ; 8

(define bar 5)

(define (baz) 5)
bar
baz

;; (bar)  Error
(baz) ; 5


(let ((a 3)
      (b 5))
  (+ a b))  ; 8

(let ((+ *)
      (* +))
  (+ 3 (* 4 5))) ; 27

(define m 3)
(let ((m (+ m 1)))
  (+ m 1)) ; 5

(define n 4)

(let ((n 12)
      (o (+ n 2))) ; 6
  (* n o)) ; 6 * 12


