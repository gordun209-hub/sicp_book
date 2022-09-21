#lang sicp

(define (square x) (* x x))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ;; n even ise squaresini al ve 2 ye bol n i
        ((even? n) (square (fast-expt b (/ n 2))))
        ;; deilse b ile carp ve n i 1 eksilt
        (else (* b (fast-expt b  (- n 1))))))

(define (fast-expt-iterative b n)
  (define (expt-iter b n a)
    (cond ((= n 0) a)
          ((even? n) (expt-iter (square b) (/ n 2) a))
          (else
           (expt-iter b (- n 1) (* b a)))))
  (expt-iter b n 1))

(fast-expt-iterative 2 3)

;; (expt-iter 2 4 2)
;; (expt-iter 2 2 4)
;; (expt-iter 2 1 16)
;; (expt-iter]

