#lang sicp


(define (cont-frac n d k)
  (let ((N (n k))
        (D (d k)))
    (cond ((= k 0) (/ N D))
          (else
           (/ N (+ D (cont-frac n d (- k 1))))))))



(define (cont-frac-iter n d k)
  (define (loop result term)
    (if (= term 0)
        result
        (loop (/ (n term)
                 (+ (d term) result))
              (- term 1))))
  (loop 0 k))

(define (square x) (* x x))

(define (tan-cf x k)
  (cont-frac (lambda (i)
               (if (= i 1) x (- (* x x))))
             (lambda (i)
               (- (* i 2) 1))
             k))
(define (tan-cff x k)
  (let ((a (- (* x x))))
    (cont-frac (lambda (i) (if (= i 1) x a))
               (lambda (i) (- (* i 2) 1))
               k)))
