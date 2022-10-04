#lang sicp

;; continued fraction
;; A)
(define (cont-frac n d k)
  (let ((N (n k))
        (D (d k)))
    (cond ((= k 0) (/ N D))
          (else
           (/ N (+ D (cont-frac n d (- k 1))))))))

(/ 1(cont-frac (lambda (i) 1.0)
               (lambda (i) 1.0)
               10))


(define (cont-frac-iter n d k)
  (define (loop result term)
    (if (= term 0)
        result
        (loop (/ (n term)
                 (+ (d term) result))
              (- term 1))))

  (loop 0 k))

