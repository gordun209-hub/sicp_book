#lang sicp

;; dotted tail

(define (f x y . z) z)

(f 1 2 3 4 5 6) ;; (3 4 5 6)


(define (g . w) w)

(g 1 2 3 4 5 6)

(define (same-parity first . rest)
  (define (same-parity-iter source dist remainder-val)
    (if (null? source)
        dist
        (same-parity-iter (cdr source)
                          (if (= (remainder (car source) 2) remainder-val)
                              (append dist (list (car source)))
                              dist)
                          remainder-val)))

  (same-parity-iter rest (list first) (remainder first 2)))
