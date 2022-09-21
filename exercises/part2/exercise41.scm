#lang sicp

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (unique-tuples n k)
  (cond ((< n k) nil)
        ((= k 0) (list nil))
        (else (append (unique-tuples (- n 1) k)
                      (map (lambda (tuple) (cons n tuple))
                           (unique-tuples (- n 1) (- k 1)))))))

; application to the case of 3-tuples
(define (triples-of-sum s n)
  (filter (lambda (seq) (= (accumulate + 0 seq) s))
          (unique-tuples n 3)))
(triples-of-sum 20 30)

