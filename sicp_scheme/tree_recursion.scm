#lang racket/base
(require racket/trace)
(define (fib-slow n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib-slow (- n 1))
                 (fib-slow(- n 2))))))

(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))
(trace fib-iter)

(fib 8)
