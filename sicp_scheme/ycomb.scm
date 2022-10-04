#lang sicp
;; bune amk
((lambda (n)
   ((lambda (fact) (fact fact n))
    (lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1)))))))
 10


 (lambda (n)
   ((lambda (fib)
      (fib fib n))
    (lambda (f n)
      (cond ((= n 0) 0)
            ((= n 1) 1)
            (else (+ (f f (- n 2)) (f f (- n 1)))))))))

;; b. filling the box as sequence:

(define (f x)
  ((lambda (even? odd?) (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? )))))


;non-recursive factorial function
(define fact-once
  (lambda (f)
    (lambda (n)
      (if (= n 0)
          1
          (* n (f (- n 1)))))))

;y-combinator
(define Y
  (lambda (f)
    ((lambda (x) (x x))
     (lambda (x) (f (lambda (y) ((x x) y)))))))

(define factorial (Y fact-once))
(factorial 20)  ;=2432902008176640000
