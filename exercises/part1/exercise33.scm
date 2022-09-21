#lang sicp

(define (accumulate combiner null-value term a next b)
  (if (< b a)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a)  next b))))

(define (filter-acc combiner null-value term a next b pred)
  (cond ((< b a) null-value)
        ((not (pred (term a))) (filter-acc combiner null-value term (next a) next b pred))
        (else
         (combiner (term a) (filter-acc combiner null-value term (next a) next b pred)))))

(define (filtered-accumulate combiner null-value term a next b filter)
  (if (> a b) null-value
      (if (filter a)
          (combiner (term a) (filtered-accumulate combiner null-value term (next a) next b filter))
          (combiner null-value (filtered-accumulate combiner null-value term (next a) next b filter)))))

(define evens-between-one-and-ten (filter-acc cons nil + 1 inc 10 even?))



(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (prime? n)
  ; Perform the test how many times?
  ; Use 100 as an arbitrary value.
  (fast-prime? n 100))



(define (sum-of-prime-squares a b) (filtered-accumulate + 0 square a inc b prime?)) 

(sum-of-prime-squares 1 10)

