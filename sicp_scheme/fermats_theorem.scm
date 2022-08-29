#lang sicp
(define (square x) (* x x))


(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n ) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (expmod base exp m)
  (cond ((= exp 0 ) 1)
        ((even? exp)
         ;; burda ek olarak remainder var?
         ;; gne de TODO
         ;; Successive squaring??
         (remainder (square (expmod base (/ exp 2 )m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor)n )n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n )(fast-prime? n (- times 1)))
        (else false)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display "*** ")
  (display elapsed-time))



(define (search-for-primes lower upper)
  (define (iter n)
    ;; if n <= upper call timed-prime-test else iter with + n 2
    (cond ((<= n upper) (timed-prime-test n) (iter (+ n 2)))))
  ;; make sure its odd?
  (iter (if (odd? lower) lower (+ lower 1))))
(search-for-primes 1000 2000)
