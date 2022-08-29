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

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(expmod 6 7 7)

(fermat-test 22)

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

