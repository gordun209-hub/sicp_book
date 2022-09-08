#lang sicp

;; viewing systems as collections of objects with local state is a
;; powerful technique for maintaining a modular design. as a simple example
;; consider the design of a procedure rand that whenever its called,
;; returns an integer choosen at random
(define (rand-update x)
  (+ x 1))

(define rand (let ((x 1))
               (lambda ()
                 (set! x (rand-update x))
                 x)))

;; assume we have rand-update

;; we can approximate pi using fact that 6/pi^2 is the probability that
;; two integers  choosen at random will have no factors in common


;; we can approximate pi using the fact that 6/pi^2 is the probability that
;; two integers chosen at random will have no factors in common; that is, that
;; their greatest common divisior will be 1
;; IMP fraction of times that the test is passed gives us our estimate of 6/pi^2
;; bole yapnca modular oluyo

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1)
                 trials-passed))))
  (iter trials 0))

(estimate-pi 100)

;; Exercise 3.5 monte carlo integration

;; Consider computing the area of a region of a space described by a predicate
;; P(x,y) that is true for points (x,y)
;;and false for points not in the region
;; for example region contained within a circle of radius 3 centered at (5,7)
;; described by the predicate that test whether (x-5)^2 + (y-7)^2 <= 3^2
;; to estimate the are of the region, begin by choosing a rectangle that
;; contains the region for example, a rectangle with diagonally opposite
;; corners at (2,4) and (8,10) contains the circle  above
;; we can estimate the integral by picking, at random, points P(x,y) for each
;; point to determine whether the point lies in the region.

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (P x y)
  (< (+ (expt (- x 5) 2)
        (expt (- y 7) 2))
     (expt 3 2)))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (define (experiment)
    (P (random-in-range x1 x2)
       (random-in-range y1 y2)))
  (monte-carlo trials experiment))


