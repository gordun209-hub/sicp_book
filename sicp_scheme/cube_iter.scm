#lang sicp

(define (cube-iter guess x)
  (if (good-enough? guess x)
      guess
      (cube-iter (improve guess x) x)))

(define (cub x)
  (* x x x))

(define (good-enough? guess x)
  (< (abs (- (cub guess) x))  .001))

(define (improve guess x)
  (/
   (+ (/ x (* guess guess)) (* 2 guess))
   3))


(define (cube x) (cube-iter 1.0 x))

;;(cube 64) ; => 4




(define (sqrt-1 x)
  ;; these are inline definitions that does not visible outside
  (define (square x) (* x x))

  (define (good-enough? guess x)
    (< (abs (- (square guess) x )) 0.001))

  (define (improve guess x) (/ x guess))

  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))

  (sqrt-iter 1.0 x))

;; using lexical scoping for x e erismek
(define (sqrt x)

  (define (average x y) (/ (+ x y) 2))

  (define (square x) (* x x))

  (define (good-enough? guess)
    (< (abs (- (square guess) x )) 0.001))

  (define (improve guess)
    (average guess (/ x guess)))

  (define (sqrt-iter guess)
    (if (good-enough? guess) guess (sqrt-iter (improve guess))))

  (sqrt-iter 1.0))


;; Linear recursion
(define factorial
  (lambda (x)
    (if (= x 1)
        1
        (* x (factorial (- x 1))))))

;; Iterative
(define (fact-iterr n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))


(define (factorial-with-iter n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))
