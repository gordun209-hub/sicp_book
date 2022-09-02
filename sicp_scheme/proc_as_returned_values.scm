#lang sicp

(define (average x y) (/ (+ x y) 2))

(define (average-damp f)
  ;; get average of x and f applied x
  (lambda (x) (average x (f x))))

(define (square x) (* x x))
((average-damp square) 2)
;; => ( / 2 (+ 2 4) )
(define  tolerance 0.0001)



;; --------------------------------------------------------------
;; TODO ikisi arasindaki farki anla incele fln
(define (sqrtt x)
  (sqrt-iter 1.0 x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))
(define (good-enough? guess x)
  (< (abs (- (square guess) x )) 0.001))
(define (improve guess x)
  (average guess (/ x guess)))

;; ----------------------------------------------------------------

(define (fixed-point f first-guess)
  ;; close-enough = good-enough
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  ;; apply function and see if close enough
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
;; anlamadm mq anasini skmn
(define (sqrttt x) (fixed-point (average-damp (lambda (y) (/ x y)))
                                1.0))

; ------------------------------------------------------------

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y)))) 1.0))

;; Newtons method
(define dx 0.00001)
;; g fonksiyon x ise 5
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))


(define (cube x) (* x x x))
;; 3*5^2 = 25* 3 = 75
((deriv cube) 5)

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))
;; TODO tekrr
(define (sqrtttt x)
  (newtons-method
   (lambda (y) (- (square y) x)) 1.0))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt x)
  (fixed-point-of-transform
   (lambda (y) (/ x y)) average-damp 1.0))
