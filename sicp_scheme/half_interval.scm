#lang sicp

(define (average x y) (/ (+ x y) 2))
(define (close-enough? x y) (< (abs (- x y)) 0.001))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))


;; (half-interval-method sin 2.0 4.0)
;;
;; ;; another example for x^3 - 2x -3 = 0 between 1 and 2
;;
;; (half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3))
;;                       1.0
;;                       2.0)

;;((lambda (x) (- (* x x x) (* 2 x) 3)) 1.89306640625)

;; Finding fixed point functions

;; A number x is called a fixed point of a function f if x satisfies
;; the equation f(x) = x for some functions f we can locate a fixed
;; point beginning with an initial guess and applying f repeatedly



; f(x), f(f(x)), f(f(f(x))), f(f(f(f(x)))) ...



(define tolerance 0.00001)

(define (fixed-point f first-guess)
  ;; check close enuf
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  ;; try?
  (define (try guess)
    ;; next = f(x) applied value

    (let ((next (f guess)))
      (display "guess =")
      (display guess)
      (newline)
      (display "next guess =")
      (display next)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;; (fixed-point cos 1.0)

;; infinite loop
;; consider initial guess is y1, next is y2 = x/y1 and next is
;; y3 = x/y2 = x/(x/y1) = y1
(define (sqrt-broke x)
  (fixed-point (lambda (y) (/ x y)) 1.0))

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y) 1.0))))
;; Exercise 1.35
;; (fixed-point (lambda (x) (+ 1 (/ 1 x)) )1.0)

;; Exercise 1.36
;; (fixed-point (lambda (x) (/ (log 1000) (log x)))10)
;; ((lambda (x) (/ (log 1000) (log x)) 4.5555))



;; Exercise 1.37

(define (cont-frac n d k)
  ;; term = k result = 0
  ;; k is like tekrar etme sayisi
  (define (loop result term)
    (if (= term 0)
        result
        (loop (/ (n term)
                 (+ (d term) result))
              (- term 1))))

  (loop 0 k))

(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= 0 i)
        result
        (iter (sub1 i)
              (/ (n i) 
                 (+ result (d i))))))
  (iter (sub1 k) (/ (n k) (d k))))
;; (cont-frac (lambda (i) 1.0)
;;            (lambda (i) 1.0)
;;            1)
;; TODO buraya cok cok cok cok x999 ii calis
(define (cont-frac-r n d k)
  (cond ((= k 0) 0)
        (else (/ (n k) (+ (d k) (cont-frac-r n d (- k 1)))))))

(define (e-euler k)
  (+ 2.0 (cont-frac (lambda (i) 1)
                    (lambda (i)
                      (if (= (remainder i 3) 2)
                          (/ (+ i 1) 1.5)
                          1))
                    k)))

;; Exc 1.38
;; F3N4 TODO
(define (cont-frac-recur n d k)
  (define (recur i)
    (if (= k i)
        ;; base case when we reach
        (/ (n i) (d i))
        ;; N1 / D1 + rest
        (/ (n i) (+ (d i) (recur (+ 1 i))))))
  (recur 1))

(define (sub1 x) (- x 1))

;; (define (cont-frac-iter n d k)
;;   (define (iter i result)
;;     (if (= 0 i)
;;         result
;;         (iter (sub1 i) (/ (n i) (+ result (d i))))))
;;
;;   (iter (sub1 k) (/ (n k) (d k))))







(define (tan-cf x k)
  (cont-frac-iter
   (lambda (i) (if (= i 1) x (* x x -1)))
   (lambda (i) (- (* 2.0 i) 1))
   k))


; check result
(define x 1)
(display (tan-cf x 8)) (newline)
(display (tan x)) (newline)















