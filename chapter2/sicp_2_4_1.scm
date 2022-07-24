#lang sicp
;; Helpers

(define square
  (lambda (x) (* x x)))

(define (add-complex z1 z2)
  ; takes two complex numbers and adds
  ;  (real-part z1 + real-part z2) , (imag-part z1 + imag-part z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

;; First Representation

(define (real-part z ) (car z ))

(define (imag-part z ) (cdr z))

(define (magnitude z)
  ; magnitude of a complex number equals
  ; square of real-part z plus, square of imag-part z
  (sqrt (+ (square (real-part z))
           (square (imag-part z)))))

(define (angle z)
  ; angle of complex number equals to arctan of ima-part z and real-part z
  (atan (imag-part z) (real-part z)))
; simply cons
(define (make-from-real-imag x y) (cons x y))
; make imaginary number with magnitude and angle
; multiply magnitude with cos(a) and multply magnitude with sin a then cons em
(define (make-from-mag-ang r a)
  (cons (* r (cos a)) (* r (sin a))))



; Second representation
(define (real-part2 z) (* magnitude z) (cos (angle z)))

(define (imag-part2 z) (* (magnitude z) (sin (angle z))))

(define (magnitude2 z) (car z))

(define (angle2 z) (cdr z))

(define (make-from-real-imag2 x y)
  (cons (sqrt (+ (square x) (square y)))))

(define (make-from-mag-ang2 r a) (cons r a))

















