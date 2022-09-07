#lang sicp
;; rectangular form => real and imaginary part
;; polar form => magnitude and angle

;; Real-part(z1 + z2) = Real-part(z1) + Real-part(z2)
;; Imaginary-part(z1 + z2) = Imaginary-part(z1) + Imaginary-part(z2)

;; when multiplyng complex nunmbers, it is more natural to think
;; in terms of representing a complex number in polar form, as a magnitude
;;and an angle (r and A)


;; Magnitude(z1 * z2) = Magnitude(z1) * Magnitude(z2)
;; Angle(z1 * z2) = Angle(z1) + Angle(z2)


;; selectors, real-part imag-part magnitude angle
;; make-from-real-imag retunrs complex number with specified real and imag parts
;; make-from-mag-ang returns a complex number with specified mag and ang

;; (make-from-real-imag (real-part z) (imag-part z))

;; and

(define (square x) (* x x))
;; (make-from-mag-ang (magnitude z) (angle z))

(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z) (eq? (type-tag z) 'polar))

;; we can represent a complex number in "rectangular form" as a pair
;; (real part, imaginary part) or in "polar form" as a pair (magnitude angle)
;;Imagine  that there are two programmers ben Bitdiddle and alyssa p hacker who
;; are independently designin representations for complex numbers system
;; ben chooses rectangular form and alyssa chooses polar form
;; BEN's representation -------------------------------------

(define (real-part-rectangular  z) (car z))
(define (imag-part-rectangular z) (cdr z))

(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))

(define (angle-rectangular z)
  (atan (imag-part-rectangular z) (real-part-rectangular z)))

(define (make-from-real-imag-rectangular x y)
  (attach-tag
   'rectangular
   (cons x y)))

(define (make-from-mag-ang-rectangular r a)
  (attach-tag
   'rectangular
   (cons (* r (cos a)) (* r (sin a)))))

;;----------------------------------------------------------------------

;; Alyssa's representation
(define (real-part-polar z) (* (magnitude-polar z) (cons (angle-polar z))))

(define (imag-part-polar z) (* (magnitude-polar z) (sin (angle-polar z))))
(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))

(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
              (cons (sqrt (+ (square x) (square y)))
                    (atan y x))))

(define (make-from-mag-and-polar r a) (attach-tag 'polar (cons r a)))

(define (real-part z)
  (cond ((rectangular? z)
         (real-part-rectangular (contents z)))
        ((polar? z)
         (real-part-polar (contents z)))
        (else (error "Unknown type: REAL-PART" z))))

(define (magnitude z)
  (cond ((rectangular? z)
         (magnitude-rectangular (contents z)))
        ((polar? z)
         (magnitude-polar (contents z)))
        (else (error "Unknown type: MAGNITUDE" z))))

(define (angle z)
  (cond ((rectangular? z)
         (angle-rectangular (contents z)))
        ((polar? z)
         (angle-polar (contents z)))
        (else (error "Unknown type: ANGLE" z))))

(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))

(define (make-from-mag-ang r a)
  (make-from-mag-and-polar r a))

(define (add-complex z1 z2)
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
                     (- (angle z1) (angle z2))))

;; Tagged data
