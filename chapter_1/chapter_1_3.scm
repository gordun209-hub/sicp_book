#lang sicp
; Formulating abstractions with higher order procedures
; sum ints a trough b
;; (define (sum-integers a b)
;;   (if (> a b)
;;       0
;;       (+ a (sum-integers (+ a 1) b))))

;; (define (sum-cubes a b)
;;   (if (> a b)
;;       0
;;       (+ (cube a)
;;          (sum-cubes (+ a 1 )b ))))

;; (define (cube a) (* a a a))

; 1\1 * 3 + 1/5*7 + 1/9 * 3 + ... -> converges to pi/8 slowly

;; (define (pi-sum a b)
;;   (if (> a b)
;;       0
;;       (+ (/ 1.0 (* a (+ a 2)))
;;          (pi-sum (+ 4 a) b))))

; we re filling in slots in the same template:

; (define (<name>) a b)
;    (if (> a b)
;         0
;         (+ (<term> a) -> yapilan islem <term>
;         (<name> (<next> a) b)))) -> natural recursion <name> with next

; common name of this pattern is "sigma notation"

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n ) (+ n 1))

(define (sum-cubes a b)
  (sum cube a inc b))


(define (identity  x) x)
(define (sum-integers a b)
  (sum (identity a inc b)))

;; (define (pi-sum a b)
;;   (define (pi-term x)
;;     (/ 1.0 (* x (+ x 2))))
;;   (define (pi-next x)
;;     (+ x 4))
;;   (sum pi-term a pi-next b))
;;
;; (define (integral f a b dx)
;;   (define (add-dx x)
;;     (+ x dx))
;;   (* (sum f (+ a (/ dx 2.0)) add-dx b)
;;      dx))
;;
(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

(define (integral f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
     dx))

; (define (plus4 x) (+ x 4)) === (define plus4 (lambda (x) (+ x 4)))
(define (square x) (* x x))
(define (f x y)
  (define (f-helper a b)
    (+ (* x (square a))
       (* y b)
       (* a b)))
  (f-helper (+ 1 (* x y))
            (- 1 y)))

(define (g x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

;; (let ((⟨var1⟩ ⟨exp1⟩)
;; (⟨var2⟩ ⟨exp2⟩)
;; . . .
;; (⟨varn⟩ ⟨expn⟩))
;; ⟨body⟩)

;; which can be thought of as saying
;; let ⟨var1⟩ have the value ⟨exp1⟩ and
;; ⟨var2⟩ have the value ⟨exp2⟩ and
;; . . .
;; ⟨varn⟩ have the value ⟨expn⟩
;; in ⟨body⟩

(define (k g) (g 2))

; then we have
; (k square) = 4

; (k (lambda (z) (* z (+ z 1))))
; check close-enough?
(define (close-enough? x y) (< (abs (- x y)) 0.001))
; avg
(define (average x y) (/  (+ x y) 2))
; search
; take 2 points one positive and one negative
(define (search f neg-point pos-point)
  ; midpoint is average of negative and positive points
  (let ((midpoint (average neg-point pos-point)))
    ; check error
    (if (close-enough? neg-point pos-point)
        ; if close-enough? return midpoint as answer
        midpoint
        ;else let test value f midpoint ?
        (let ((test-value (f midpoint)))
          ; pozitifse recur with neg point and midpoint
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ; opisite
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

(half-interval-method sin 2.0 4.0)

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; exercise 1.12

(define (pascal row col)
  (cond ((= row 1) 1)
        ((or (= col 1) (= col row)) 1)
        (else (+ (pascal (- row 1) (- col 1))
                 (pascal (- row 1 )col)))))

(define (pascalr r c)
  (if (or (= c 1) (= c r))
      1
      (+ (pascal (- r 1) (- c 1)) (pascal (- r 1) c))))
;         r  c
; (pascal 4 2)
; (+ (pascal 3 1) (pascal (3 2)))
; (+ )

;; (pascalr 1 1)
;; (pascalr 2 2)
;; (pascalr 3 2)
;; (pascalr 4 2)
;; (pascalr 5 2)
;; (pascalr 5 3)
;; (pascalr 3 3)
(pascal 4 2)
; (+ pascal(3 1) (pascal (3 2)))
; (+ 1 (+ (pascal 2 1) (pascal 2 2)))
; (+ 1 (+ 1 (+ 1))) = 3
(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))


(sine 12.15)
(define (sine-count-step angle step)
  (if (not (> (abs angle) 0.1))
      step
      (sine-count-step (/ angle 3.0) (+ step 1))))

(display "| ") (display "a         ") (display " | ") (display "number of steps              ") (display " |") (newline)
(display "| ") (display "----------") (display " | ") (display "-----------------------------") (display " |") (newline)
(display "| ") (display       12 ) (display " | ") (display (sine-count-step       12 1)) (display " |") (newline)
(display "| ") (display      120 ) (display " | ") (display (sine-count-step      120 1)) (display " |") (newline)
(display "| ") (display     1200 ) (display " | ") (display (sine-count-step     1200 1)) (display " |") (newline)
(display "| ") (display    12000 ) (display " | ") (display (sine-count-step    12000 1)) (display " |") (newline)
(display "| ") (display   120000 ) (display " | ") (display (sine-count-step   120000 1)) (display " |") (newline)
(display "| ") (display  1200000 ) (display " | ") (display (sine-count-step  1200000 1)) (display " |") (newline)
(display "| ") (display 12000000 ) (display " | ") (display (sine-count-step 12000000 1)) (display " |") (newline)


(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n)
         n)
        ((divides? test-divisor n)
         test-divisor)
        (else (find-divisor
               n
               (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

