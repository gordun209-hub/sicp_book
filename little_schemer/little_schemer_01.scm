#lang scheme
(define atom? (lambda (x) (and (not (pair? x)) (not (null? x)))))
(atom? (quote ()))

; () is not a atom
(car '(z b c)) ; first atom in list

; law of the car
; the primitive car is defined only for non-empty lists.
(car (car '(((hotdogs)) (and)))) ; (hotdogs)

(cdr '(a b c)) ; (b c)

; The law of cdr
;; the primitive cdr is defined only for non empty lists.
;; the cdr of any non-empty list is always another list

(cdr (cdr '((b) (x y) ((c))))) ;; (((c)))
;; cons adds atom to the list
(cons 'butter '(and jelly))

(cons '((help) this) '(is very ((hard) to learn)))

(cons 'a '()) ; (a)
