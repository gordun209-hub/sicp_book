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

; The Laws of cons
; The primitive cons takes two arguments.
; the second argument to cons must be a list
; the result is a list

; The law of Null?
; the primitive null? is defined only for lists

; the law of Eq?
; the primitive eq? takes two arguments. each must be a non numeric atom.
(define l '(Marry had a little lamb chop))
(define a 'Marry)
(eq? (car l) a) ; #t

(define lat?
  (lambda (l)
    (cond
      [(null? l) #t] ; check if null
      ; if not null take first element and check if atom if not
      ; call same function with cdr l which drops first and takes rest
      [(atom? (car l)) (lat? (cdr l))]
      [else #f])))

(lat? '(bacon and eggs))
(define member?
  (lambda (a lat)
    (cond
      ; check if nulll if null return false
      [(null? lat) #f]
      ; check if either first atom of lat and given a is equal
      ; of not call member function with same a but cdr lat
      [else (or (eq? (car lat) a) (member? a (cdr lat)))])))
(member? 'meat '(mashed potatoes and meat gravy))

; The first commandment
; Always ask null? as the first question in expressing any function
;; wrong implementation
;; (define rember
;;   (lambda (a lat)
;;     (cond
;;       [(null? lat) (quote ())]
;;       [else
;;        (cond
;;          [(eq? (car lat) a) (cdr lat)]
;;          [else (rember a (cdr lat))])])))


(define rember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((eq? (car lat) a) (cdr lat))
              (else (cons (car lat)
                          (rember a
                                  (cdr lat)))))))))

