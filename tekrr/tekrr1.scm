#lang sicp

;Helpers

(define square
  (lambda (x)
    (* x x)))
; with define-syntax

(define (fdefine x y)
  (define (f-helper a b)
    (+ (* x (square a))
       (* y b)
       (* a b)))
  (f-helper (+ (* x y))
            (- 1 y)))

; with lambda

(define (flambda x y)
  ((lambda (a b)
     (+ (* x (square a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))

; with let (best)

(define (flet x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

(define double
  (lambda (f)
    (lambda (x)
      (f (f x)))))


(define inc
  (lambda (x)
    (+ x 1)))

;; ((double inc) 5)
;; (((double (double double)) inc) 5)

(define compose
  (lambda (p1 p2)
    (lambda (x)
      (p1 (p2 x)))))

;; ((compose square inc) 6)
; ben ytaptm bunu fena mk
(define repeated
  (lambda (p repat-time)
    (lambda (n)
      (cond
        ((= repat-time 0) n)
        (else
         (p ((repeated p (- repat-time 1))  n)))))))

;; ((repeated square 2) 5)
;   bunemk
(define (repeatedd f n)
  (define (iter t rf)
    (if (= t n) rf
        (iter (+ t 1) (compose f rf))))
  (iter 0 (lambda (x) x)))

; maeks sense
(define (repeateds f n)
  (if (= n 1)
      f
      (compose f (repeateds f (- n 1)))))

(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond
      ((eq? op 'real-part) x)
      ((eq? op 'imag-part) y)
      ((eq? op 'magnitude)
       (sqrt (+ (square x) (square y))))
      ((eq? op 'angle) (atan y x))
      (else
       (error "Unknown op -- MAKE-FROM-REAL-IMAG " op))))
  dispatch)


(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond
      ((eq? op 'real-part) (* r (cos a)))
      ((eq? op 'imag-part) (* r (sin a)))
      ((eq? op 'magnitude) r)
      ((eq? op 'angle) a)
      (else (error "Unknown op ---- make-from-mag-ang" op))))
  dispatch)

;; (( make-from-real-imag 5 2)'real-part)

(define (apply-generic op arg) (arg op))
(define five-and-two (make-from-real-imag 15 4))
;; (apply-generic 'real-part five-and-two) ; 15

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

;; (memq 'apple '(pear banana prune)) ; false
;;
;; (memq 'apple '(x (apple sauce) y apple pear)) ; (apple pear)
;;
;; (list 'a 'b 'c) ; (a b c)
;;
;; (list (list 'george)) ; ((george))
;;
;; (cdr '((x1 x2) (y1 y2))) ; ((y1 y2))
;;
;; (cadr '((x1 x2) (y1 y2))) ; (y1 y2)
;;
;; (cdr '(w e q e))
;;
;; (pair? (car '(a short list))) ; f
;;
;; (memq 'red '((red shoes) (blue socks))) ; false
;;
;; (memq 'red '(red shoes blue socks)) ; (red shoes blue socks)
;;
;;
;; (equal? '(this is a list) '(this is a list)) ;true
;;
;; (equal? '(this is a list) '(this (is a) list)) ; false

;; (define (equal? list1 list2)
;;   ; if both are not pair return (eq? list1 list2)
;;   (cond ((and (not (pair? list1)) (not (pair? list2)))
;;          (eq? list1 list2))
;;         ; if both are pair cons and with car of both and cdr of both else false
;;         ((and (pair? list1) (pair? list2))
;;          (and (equal? (car list1) (car list2)) (equal? (cdr list1) (cdr list2))))
;;         (else false)))


;Symbolic Differentiation
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))


(define (=number? exp num)
  (and (number? exp) (= exp num)))
;;(define (make-sum a1 a2) (list '+ a1 a2))
(define (make-sum a1 a2)
  (cond ((=number? a1 0 ) a2)
        ((=number? a2 0 ) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))
;;(define (make-product a1 a2) (list '* a1 a2))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s ) (caddr s))


(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))


(define (deriv exp var)
  (cond
    ((number? exp) 0)
    ((variable? exp)
     (if (same-variable? exp var) 1 0))
    ((sum? exp)
     (make-sum (deriv (addend exp) var)
               (deriv (augend exp) var)))
    ((product? exp)
     (make-sum
      (make-product (multiplier exp)
                    (deriv (multiplicand exp) var))
      (make-product (deriv (multiplier exp) var)
                    (multiplicand exp))))
    (else
     (error "Unknown exp type -- DERIV" exp))))
;; (augend '(+ x 3))
(deriv '(+ x 3) 'x);) (+ 1 0))


;; (sum? '(* (* x y) (+ x 3))) ;f
;; (product? '(* (* x y) (+ x 3))) ;t
; (make-product (multiplier (* (* x y) (+ x 3)))
;                (deriv (multiplicand (* (* x y) (+ x 3))) var))
;(multiplier '(* (* x y) (+ x 3))) ; (* x y)
;(multiplicand '(* (* x y) (+ x 3)))
; (list '*  (* x y) (deriv (+ x 3) x))
;(list '*  (* x y) (+ (deriv x x) (deriv x 3)))
;('* (* x y) (+ 1))


; EXERCISE 2.56
;;


(define (base exp)
  (cadr exp))

(define (exponent exp)
  (caddr exp))

(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '**)))
(define (make-exponentiation base exp)
  (cond ((=number? base 1) 1)
        ((=number? exp 1) base)
        ((=number? exp 0) 1)
        (else
         (list '** base exp))))

(define (deriv2 exp var)
  (cond
    ((number? exp) 0)
    ((variable? exp)
     (if (same-variable? exp var) 1 0))
    ((sum? exp)
     (make-sum (deriv (addend exp) var)
               (deriv (augend exp) var)))
    ((product? exp)
     (make-sum
      (make-product (multiplier exp)
                    (deriv (multiplicand exp) var))
      (make-product (deriv (multiplier exp) var)
                    (multiplicand exp))))
    ((exponentiation? exp)
     (make-product
      (make-product (exponent exp)
                    (make-exponentiation (base exp)
                                         (if (number? (exponent exp))
                                             (- (exponent exp) 1)
                                             (' (- (exponent exp) 1)))))
      (deriv (base exp) var)))
    (else
     (error "unknown exp -- DERIV" exp))))
