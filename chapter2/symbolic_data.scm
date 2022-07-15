#lang racket/base
(require racket/trace)
(define a 1)
(define b 2)

;; (list a b); => (1 2)
;;
;; (list 'a 'b) ; => (a b)
;;
;; (list 'a b) ; => (a 2)
;;
;; (car '(a b c)) ; => a
;;
;; (cdr '(a b c)) ; => (b c)

;; One additional primitive used in manipulating symbols is eq?, which
;; takes two symbols as arguments and tests whether they are the same.

(define (memq item x)
  (cond ((null? x) #f)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

;; (trace memq)

;; (memq 'apple '(pear babana prune))
;;
;; (memq 'apple '(x (apple sauce) y apple pear))


;Exercise 2.53 What would the interpreter print in response
;to evaluating each of the following expressions?

;; (list 'a 'b 'c) ; => (a b c)
;;
;; (list (list 'george)) ; => ((george))
;;
;; (cdr '((x1 x2) (y2 y2))); => ((y2 y2))
;;
;; (cadr '((x1 x2) (y2 y2))) ; => (y2 y2)
;;
;; (pair? (car '(a short list))) ; #f
;;
;; (memq 'red '((red shoes) (blue socks))) ; => #f
;;
;; (memq 'red '(red shoes blue socks)) ; => '(red shoes blue socks)
; benim cozumum
(define equal?
  (lambda (l1 l2)
    (cond
      ((null? l1) #t)
      ((not (eq? (car l1) (car l2))) #f)
      (else
       (equal? (cdr l1) (cdr l2))))))

;; (eq? 1 22)
;; (trace equal?)
;; (equal? (list 1 2 3) (list 1 2 3))


(define (equal2? a b)
  (if (and (pair? a) (pair? b))
      (and (equal2? (car a) (car b)) (equal2? (cdr a) (cdr b)))
      (eq? a b)))

(define (equal3? a b)
  (if (or
       (eq? a b)
       (and
        (or
         (and
          (pair? a)
          (pair? b))
         (and
          (null? a)
          (null? b)))
        (and
         (equal3? (car a) (car b))
         (equal3? (cdr a) (cdr b)))))
      #t #f))

;; (equal3? (list 1 2 3) (list 3 4 5))

; Exercise 2.55
(car ''abracadabra)
(pair? ''abb)


