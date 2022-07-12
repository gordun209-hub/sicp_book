#lang sicp

;; 1. Special Forms
;; (a) define (sugared form) (define (name parameters) expressions)
;; This form is equivalent to (define name (lambda (parameters) expressions)).
;; (b) let (let bindings body)
;; Binds the given bindings for the duration of the body. The bindings is a list of (name
;; value) pairs. The body consists of one or more expressions which are evaluated in order
;; and the value of last is returned



;; 1. Guess the value, then evaluate the expression in scheme. If your guess differs from the actual
;; output, try desugaring any relevant expressions.


(define (foo x)
  (+ x 3))

foo ; procedure

(foo 5) ; 8



;; Data Structures
;; New procedures
;; 1. (cons a b) Makes a conscell (pair) from a and b
;; 2. (car c) extracts the value of the first part of the pair
;; 3. (cdr c) extracts the value of the second part of the pair
;; 5. (list a b c ...) builds a list of the arguments to the procedure
;; 6. (listref lst n) returns the nth element of lst
;; 7. (append l1 l2) makes a new list containing the elements of both lists
;; 8. (null? lst) is lst the empty list?

(list 1 2 3)
(list (list 1 2) (list 3 4) (list 5 6)) ;((1 2) (3 4) (5 6))

(list (list 4 7) 2)

(car (cdr (cdr (cdr (list 7 6 5 4 3 2 1)))))

(car(cdr (cdr (car (cdr (list (list 7) (list 6 5 4) (list 3 2) 1))))))

(car (car (cdr (car (cdr(car (cdr(list 7 (list 6 (list 5 (list 4 (list 3 (list 2 (list 1))))))))))))))

(car (car (car (cdr (cdr (car (car ( cdr (list 7 (list (list 6 5 (list (list 4)) 3 ) 2) 1)))))))))


; Lec 5

;; Write listcopy, which takes a list and returns an identical new list (ie do not just return
;; the original list, cons up a new list).

(define list-copy
  (lambda (l)
    (cond
      ((null?  l) nil)
      (else
       (cons (car l) (list-copy (cdr l)))))))

;; (list-copy (list 1 2 3 4))
; MY exc do with reverse bruh

;; Write ncopies, which takes a value and a number of copies, and returns a list
;;with the
;; appropriate number of copies.

(define n-copies
  (lambda (val n)
    (cond
      ((= n 0) '())
      (else
       (append  val (n-copies  val (- n 1)))))))


;; (n-copies (list 1 2 3) 2)
;;
;; (n-copies (list 3) 3)

;Write reverse, which takes a list and returns new list with the order of the
;elements reversed.

(define reverse2
  (lambda  (l)
    (cond
      ((null? l) '())
      (else
       (append (reverse (cdr l)) (list (car l)))))))
; that differs from upside wersion brhaps
;; (define reverse
;;   (lambda (l)
;;     (cond
;;       ((null? l) '())
;;       (else
;;         (append (list (car l)) (reverse (cdr l)))))))

; IM FCKNG PROUD OF THAT
(define (rev l)
  (define (rev-iter l res)
    (cond
      ((null? l) res)
      (else
       (rev-iter (cdr l) (cons (car l) res)))))
  (rev-iter l nil))

;; (cons  2 '())
;; (cons 2 (cons 3 '()))
;;
;;
;; (rev (list 2 3 4 5))

;; Write append, which takes two lists and returns a new list with the elements
;of the first list
;; and the second list.

(define appendx
  (lambda (x y)
    (cond
      ((null? x) y)
      (else
       (cons (car x) (appendx (cdr x)  y))))))
(append nil (list 1 2))
(append (list 3 4) (list 1 2))

;; Write listref, which takes a list and an index (starting at 0), and returns
;;the nth element  of the list. You may assume that the index is less than the
;length of the list.

(define list-ref 
  (lambda (l i)
    (cond 
      ((= 0 i) (car l))
      (else 
        (list-ref (cdr l) (- i 1))))))

(list-ref (list 17 42 35 "hike" ) 0)

(list-ref (list 17 42 35 "hike") 1)
(list-ref (list 17 42 35 "hike") 2)

