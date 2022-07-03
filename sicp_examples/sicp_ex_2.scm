#lang sicp
; Exercies 2.21
;; (define (square-list list)
;;   (map (lambda (x) (* x x)) list))
;;
;; (square-list (list 1 2 3 4))

(define (square-list-without-map list)
  (if (null? list)
      nil
      (cons (* (car list) (car list) ) (square-list-without-map (cdr list)))))

(square-list-without-map (list 1 2 3 4))

;; Exercise 2.22: Louis Reasoner tries to rewrite the first square-
;; list procedure of Exercise 2.21 so that it evolves an itera-
;; tive process:
;; (define (square-list items)
;; (define (iter things answer)
;; (if (null? things)
;; answer
;; (iter (cdr things)
;; (cons (square (car things))
;; answer))))
;; (iter items nil))
;; Unfortunately, defining square-list this way produces the
;; answer list in the reverse order of the one desired. Why?
;; Louis then tries to fix his bug by interchanging the argu-
;; ments to cons:
;; (define (square-list items)
;; 145
;; (define (iter things answer)
;; (if (null? things)
;; answer
;; (iter (cdr things)
;; (cons answer
;; (square (car things))))))
;; (iter items nil))
;; this doesn’t work either. Explain
(define (square x) (* x x))




;; (define (square-list items)
;;   (define (iter things answer)
;;     (if (null? things)
;;         answer
;;         (iter (cdr things)
;;               (cons (square (car things)) answer))))
;;   (iter items nil))
;;
;; (square-list (list 1 2 3 4))

;; The above doesn't work because it conses the last item from the
;; front of the list to the answer, then gets the next item from the
;; front, etc.

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer (square (car things))))))
  (iter items nil))

(square-list (list 1 2 3 4))
