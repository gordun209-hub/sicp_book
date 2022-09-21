#lang sicp

(define x (list (list 1 2) (list 3 4)))

(define (fringe lst)
  (cond ((null? lst) '())
        ((not (pair? (car lst))) (cons (car lst) (fringe (cdr lst))))
        (else (append (fringe (car lst))
                    (fringe (cdr lst))))))

(fringe x)


(define (fringee tree) 
  (define nil '()) 
  
  (define (build-fringe x result) 
    (cond ((null? x) result) 
          ((not (pair? x)) (cons x result)) 
          (else (build-fringe (car x)  
                              (build-fringe (cdr x) result))))) 
  
  (build-fringe tree nil)) 
(fringee x)
