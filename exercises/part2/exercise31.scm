#lang sicp

(define (map-tree  proc tree)
  (cond ((null? tree) nil)
        ((not (pair? tree))
         (proc tree))
        (else 
          (cons (map-tree proc (car tree))
                (map-tree proc (cdr tree))))))

(define mytree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(define (square x )(* x x))

(define (square-tree tree)
  (map-tree square tree))


(square-tree mytree)
