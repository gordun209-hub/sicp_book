#lang sicp

(define (scale-tree tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)


(define (scale-tree-2 tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree-2 sub-tree factor)
             (* sub-tree factor)))
       tree))

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (* sub-tree sub-tree)))
       tree))
(define (square-tree2 tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree2 (car tree))
                    (square-tree2 (cdr tree))))))

(square-tree2
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

(define (tree-map tree proc)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (proc tree))
        (else (cons (tree-map (car tree) proc)
                    (tree-map (cdr tree) proc)))))

(define (square x) (* x x))

(define (square-treee tree) (tree-map tree square))
(square-treee (list 1 (list 2 (list 3 4) 5) (list 6 7)))


(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x )) rest)))))

(subsets '(1 2 3 4))
