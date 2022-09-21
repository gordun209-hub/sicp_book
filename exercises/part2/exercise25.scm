#lang sicp

(cdr  '(1 2 ( 5 7) 9))
(cadr (car (cdr (cdr  '(1 2 ( 5 7) 9))))) ;; 7

(car (car '((7))))

(cadr (cadr (cadr (cadr (cadr(cadr '(1 (2 (3 (4 (5 (6 7))))))))))))

(define (get-seven lst)
  (cadr (cadr (cadr (cadr (cadr (cadr lst)))))))

(get-seven '(1 (2 (3 (4 (5 (6 7)))))))
