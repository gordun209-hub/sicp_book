#lang sicp
(define (square x) (* x x))

;; Problem here is that reverses because it begins with start of list and
;; conses that last
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))
;; this conses a element to a list so fcked up
(define (square-list2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))


;; possible fix

(define (square-list-2 items)
  (define (iter l pick)
    (define r (square (car l)))
    (if (null? (cdr l))
        (pick (list r))
        (iter (cdr l) (lambda (x) (pick (cons r x))))))
  (iter items (lambda (x) x)))
(square-list-2 '(2 3 4 5))
