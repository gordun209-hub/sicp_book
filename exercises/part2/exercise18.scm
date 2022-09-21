#lang sicp

(define (reverse lst)
  (define (iter lst answer)
    (cond ((null? lst) answer)
          (else
           (iter (cdr lst) (cons (car lst) answer)))))
  (iter lst '()))

(reverse (list 1 4 9 16 25))



;; second reverse


(define (reverse2 items)
  (if (null? (cdr items))
      items
      (append (reverse2 (cdr items))
              (cons (car items) nil))))

(reverse2 (list 1 2 3 4))




(define (reverse3 items)
  (if (null? (cdr items))
      items
      (append (reverse3 (cdr items))
              (list (car items)))))
