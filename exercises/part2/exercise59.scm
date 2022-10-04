#lang sicp
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set s1 s2)
  (cond ((and (null? s1) (not (null? s2)))
         s2)
        ((and (not (null? s1)) (null? s2))
         s1)
        ((element-of-set? (car s1) s2)
         (union-set (cdr s1) s2))
        (else (cons (car s1)
                    (union-set (cdr s1) s2)))))
