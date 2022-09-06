#lang sicp

;; Sets as unordered lists

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

(define (union-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))

(define (union-sett s1 s2)
  (cond ((and (null? s1) (not (null? s2)))
         s2)
        ((and (not (null? s1)) (null? s2))
         s1)
        ((element-of-set? (car s1) s2)
         (union-sett (cdr s1) s2))
        (else
         (cons (car s1)
               (union-sett (cdr s1) s2)))))

(union-set '(2 3 4 5 6) '(2 3 4 9 1))
(union-sett '(2 3 4 5 6) '(2 3 4 9 1))
(intersection-set '(2 3 4 5 6) '(2 3 4 9 1))

;; Exc 2.60

(define sss '(2 3 2 1 3 2 2)) ;; => (1 2 3)



(define (remove-dups set)
  (define (remove-iter set init)
    (cond ((null? set) init)
          ((element-of-set? (car set) init)
           (remove-iter (cdr set) init))
          (else
           (remove-iter (cdr set) (cons (car set) init)))))
  (remove-iter set '()))

(remove-dups sss)

;; use remove-dups for all above DONE!!








