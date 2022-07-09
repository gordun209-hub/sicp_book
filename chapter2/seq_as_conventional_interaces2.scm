#lang racket/base
(require racket/trace)
;; (define length
;;   (lambda (ls)
;;     (if (null? ls)
;;         0
;;         (+ (length (cdr ls)) 1 ))))
;; ;; (trace length)
;; (length '(a b c d))
;;

(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

;; (trace filter)
;; (filter odd? (list 1 2 3 4 5))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
;;
;; (trace accumulate)
;; (accumulate  0 (list 1 2 3 4 5))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))


(define (enumerate-tree tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define tree '(1 (2 (3 4)) 5))
;; (trace enumerate-tree)
;; (car '((2 (3 4)) 5))
; FIRST RECUR
;; (car tree) ; => 1
;; (cdr tree) ; => ((2 (3 4)) 5)
;; ; SECOND RECUR
;;
;; (car (cdr tree)) ; => (2 (3 4))
;; (cdr (cdr tree)) ; => (5)
;; (car (car (cdr tree)))
;; (cdr (car (cdr tree)))
;; (cdr '((2 (3 4)) 5))
;; (car '(2 (3 4)))
;; (cdr '(2 (3 4)))
;; (pair? '(5))
;; (enumerate-tree tree)
;; (pair? tree)

(define (square x ) (* x x))
(define (sum-odd-squares tree)
  (accumulate
   + 0 (map square (filter odd? (enumerate-tree tree)))))

(define (fib n)
  (if (<= n 2) 1
      (+ (fib (- n 1)) (- n 2))))
;; (fib 5)


(define (even-fibs n )
  (accumulate
   cons
   null
   (filter even? (map fib (enumerate-interval 0 n)))))

(define (list-fib-squares n)
  (accumulate
   cons
   null
   (map square (map fib (enumerate-interval 0 n)))))

(define (product-of-squares-of-odd-elements sequence)
  (accumulate * 1 (map square (filter odd? sequence))))

;; (define (map p sequence)
;;   (accumulate (lambda (x y) (cons (p x) y)) '() sequence))
;; (map (lambda (x) (+ x x)) '(1 2 3))

(define (my-map proc sequence)
  (accumulate (lambda (first already-accumulated)
                (cons (proc first) already-accumulated))
              null
              sequence))

;; Test:

(my-map square (list))
(my-map square (list 1 2 3 4))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ (* higher-terms x) this-coeff))
              0
              coefficient-sequence))
;(horner-eval 2 (list 1 3 0 5 0 1)) => 79 
(define (count-leaves t)
  (accumulate + 0 (map (lambda (node)
                         (if (pair? node)
                             (count-leaves node) 1)) t)))


(define (count-leaves-recursive t)
  (accumulate + 0
              (map
               (lambda (t)
                 (cond ((null? t) 0)
                       ((pair? t) (count-leaves-recursive t))
                       (else 1)))
               t)))
