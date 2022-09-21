#lang sicp
(define (square x) (* x x))

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))







(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial 
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
    nil
    (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (sum-odd-squares tree)
  (accumulate 
    + 0 (map square (filter odd? (enumerate-tree tree)))))

(define (even-fibs n)
  (accumulate 
    cons 
    nil 
    (filter even? (map fib (enumerate-interval 0 n)))))

(define (list-fib-squares n)
  (accumulate cons nil (map square (map fib (enumerate-interval 0 n)))))

(define (product-of-squares-of-odd-elements sequence)
  (accumulate * 1 (map square (filter odd? sequence))))



(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(map (lambda (x) (+ x x)) '(1 2 3 4))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
(append '(1 2 3) '(3 4 5))

(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

(length '(2 3))


