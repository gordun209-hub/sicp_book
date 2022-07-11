#lang racket/base
(require racket/trace)
; HELPERS
(define (square x) (* x x))

(define (prime? x) (= (remainder x 2)))

(define (fib n)
  (if (<= n 2)
      1
      (+ (fib (- n 1)) (fib (- n 2)))))

(define (filter predicate sequence)
  (cond
    ((null? sequence) null)
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
      null
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond
    ((null? tree) null)
    ((not (pair? tree)) (list tree))
    (else (append (enumerate-tree (car tree))
                  (enumerate-tree (cdr tree))))))

(define (sum-odd-squares tree)
  (accumulate
   + 0 (map square (filter odd? (enumerate-tree tree)))))

(define (even-fibs n)
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

(define (my-map p seq)
  (accumulate (lambda (x y) (cons (p x ) y)) null seq))

(define (my-append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length seq)
  (accumulate (lambda (x y)(+ 1 y)) 0 seq))

(define (horner-eval x coefficient-seq)
  (accumulate ( lambda (this-coeff higher-terms)
                 (+ (* higher-terms x ) this-coeff))
              0
              coefficient-seq))

(define (count-leaves-recursive t)
  (accumulate + 0 (map (lambda (node)
                         (if (pair? node)
                             (count-leaves-recursive node)
                             1))
                       t)))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;; (define (accumulate op initial sequence)
;;   (if (null? sequence)
;;       initial
;;       (op (car sequence)
;;           (accumulate op initial (cdr sequence)))))


(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))


(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        ; burasi oneml iter ile resultun icine soldan biriktirioz
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))


(define (reverse-using-right items)
  (fold-right (lambda (first already-reversed)
                (append already-reversed (list first)))
              null
              items))

(define (reverse-using-left items)
  (fold-left (lambda (result first) (cons first result))
             null
             items))
(trace fold-left)
;; Test
(define items (list 1 2 3 4 5))

(reverse-using-right items)

(reverse-using-left items)

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
; TODO analmadm TODO
(define (prime-sum-pairs n )
  (map make-pair-sum
       (filter prime-sum? (flatmap
                           (lambda (i)
                             (map (lambda (j) (list i j))
                                  (enumerate-interval 1 (- i 1))))
                           (enumerate-interval 1 n)))))
; TODO analmadm TODO
