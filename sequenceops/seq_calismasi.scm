#lang racket/base
(require racket/trace)
; HELPERS
(define (square x) (* x x))

(define (prime? x)
  (define (test divisor)
    (cond ((> (* divisor divisor) x) #t)
          ((= 0 (remainder x divisor)) #f)
          (else (test (+ divisor 1)))))
  (test 2))


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

(define (permutations s)
  (if (null? s)
      (list null)
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))


;; Exercise 2.40: Define a procedure unique-pairs that, given
;; an integer n, generates the sequence of pairs (i, j) with 1 ≤
;; j < i ≤ n. Use unique-pairs to simplify the definition of
;; prime-sum-pairs given above.


(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))


(define (prime-sum-pairss n )
  (map (make-pair-sum
        (filter prime-sum? (unique-pairs n)))))

;; Exercise 2.41: Write a procedure to find all ordered triples
;; of distinct positive integers i, j, and k less than or equal to
;; a given integer n that sum to a given integer s

; ANLAMADIM WTF TODO ANLAMAYA CALS TODO TODO
; k-tuples of [1..n]
(define (unique-tuples n k)
  (cond ((< n k) null)
        ((= k 0) (list null))
        (else (append (unique-tuples (- n 1) k)
                      (map (lambda (tuple) (cons n tuple))
                           (unique-tuples (- n 1) (- k 1)))))))

; application to the case of 3-tuples
(define (triples-of-sum s n)
  (filter (lambda (seq) (= (accumulate + 0 seq) s))
          (unique-tuples n 3)))
(triples-of-sum 20 30)

; TODO queen  exercise coz
