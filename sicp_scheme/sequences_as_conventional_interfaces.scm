#lang sicp


(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

(define (square x) (* x x))

(define (sum-odd-squaress tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squaress (car tree))
                 (sum-odd-squaress (cdr tree))))))



(define (even-fibss n)
  (define (next k)
    (if (> k n)
        nil
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))

;; Sequence operations

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
   ;; sagdan sola; enumerate numbers map to fibonacci numbers filter even
   ;; cons them
   (filter even? (map fib (enumerate-interval 0 n)))))

(define (eww n)
  (filter even? (map fib (enumerate-interval 0 n))))

;; (eww 20)
;;
;; (even-fibs 20)


(define (list-fib-squares n)
  ;; map every item in interval to fib then square them
  (map square (map fib (enumerate-interval 0 n))))

;; (list-fib-squares 30)

(define (product-of-squares-of-odd-elements sequence)
  (accumulate * 1 (map square (filter odd? sequence))))

;; (product-of-squares-of-odd-elements (list 1 2 3 4 5))



;; (define (map p sequence)
;;   (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (my-map proc sequence)
  (accumulate (lambda (first already-accumulated)
                (cons (proc first) already-accumulated))
              nil
              sequence))

;; (map abs (list 1 -2 3))

(define (append seq seq2)
  (accumulate cons seq2 seq))

(append (list 3 4) (list 5 6))

(define (length sequence)
  (accumulate (lambda (_ x) (+ x 1)) 0 sequence))

(length (list 2 3 4))


;; (define (horner-eval x coefficient-sequence)
;;   (accumulate (lambda (this-coeff higher-terms)
;;                 (+))
;;               0
;;               coefficient-sequence))
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ (* higher-terms x) this-coeff))
              0
              coefficient-sequence))
(horner-eval 2 (list 1 3 0 5 0 1))

;; exc 2.35 thats how its done
(define (count-leaves t)
  (accumulate + 0 (map  (lambda (x) 1) (enumerate-tree t))))

(count-leaves (list 2 (list 3 4) (list 4) 4))


(define (count-leaves-recursive t)
  (accumulate + 0 (map (lambda (node)
                         (if (pair? node)
                             (count-leaves-recursive node)
                             1))
                       t)))

;; affed
;; Exc 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      ;; because of seqs is list of lists
      ;; we use map lists to car for taking first list and accumulate
      ;; then recur with rest of the list basitmis aq
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))


;; Exercise 2.37

;; represent vectors v = (v i)

;; matrices  m = (m i j))


(define matrix (list (list 1 2 3 4) (list 5 6 7 8) (list 9 10 11 12)))
(define (dot-product v w)
  (accumulate + 0 (map * v w)))


(dot-product (list 1 2 3) (list 4 5 6))


;; a.
(define (matrix-*-vector m v)
  (map (lambda (m-row) (dot-product m-row v))
       m))

;; Test
(matrix-*-vector matrix (list 2 3 4 5))


;; b.
(define (transpose m)
  (accumulate-n cons nil m))

;; Test
(transpose matrix)


;; c.
(define (matrix-*-matrix m n)
  (let ((n-cols (transpose n)))
    (map (lambda (m-row)
           (map (lambda (n-col)
                  (dot-product m-row n-col))
                n-cols))
         m)))

;; ;; But the inner map is just matrix-*-vector, so here's better:
;; (define (matrix-*-matrix m n)
;;   (let ((n-cols (transpose n)))
;;     (map (lambda (m-row) (matrix-*-vector n-cols m-row))
;;          m)))

;; Test
;; (matrix-*-matrix matrix (list (list 1 2) (list 1 2) (list 1 2) (list 1 2)))
;; Tekrr
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (fold-right op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op (car rest) result)
              (cdr rest))))
  (iter initial sequence))
;; initial = nil op = list sequence = (list 1 2 3)
;; (fold-right cons nil (list 1 2 3))
;; (fold-left cons nil (list 1 2 3))
;(iter list nil 1) (list 2 3)
; (iter list (list nil 1) 2) (list 3)
; (iter list (list (list nil 1) 2) 3) ()

(define (reverse sequence)
  (fold-right (lambda (x y) (cons x y)) nil sequence))

;; rest = sonraki item initial = accumulated item
(define (reversee sequence)
  (fold-left (lambda (initial rest) (cons rest initial)) nil sequence))

(reverse (list 1 2 3 4))
