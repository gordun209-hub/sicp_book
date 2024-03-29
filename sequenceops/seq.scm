#lang racket/base
; TODO burayi anla
(require racket/trace)
(require errortrace)
(define (filter predicate sequence)
  (cond ((null? sequence ) null)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
;(trace filter)
;(filter odd? '(1 2 3 4 5))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (my-append list1 list2)
  (accumulate cons
              list2
              list1))
;; (trace my-append)
;; (my-append '(2 3 4) '(5 6))
;op   'initial sequence
; (accumulate cons '(5 6) '(2 3 4))
; (cons (2 (accumulate cons '(5 6) '(3 4))))
; (cons 2 (cons 3 (accumulate cons '(5 6) '(4))))
; (cons 2 (cons 3 (cons 4 (accumulate cons '(5 6) '()))))
;; (cons 2 (cons 3 (cons 4 (cons '() '(5 6)))))


;; (accumulate + 2 (list 1 2 3 4 5))

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))

;; (trace enumerate-interval)
;; (enumerate-interval 2 7)

(define (enumerate-tree tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))
;; (trace enumerate-tree)
;; (enumerate-tree (list 1 (list 2 (list 3 4)) 5))

(define (square x) (* x x))
(define (sum-odd-squares tree)
  (accumulate
   + 0 (map square (filter odd? (enumerate-tree tree)))))

;; (sum-odd-squares (list 1 (list 2 3) 4 (list 5 6)))

; TODO
(define (my-map proc sequence)
  (accumulate (lambda (first already-accumulated)
                (cons (proc first) already-accumulated))
              null
              sequence))


(define (my-length sequence)
  (accumulate (lambda (first already-acc)
                (+ 1 already-acc))
              0
              sequence))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ (* higher-terms x) this-coeff))
              0
              coefficient-sequence))

;; (horner-eval 2 (list  1 3 0 5 0 1))

(define (count-leaves t)
  (accumulate + 0 (map (lambda (node)
                         (if (pair? node)
                             (count-leaves node)
                             1))
                       t)))

(define (count-leaves-recursive t)
  (accumulate + 0
              (map
               (lambda (t)
                 (cond ((null? t) 0)
                       ((pair? t) (count-leaves-recursive t))
                       (else 1)))
               t)))

;; Exercise 2.36: e procedure accumulate-n is similar to
;; accumulate except that it takes as its third argument a se-
;; quence of sequences, which are all assumed to have the
;; same number of elements. It applies the designated accu-
;; mulation procedure to combine all the first elements of the
;; sequences, all the second elements of the sequences, and so
;; on, and returns a sequence of the results. For instance, if s
;; is a sequence containing four sequences, ((1 2 3) (4 5 6)
;; (7 8 9) (10 11 12)), then the value of (accumulate-n +
;; 0 s) should be the sequence (22 26 30). Fill in the missing
;; expressions in the following definition of accumulate-n:
(define t (list (list 1 2 3) (list 40 50 60) (list 700 800 900)))

(define (select-cars sequence)
  (map car sequence))

(define (select-cdrs sequence)
  (map cdr sequence))

;; (select-cars t)
;; (select-cdrs t)
; 891982389123 IQ movement
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;; (accumulate-n + 0 t)

; (dot-product v w) returns the sum
; (matrix-*-vector m v) returns vector t where

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (w)
         (dot-product v w)) m))

(define (transpose m)
  (accumulate-n cons '() m))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v) (matrix-*-vector cols v)) m)))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))
;; (trace fold-left)

;; (fold-left + 0 (list 1 2 3))

;; (define n 6)
;; (accumulate append null (map (lambda (i)
;;                                (map (lambda (j) (list i j))
;;                                     (enumerate-interval 1 (- i 1))))
;;                              (enumerate-interval 1 n)))

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))


(define (prime? x) (= 1 (remainder x 2)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

; combine all TODO
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (flatmap
                           (lambda (i)
                             (map (lambda (j)
                                    (newline)
                                    (display i)
                                    (display j) (list i j))
                                  ; enumerate between i and i minus 1
                                  (enumerate-interval 1 (- i 1))))
                           (enumerate-interval 1 n)))))
;; (prime-sum-pairs 5)

(define (permutations s)
  (if (null? s)
      (list null)
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))


(define (map-2 p seq)
  (accumulate (lambda (x y) (cons (p x )y ))
              null
              seq))

(map-2 (lambda (x) (+ x x)) (list 1 2 3 4))
