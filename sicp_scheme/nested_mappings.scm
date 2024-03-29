#lang racket

;; Nested mappings ... bruh
(define nil '())

;|----------------------------------------------
;| i       | 2   3   4   4   5   6   6   |
;| j       | 1   2   1   3   2   1   5   |
;|----------------------------------------------
;| i + j   | 3   5   5   7   7   7   11  |
;|----------------------------------------------

; given a positive integer n, find all ordered pairs if distinct positive
;; integers i and j, where 1 <= j < i <= n such that i + j is prime for


(define (square x) (* x x))

(define (divides? a b) (= (remainder b a ) 0))

(define (next test-divisor)
  (cond ((= test-divisor 2) 3)
        (else (+ test-divisor 2))))


(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n ) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (smallest-divisor n) (find-divisor n 2))

;; (smallest-divisor 28)

(define (prime? n)
  (= n (smallest-divisor n)))


(define (accumulate op initial sequence)
  (if (null? sequence)
      ;; burda nil yani initial sona gidiyo
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;; (accumulate cons '() (list 1 2 3))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

;; zormsm q
(define n 8)
;; (accumulate
;;  ;; map in aldigi proca verdigimiz sey 1 den n e kadar interval?
;;  ;; i = 1 den 8 e kadar sayilarin teker teker alinimi
;;  ;; j = her i icin 1 den i - 1 e kadar olan list
;;  ;; return edilen sey (list i j)
;;  append '() (map (lambda (i)
;;                    ;; list haline getir
;;                    (map (lambda (j) (list i j))
;;                         ;; 1 den i - 1 e kadar list olustur
;;                         ;; mesela i 8 ise 8 den 1 e kadar
;;                         ;; i 2 ise 2 den 1 e kadar
;;                         (enumerate-interval 1 (- i 1))))
;;                  ;; 1 den 8 (n) e kadar olstr
;;                  (enumerate-interval 1 n)))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))


(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  ;; take list and feed with make-pair-sum
  (map make-pair-sum
       ;; filter the prime-sums
       (filter prime-sum? (flatmap
                           (lambda (i)
                             ;; enumerate list from 1 to i - 1
                             (map (lambda (j) (list i j))
                                  (enumerate-interval 1 (- i 1))))
                           (enumerate-interval 1 n)))))
(define (make-pair-s n)
  (map make-pair-sum
       ;; filter prime-sum from unique-pairs
       (filter prime-sum? (unique-pairs n))))

;; (make-pair-s 5)
;; (prime-sum-pairs 5)



(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

(define (permutations s)
  (if (null? s)
      (list nil)
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))


;; Exercise 2.41
;; find all triplets of distinct positive integers i,j and k less than or
;; equal to a given integer n that sum


; k-tuples of [1..n]
(define (unique-tuples n k)
  (cond ((< n k) nil)
        ((= k 0) (list nil))
        (else (append (unique-tuples (- n 1) k)
                      (map (lambda (tuple) (cons n tuple))
                           (unique-tuples (- n 1) (- k 1)))))))

; application to the case of 3-tuples
(define (triples-of-sum s n)
  (filter (lambda (seq) (= (accumulate + 0 seq) s))
          (unique-tuples n 3)))
(triples-of-sum 20 30)

;; Eight queen puzzle 2.42
