#lang sicp
;; length procedure of Section 2.2.1 with the count-leaves procedure,
;; which returns the total number of leaves of a tree:
(define x (cons (list 1 2) (list 3 4)))
(length x)
;; 3
;; (count-leaves x)
;; 4
(list x x)
;; (((1 2) 3 4) ((1 2) 3 4))
(length (list x x))
;; 2
;; (count-leaves (list x x))
;; 8
;; To implement count-leaves, recall the recursive plan for computing
;; length:
;; • length of a list x is 1 plus length of the cdr of x.
;; • length of the empty list is 0.
;; count-leaves is similar. e value for the empty list is the same:
;; • count-leaves of the empty list is 0.
;; But in the reduction step, where we strip off the car of the list, we must
;; take into account that the car may itself be a tree whose leaves we need
;; to count. us, the appropriate reduction step is
;; • count-leaves of a tree x is count-leaves of the car of x plus
;; count-leaves of the cdr of x.
;; Finally, by taking cars we reach actual leaves, so we need another base
;; case:
;; • count-leaves of a leaf is 1.
;; To aid in writing recursive procedures on trees, Scheme provides the
;; primitive predicate pair?, which tests whether its argument is a pair.
;; Here is the complete procedure:

(define (count-leaves x)
  (cond ((null? x ) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))


;; Exercise 2.24: Suppose we evaluate the expression (list
;; 1 (list 2 (list 3 4))). Give the result printed by the
;; interpreter, the corresponding box-and-pointer structure,
;; and the interpretation of this as a tree (as in Figure 2.6).

;; (1 (2 (3 4)))
;;      ^
;;    /   \
;;   1     ^ (2 (3 4))
;;       /   \
;;      2     ^ (3 4)
;;          /   \
;;         3     4



;; Exercise 2.25: Give combinations of cars and cdrs that
;; will pick 7 from each of the following lists:
;; (1 3 (5 7) 9) ; (cdr (cdr (cdr (cdr list))))
(cdr (car (cdr (cdr '(1 3 (5 7) 9))))) ; 7 bruh
;; ((7))
(car (car '((7)))) ; 7
(cadr  (cadr  (cadr (cadr (cadr (cadr '(1 (2 (3 (4 (5 (6 7)))))))))))) ; 7
; ; (1 (2 (3 (4 (5 (6 7))))))



;; Exercise 2.26: Suppose we define x and y to be two lists:
(define a (list 1 2 3))
(define b (list 4 5 6))
;; What result is printed by the interpreter in response to eval-
;; uating each of the following expressions:
(append a b)
;; ()
(cons a b)
(list a b)

(define (reverse items)
  (define (iter items result)
    (if (null? items)
        result
        (iter (cdr items) (cons (car items) result))))

  (iter items nil))
;; Exercise 2.27: Modify your reverse procedure of Exercise
;; 2.18 to produce a deep-reverse procedure that takes a list
;; as argument and returns as its value the list with its ele-
;; ments reversed and with all sublists deep-reversed as well.
;; For example,
;; (define x (list (list 1 2) (list 3 4)))
;; x
;; ((1 2) (3 4))
(reverse x)
;; ((3 4) (1 2))
;; (deep-reverse x)
;; ((4 3) (2 1))

;; First try:
(define (deep-reverse items)
  (define (deep-rev-imp items result)
    (if (null? items)
        result
        (let ((first (car items)))
          (deep-rev-imp (cdr items)
                        (cons (if (not (pair? first))
                                  first
                                  (deep-reverse first))
                              result)))))
  (deep-rev-imp items nil))


(define (deep-reverse-2 items)
  (define (deep-rev-if-required item)
    (if (not (pair? item))
        item
        (deep-reverse-2 item)))
  (define (deep-rev-imp items result)
    (if (null? items)
        result
        (deep-rev-imp (cdr items)
                      (cons (deep-rev-if-required (car items))
                            result))))
  (deep-rev-imp items nil))


(define (eli-deep-reverse lst)
  (cond ((null? lst) nil)
        ((pair? (car lst))
         (append
          (eli-deep-reverse (cdr lst))
          (list (eli-deep-reverse (car lst)))))
        (else
         (append
          (eli-deep-reverse (cdr lst))
          (list (car lst))))))


;; Exercise 2.28: Write a procedure fringe that takes as argu-
;; ment a tree (represented as a list) and returns a list whose
;; elements are all the leaves of the tree arranged in le-to-
;; right order. For example,
(define s (list (list 1 2) (list 3 4)))
s

(define (fringe tree)
  (define nil '())

  (define (build-fringe x result)
    (cond ((null? x) result)
          ((not (pair? x)) (cons x result))
          (else (build-fringe (car x)
                              (build-fringe (cdr x) result)))))

  (build-fringe tree nil))
(define my-tree (list 1 (list 2 (list 3 4) (list 5 6)) (list 7 (list 8))))

(fringe my-tree)

(define (fringe-better tree)
  (define nil '())
  (if (null? tree)
      nil
      (let ((first (car tree)))
        (if (not (pair? first))
            (cons first (fringe-better (cdr tree)))
            (append (fringe-better first) (fringe-better (cdr tree)))))))

