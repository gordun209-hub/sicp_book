#lang sicp

;; mobile consists of two branches, left and right
;; each branch is  a rod of a certaing length from which hangs
;;either a weight or another binary mobile

(define (make-mobile left right)
  (list left right))

;; a branch is constructed from a length together with a structure
;; which may be either a number or another mobile

(define (make-branch length structure)
  (list length structure))


(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (car (cdr mobile)))


(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (car (cdr branch)))

;; Finding the total weight of a binary mobile
;; Using wishful thinking to recurse the addition of left-branch and right-branch mobiles

(define (total-weight mobile)
  (cond ((null? mobile) 0)
        ((not (pair? mobile)) mobile)
        (else (+ (total-weight (branch-structure (left-branch mobile)))
                 (total-weight (branch-structure (right-branch mobile)))))))

;; Test
(define a (make-mobile (make-branch 2 3) (make-branch 2 3)))
a
(total-weight a) ;; 6

(define (torque branch)
  (* (branch-length branch) (total-weight (branch-structure branch))))

;; Finally to check if the torques of both sides are equal
;; And if the sub-mobiles are balanced using recursion

(define (balanced? mobile)
  (if (not (pair? mobile))
      true
      (and (= (torque (left-branch mobile)) (torque (right-branch mobile)))
           (balanced? (branch-structure (left-branch mobile)))
           (balanced? (branch-structure (right-branch mobile))))))

;; Test

(define d (make-mobile (make-branch 10 a) (make-branch 12 5)))
;; Looks like: ((10 ((2 3) (2 3))) (12 5))

(balanced? d) ;; #t
