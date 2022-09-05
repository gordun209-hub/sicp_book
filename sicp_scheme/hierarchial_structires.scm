#lang sicp

(define x (cons (list 1 2) (list 3 4)))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define y (list 1 2 3))
(define z (list 4 5 6))

;; (append y z) ;; (1 2 3  4 5 6)
;; (cons y z);; ((1 2 3) 4 5 6)
;; (list y z) ;; ((1 2 3) (4 5 6))


;; (reverse (list 1 2 3))
;;
;; (define nil '())
;;
;; (define (reverse items)
;;   (define (iter items result)
;;     (if (null? items)
;;         result
;;         (iter (cdr items) (cons (car items) result))))
;;  (iter items nil))


(define (deep-reverse tree)
  (define (iter items result)
    (cond ((null? items) result)
          ((not (pair? items)) (iter (cdr items) (cons items result)))
          (else (iter (cdr items) result))))
  (iter tree nil))

;; (deep-reverse '((3 4) (1 2)))

;; TODO anla
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

;; (eli-deep-reverse x)
;; (define ls '((3 4) (1 2)))
;; ;; (eli-deep-reverse '((3 4) (1 2)))
;; (cdr '((1 2)))

;; Exercise 2.28
;; takes as argument a tree and returns a list whose elements are all the
;; leaves of the tree arranged in left to right order

(define w (list (list 1 2) (list 3 4)))
;; (list w w)
(define (fringee tree)
  (define (fringe-iter leaf result)
    (cond ((null? leaf) result)
          ((not(pair? leaf)) (car leaf) (fringe-iter (cdr leaf) (append result (car leaf))))
          (else (fringe-iter (cdr leaf) result))))
  (fringe-iter tree nil))

;; That doesnt work
(define (fringe tree)
  (define nil '())
  (if (null? tree)
      nil
      (let ((first (car tree)))
        (if (not (pair? first))
            (cons first (fringe (cdr tree)))
            (append (fringe first) (fringe (cdr tree)))))))

;; (fringee  (list w w))

;; Zekice
;; construct the list from the right of the tree, to the left
;; (define (fringeee T)
;;   (define (iter T R)
;;     (cond ((null? T) R)
;;           ((not (pair? T)) (cons T R))
;;           (else (iter (car T)
;;                       (iter (cdr T) R)))))
;;   (iter T '()))
;;
;;
;;
;; (fringe '(2)) ;; 2





;; Exc 2.29
;; lenght or another binary mobile
(define (make-mobile left right)
  (list left right))
;; number or another mobile
(define (make-branch length structure)
  (list length structure))

(define left-branch car)
(define right-branch cdr)

(define branch-length car)
(define branch-structure cdr)







(define (total-weight mobile)
  (cond ((null? mobile) 0)
        ((not (pair? mobile)) mobile)
        (else (+ (total-weight (branch-structure (left-branch mobile)))
                 (total-weight (branch-structure (right-branch mobile)))))))

;; Test
(define a (make-mobile (make-branch 2 3) (make-branch 2 3)))
(total-weight a) ;; 6





(define (torque branch)
  (* (branch-length branch) (total-weight (branch-structure branch))))

;; Finally to check if the torques of both sides are equal
;; And if the sub-mobiles are balanced using recursion
;; f3n4
(define (balanced? mobile)
  (if (not (pair? mobile))
      true
      (and (= (torque (left-branch mobile)) (torque (right-branch mobile)))
           (balanced? (branch-structure (left-branch mobile)))
           (balanced? (branch-structure (right-branch mobile))))))



