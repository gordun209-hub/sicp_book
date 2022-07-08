#lang sicp


(define (same-parity first . rest)
  (let ((yes? (if (even? first)
                  even?
                  odd?)))
    (define (iter items result)
      (if (null? items)
          (reverse result)
          (iter (cdr items)
                (if (yes? (car items))
                    (cons (car items) result)
                    result))))
    (iter rest (list first))))

(define (same-parity3 first . rest)
  (let ((yes? (if (even? first)
                  even?
                  odd?)))
    (define (iter result items)
      (if (null? items) result)
      (iter (if (yes? (car items))
                (cons (car items) result)
                result)
            (cdr items)))
    (iter 'first rest)))




;; (same-parity 2 4 5 6 7)

(define (sam-parity first . rest)
  (define (inter yes? lat)
    (cond
      ((null? lat) (quote ()))
      ((yes? (car lat)) (cons (car lat) (inter yes? (cdr lat))))
      (else
       (inter yes? (cdr lat)))))
  (if (odd? first)
      (inter odd? rest)
      (inter even? rest)))

(define (same-parity-2 first . rest)
  (define (congruent-to-first-mod-2? a)
    (= (remainder a 2) (remainder first 2)))

  (define (select-same-parity items)
    (if (null? items)
        items
        (let ((curr (car items))
              (select-rest (select-same-parity (cdr items))))
          (if (congruent-to-first-mod-2? curr)
              (cons curr select-rest)
              select-rest))))
  (cons first (select-same-parity rest)))



;; an alternative implementation by andras:
(define (same-parity-andras a . l)
  (define (sp-builder result tail)
    (if (null? tail)
        result
        (if (even? (+ a (car tail)))
            ;;test for same parity
            ;;if the current beginning of the rest (car tail) is the sa
            ;me parity as "a", then it is appended to the result, else the result is left untouched
            (sp-builder (append result (list (car tail))) (cdr tail))
            (sp-builder result (cdr tail)))))
  (sp-builder (list a) l))

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

;; (map abs (list -10 2.5 -11.6 17))

;; ((lambda (x) (* x x)) 2)

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))

(define (square-list items)
  (if (null? items)
      nil
      (cons (* (car items) (car items)) (square-list (cdr items)))))

;; (square-list (list 2 3 4))

(define suqare-with-lambda
  (lambda (items)
    (map (lambda (x) (* x x)) items)))
;; (suqare-with-lambda (list 2 3 4))

(define (square x) (* x x))

(define (square-list-2 items)
  (if (null? (cdr items))
      items
      (append (square-list-2 (cdr items))
              (cons (* (car items) (car items)) nil))))


;; (square-list-2 '(2 5 6 22))
(define (different-reverse items)
  (if (null? (cdr items))
      items
      (append (different-reverse (cdr items))
              (cons (car items) nil))))


;; (square-list-2 '(5 2 4))

; (iter '(5 2 4) nil)
; (iter (2 4) (cons (square (5)) nil))
; (iter (2 4) (25)
;; (cons (square (car '(5 2 4))) nil) ; => 25
; (iter (4) (cons (square 2) (25)))
;; (append (list 25) (square (car (list 2 3))))
;(cons (square 2) '(25)) ; => (4 25)


(define (square-list-23 items)
  (define (iter l pick)
    (define r (square (car l)))
    (if (null? (cdr l))
        (pick (list r))
        (iter (cdr l) (lambda (x) (pick (cons r x))))))
  (iter items (lambda (x) x)))

; (reverse '(2 5 3))
;(apend (reverse (5 3)) (cons 2 nil))
;(append (reverse (3)) (cons 5 nil) (cons 2 nil))
;(append (reverse ()) (cons 3 nil) (cons 5 nil) (cons 2 nil))
;; (append '() (cons 3 nil) (cons 5 nil) (cons 2 nil))

(define (count-leaves x)
  (cond
    ((null? x) 0)
    ((not (pair? x)) 1)
    (else (+
           (count-leaves (car x))
           (count-leaves (cdr x))))))


(define (reverse items)
  (define (iter items result)
    (if (null? items)
        result
        (iter (cdr items) (cons (car items) result))))
  (iter items nil))

;; (define reverse-tree
;;   (lambda (items)
;;     (cond
;;       ((null? items)
;;        '())
;;       ((not (pair? (car items)))
;;        (list (car items)))
;;       (else (cons
;;              (reverse-tree (car items))
;;              (reverse-tree (cdr items)))))))
;; (define x (list (list 1 2) (list 3 4)))
;; (reverse-tree x)


; itrerative solution
(define (i-reverse l)
  (define (it-rev lat ans)
    (if (null? lat)
        ans
        (it-rev (cdr lat) (cons (car lat) ans))))
  (it-rev l '()))

;recursive solution
(define (r-reverse lat)
  (if (null? lat)
      '()
      (append (r-reverse (cdr lat)) (list (car lat)))))


(define x (list (list 1 2) (list 3 (list 4 5))))

(define (reverse-tree items)
  (define (rev-imp items result)
    (if (null? items)
        result
        (rev-imp (cdr items) (cons (car items) result))))
  (rev-imp items nil))

(reverse-tree x)

(define (deep-reverse items)
  (define (deep-rev-imp items result)
    (if (null? items)
        result
        (let ((first (car items)))
          (deep-rev-imp (cdr items)
                        (cons (if (not (pair? first))
                                  first
                                  ; call it again if pair
                                  (deep-reverse first))
                              result)))))
  (deep-rev-imp items nil))

(deep-reverse x)

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

(deep-reverse-2 x)


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

(eli-deep-reverse x)



(define (make-mobile left right)
  (list left right))
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (car (cdr mobile)))

(define (make-branch length structure)
  (list length structure))

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

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (* sub-tree sub-tree)))tree))

(square-tree (list 1 
                   (list 2 (list 3 4) 5)
                   (list 6 7)))


(define (subsets s)
  (if (null? s)
    (list nil)
    (let ((rest (subsets (cdr s))))
      (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(subsets (list 1 2 3))
