#lang sicp


;; Exercise 2.4: Here is an alternative procedural representa-
;; tion of pairs. For this representation, verify that (car (cons
;; x y)) yields x for any objects x and y.
;; (define (cons x y)
;; (lambda (m) (m x y)))
;; (define (car z)
;; (z (lambda (p q) p)))
;; What is the corresponding definition of cdr? (Hint: To ver-
;; ify that this works, make use of the substitution model of
;; Section 1.1.5.)



;; Exercise 2.5: Show that we can represent pairs of nonneg-
;; ative integers using only numbers and arithmetic opera-
;; tions if we represent the pair a and b as the integer that is
;; the product 2a 3b . Give the corresponding definitions of the
;; procedures cons, car, and cdr.

(define (exp base n)
  (define (iter x result)
    (if (= 0 x)
        result
        (iter (- x 1) (* base result))))
  (iter n 1))

(define (count-0-remainder-division n divisor)
  (define (iter try-exp)
    (if (= 0 (remainder n (exp divisor try-exp)))
        (iter (+ try-exp 1))
        (- try-exp 1)))
  (iter 1))



(define (my-cons a b)
  (*  (exp 2 a) (exp 3 b)))

(define (my-car z) (count-0-remainder-division z 2))
(define (my-cdr z) (count-0-remainder-division z 3))

;; (define test (my-cons 11 17))
;; (my-car test)
;; (my-cdr test)

; 2.6

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;; (lambda (f) (lambda (x) (f ((zero f) x))))
;;
;; (lambda (f) (lambda (x) (f x)))

(define one (lambda (f) (lambda (x) (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

;; (lambda (f) (lambda (x) (f ((one f) x))))

; System to compute electrical quantities, Parallel equivalent resistance of Rp
; of two resistors R1 R2 using the formula
(define (Rp R1 R2)
  (/ 1
     (+ (/ 1 R1) (/ 1 R2))))

;; if you buy a resis-
;; tor labeled “6.8 ohms with 10% tolerance” you can only be sure that the
;; resistor has a resistance between 6.8 − 0.68 = 6.12 and 6.8 + 0.68 = 7.48
;; ohms. us, if you have a 6.8-ohm 10% resistor in parallel with a 4.7-ohm
;; 5% resistor, the resistance of the combination can range from about 2.58
;; ohms (if the two resistors are at the lower bounds) to about 2.97 ohms
;; (if the two resistors are at the upper bounds).

; 6.8 + 0.68
; Ex 2.7
(define (make-interval a b) (cons a b))
(define (upper-bound z) (car z))
(define (lower-bound z) (cdr z))
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound)y)))

; Ex 2.8
(define (sub-interval i1 i2)
  (cons (- (lower-bound i1) (lower-bound i2))
        (- (upper-bound i1) (upper-bound i2))))

; ex 2.9
(define (width i)
  (/ (- ( upper-bound i) (lower-bound i))
     2))

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (length-it items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))

(define squares '(1 4 9 16 25))
(define odds '(1 3 5 7 ))

;; (append squares odds)
;; (cons squares odds)

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (last-pair list)
  (cond
    ((null? (cdr list)) (car list))
    (else
     (last-pair (cdr list)))))

(last-pair '(a b c s w))

;; (define (reverse list)
;;   (cond
;;     ((null? list) (quote ()))
;;     (else
;;       (cons (reverse (cdr list)) (car list)))))

(define (reverse list)
  (define (reverse-iter l new-list)
    (cond
      ((null? l) new-list)
      (else
       (reverse-iter (cdr l) (cons (car l) new-list)))))
  (reverse-iter list nil))

;; (reverse (list 1 4 9 16 25))

; Exercise 2.20

; given the definition
;(define (f x y . z) <body>)
; the procedure f can be called with two or more agruments if we evaluate
;; (f 1 2 3 4 5 6)
; then in the body of f, x will be 1 y will be 2 and z will be the list
; (3 4 5 6) given the definiton
;(define (g . w) ⟨body⟩)

; the procedure g can be called with zero or more arguments if we evaluate
; (g 1 2 3 4 5 6)
; body of g, w will be list (1 2 3 4 5 6)
; write procedure same-parity that takes one or more integers and returns a list
; of all the arguments that have the same even-odd parity as the first arg

; (same-parity 1 2 3 4 5 6 7)
; (1 3 5 7)

; (same-parity 2 3 4 5 6 7)
; ( 2 4 6)

(define (same-parity first . rest)
  (define (same-parity-iter source dist remainder-val)
    (if (null? source)
        dist
        (same-parity-iter (cdr source)
                          (if (= (remainder (car source) 2) remainder-val)
                              (append dist (list (car source)))
                              dist)
                          remainder-val)))
  ; source ; first      ; even or odd
  (same-parity-iter rest (list first) (remainder first 2)))

(define (same-parity-better first . rest)
  (define (congruent-to-first-mod-2? a)
    (= (remainder a 2) (remainder first 2)))

  (define (select-same-parity-better items)
    (if (null? items)
        items
        (let ((curr (car items))
              (select-rest (select-same-parity-better (cdr items))))
          (if (congruent-to-first-mod-2? curr)
              (cons curr select-rest)
              select-rest))))

  (cons first (select-same-parity-better rest)))


(define (scale-list items factor)
  (if (null? items)
      nil
      (cons (* (car items) factor)
            (scale-list (cdr items)
                        factor ))))

(define (scale-list-with-map items factor)
  (map (lambda (x) (* x factor))
       items))

(define (square-list items)
  (if (null? items)
      nil
      (cons (* (car items) (car items)) (square-list (cdr items)))))

;; (square-list '(2 5 6))

(define (for-each f list)
  (cond
    ((null? list) #t)
    (else
     ( for-each f (cdr list)) (f (car list)))))

;; (for-each (lambda (x)
;;             (newline)
;;             (display x))
;;           (list 57 321 88))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define x  (list 1 2 3))
(define y (list 4 5 6))

;; (append x y) ; => (1 2 3 4 5 6)
;; (cons x y) ; => ((1 2 3 ) 4 5 6)
;; (list x y) ; => ((1 2 3) (4 5 6))


; Exc 2.27 Modify your reverse to deep-reverse
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


(define (deep-reverse lst)
  (cond
    ((null? lst) nil)
    ((pair? (car lst))
     (append
      (deep-reverse (cdr lst))
      (list (deep-reverse (car lst)))))
    (else
     (append
      (deep-reverse (cdr lst))
      (list (car lst))))))

(define r (list (list 1 2) (list 3 4)))

;; (deep-reverse r)

(define w (list (list 1 2) (list 3 4)))

; (fringe x)
; (1 2 3 4)
; (fringe (list x x))
; (1 2 3 4 1 2 3 4)

;; (define (fringe tree)
;;   (define nil '())
;;
;;   (define (build-fringe x result)
;;     (cond ((null? x) result)
;;           ((not (pair? x)) (cons x result))
;;           (else (build-fringe (car x)
;;                               (build-fringe (cdr x) result)))))
;;
;;   (build-fringe tree nil))
;; (define (fringe x)
;;   (cond
;;     ((null? x) nil)
;;     ((pair? (car x))
;;      (fringe (car x)))
;;     (else
;;      (cons (car x) (cons  (fringe (cdr x) (fringe (car x))))))))

(define (fringe-betr tree)
  (if (null? tree) nil)
  (let ((first (car tree)))
    (if (not (pair? first))
        (cons first (fringe-betr (cdr tree)))
        (append (fringe-betr first) (fringe-betr (cdr tree))))))
;; (fringe w)
; (1 2 3 4)
;; (fringe (list w w))

;; (define (deep-reverse items)
;;   (define (deep-rev-imp items result)
;;     (if (null? items)
;;         result
;;         (let ((first (car items)))
;;           (deep-rev-imp (cdr items)
;;                    (cons (if (not (pair? first))
;;                              first
;;                              (deep-reverse first))
;;                          result)))))
;;   (deep-rev-imp items nil))

;; (define (eli-deep-reverse lst)
;;    (cond ((null? lst) nil)
;;          ((pair? (car lst))
;;           (append
;;            (eli-deep-reverse (cdr lst))
;;            (list (eli-deep-reverse (car lst)))))
;;          (else
;;           (append
;;            (eli-deep-reverse (cdr lst))
;;            (list (car lst))))))



; Exercise 2.29

; A binary mobile consists of two branches, left and right.
; each branch is a rod of certain length from which hangs either a weight or
; another binary mobile we can represent a binary mobile using compound data by
; constructuriong it from two branches ex:
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cond
    ((not (pair? (cdr (car mobile))))
     (car mobile))
    (else
     (cons (right-branch (car mobile)) (right-branch (cdr mobile))))))

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (* sub-tree sub-tree)))
       tree))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s)))
            (func (lambda (x) (cons (car s )x ))))
        (append rest
                (map  func rest )))))

(subsets '(1 2 3))

