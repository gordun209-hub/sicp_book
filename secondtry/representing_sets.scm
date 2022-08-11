#lang racket/base
(require racket/trace)
;; Representing sets

;;For unordered sets
(define (element-of-set-unordered? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #f)
        (else (element-of-set-unordered? x (cdr set)))))

;; add new element to set if not already in
(define (adjoint-set-unordered x set)
  (if (element-of-set-unordered? x set)
      set
      (cons x set)))


(define (intersection-set-unordered set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set-unordered? (car set1) set2)
         (cons (car set1)
               (intersection-set-unordered (cdr set1) set2)))
        (else (intersection-set-unordered (cdr set1) set2))))
;(intersection-set-unordered '(5  2 3 1) '(2 3 9 8))

;; Note
;; they all use element-of-set procedure so all grows as O(n)
;; except intersection-set and union-set, they grow O(n2)

(define (union-set-unordered-2 set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set-unordered? (car set1) set2)
         (union-set-unordered-2 (cdr set1) set2))
        (else (cons (car set1) (union-set-unordered-2 (cdr set1) set2)))))
;(union-set-unordered-2 '(5 2 3 1) '(2 3 9 8))


(define (union-set-unordered-3 s1 s2)
  (if (null? s1)
      s2
      (let
          ((e (car s1)))
        (union-set-unordered-3
         (cdr s1)
         ;; eger ki s1 in ilk elemani  s2 nin de elemaniysa
         ;; s2 return et yoksa ilk elemanini s2 ile consla
         (if (element-of-set-unordered? e s2) s2 (cons e s2))))))
;(union-set-unordered-3 '(5 2 3 1) '(2 3 9 8))

;; recursive process
(define (union-set-unordered A B)
  (cond ((null? B) A)
        ;; eger ki b nin ilk elemani a nin da elemaniysa
        ((element-of-set-unordered? (car B) A)
         ;; recur union set with A and rest of B
         (union-set-unordered A (cdr B)))
        ;; eger oyle deilse
        ;; B nin ilk elemanini union-set A ve rest of B ile consla
        (else (cons (car B)
                    (union-set-unordered A (cdr B))))))


;; iterative process
(define (union-set-iter-unordered A B)
  (define (iter A B U)
    (cond ((or (null? B) (null? A))
           U)
          ((element-of-set-unordered? (car B) A)
           (iter A (cdr B) U))
          (else (iter A (cdr B) (cons (car B) U)))))
  (iter A B A))

;; Sets as ordered lists

(define (element-of-set-ordered? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #t)
        (else (element-of-set-ordered? x (cdr set)))))


;;(element-of-set? 3 '(2 3 4 5))

(define (intersection-set-ordered set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      ;; get first elements
      (let ((x1 (car set1)) (x2 (car set2)))
        ;; esit ise ayni fonksiyonu geri kalani ile consla
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set-ordered (cdr set1)
                                               (cdr set2))))
              ;; kucukse set1 in cdrsi ile recurla
              ((< x1 x2)
               (intersection-set-ordered (cdr set1) set2))
              ;; ayni mantik
              ((< x2 x1)
               (intersection-set-ordered set1 (cdr set2)))))))

(define (adjoin-set-ordered x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set-ordered x (cdr set))))))

(define (adjoint-set-ordered x set)
  (cond
    ((null? set) (list x))
    ((= x (car set)) set)
    ((< x (car set)) (cons x set))
    (else
     (cons (car set) (adjoint-set-ordered x (cdr set))))))



(define (union-set-ordered set1 set2)
  ;; eger ki set1 null ise set2 dondur
  (cond  ((null? set1) set2)
         ;; eger ki set2 null ise set1 dondur
         ((null? set2) set1)
         (else
          ;; ikisi de deil ise eger
          ;; ilk elemanlarini al 2 setin de
          (let ((x1 (car set1))
                (x2 (car set2)))
            ;; eger esit ise ilk elemanlar,
            ;; (cons biri + fonksiyonu cdr ile recurle)
            (cond ((= x1 x2) (cons x1 (union-set-ordered (cdr set1) (cdr set2))))
                  ;; eger x1 x2 den kucukse gne cons ama cdr of set1
                  ((< x1 x2) (cons x1 (union-set-ordered (cdr set1) set2)))
                  ;; usttekinin aynisi ama vica versa
                  (else (cons x2 (union-set-ordered set1 (cdr set2)))))))))

(define (union-set-1-ordered set1 set2)
  (cond  ((null? set1) set2)
         ((null? set2) set1)
         ((= (car set1) (car set2))
          (cons (car set1) (union-set-1-ordered (cdr set1) (cdr set2))))
         ((< (car set1) (car set2))
          (cons (car set1) (union-set-1-ordered (cdr set1) set2)))
         (else
          (cons (car set2) (union-set-1-ordered set1 (cdr set2))))))

(define (union-set-2-ordered set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1))
               (x2 (car set2)))
           (cons (min x1 x2)
                 (union-set-2-ordered (if (> x1 x2)
                                          set1
                                          (cdr set1))
                                      (if (> x2 x1)
                                          set2
                                          (cdr set2))))))))

;; Sets as binary trees

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))
(define mytree (make-tree 8 (make-tree 7 6 5) (make-tree 4 3 (make-tree 2 1 0))))

;(left-branch mytree)
;(right-branch (right-branch mytree))
;; now we can write element-of-set?

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))



(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))



(define 0-to-7 (adjoin-set 7
                           (adjoin-set 6
                                       (adjoin-set 5
                                                   (adjoin-set 4
                                                               (adjoin-set 3
                                                                           (adjoin-set 2
                                                                                       (adjoin-set 1
                                                                                                   (adjoin-set 0
                                                                                                               '())))))))))
;(right-branch (right-branch 0-to-7))
;(right-branch (right-branch (right-branch 0-to-7)))

(define fig2-16-1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
(define fig2-16-2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
(define fig2-16-3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(tree->list-1 fig2-16-1)

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(trace tree->list-1)
(trace tree->list-2)
;(tree->list-1 fig2-16-1)

;(tree->list-2 fig2-16-1)
(define fig (make-tree 7 (make-tree 3 1 5) (make-tree 9 '() 11)))
;(tree->list-1 fig)
;(tree->list-2 fig)
