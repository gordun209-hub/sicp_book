#lang sicp

(define avg
  (lambda (x y)
    (/ (+ x y) 2)))

(define first
  (lambda (x)
    (car x)))


(define second
  (lambda (x)
    (car (cdr x))))
(define third
  (lambda (x)
    (car  (cdr (cdr x)))))

(define fourth
  (lambda (x)
    (car (cdr (cdr (cdr (cdr)))))))

(define (make-professor name salary)
  (list name salary))

(define (professor-name prof)
  (first prof))


(define (professor-salary prof)
  (second prof))

(define (make-gradstudent name salary)
  (list name salary))

(define (gradstudent-name grad)
  (first grad))

(define (gradstudent-salary grad)
  (second grad))

(define peoples (list (list "la" 2) (list "ls" 3) (list "ss" 4)))

(define (total-cost people-list)
  (cond
    ((null? people-list) 0)
    (else
     (+ (car (cdr (car  people-list))) (total-cost (cdr people-list))))))

(total-cost peoples)


;; 1. assoc (assoc key alist) returns association containing matching key or #f.
;; 2. delassoc (delassoc key alist) returns a new alist with association with
;; matching key removed.

(define alst (list (list 1 2) (list 3 4) (list 5 6)))

(assoc 4 alst)

(assoc 3 alst) ; (list 3 4)

(assoc 5 (cons (list 5 12) alst)) ; (5 12) returns first i thingss


; (del-assoc 5 alst)

(define alst2 (list (list "foo" 17) (list "bar" 42) (list "baz" 54)))

(assoc "foo" alst2)


;Trees

(define (make-node val left right)
  (list "node" val left right))

(define (node? x)
  (and (pair? x) (string=? (car x) "node")))


(define (node-val node)
  (second node))

(define (node-left node)
  (third node))
(define (node-right node)
  (fourth node))

(define (leaf? x)
  (not (node? x)))


(define (tree-contains? tree val)
  (if (leaf? tree)
      (if (= val tree)
          val
          #f)
      (if (< val (node-val tree))
          (tree-contains? (node-left tree) val)
          (tree-contains? (node-right tree) val))))



(define (sum-tree tree)
  (if (leaf? tree)
      tree
      (+ (sum-tree (node-left tree))
         (sum-tree (node-right tree)))))

;; Problem 5:

(define (insert-tree elem tree)
  (if (leaf? tree)
      (if (= elem tree)
          tree
          (if (< elem tree)
              (make-node (avg tree elem) elem tree)
              	      (make-node (avg tree elem) tree elem)))
      (if (< elem (node-val tree))
          (make-node (node-val tree)
                     (insert-tree elem (node-left tree))
                     (node-right tree))
          (make-node (node-val tree)
                     		     (node-left tree)
                                     (insert-tree elem (node-right tree))))))

