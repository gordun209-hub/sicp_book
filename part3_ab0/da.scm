#lang sicp


;; A data abstraction consists of:
; constructors
; selectors
; mutators
; operations
; contract

; Primitive data

;(define x 10); => creartes binding for name x
;x ;  returns value bound to a name 10

;(set! x 'foo)

;x ; it is now foo

;; Substitution model --functional programming
; (define x 10)
; (+ x 5) => 15
; ...
; (+ x 5) => 15

; With assignment:
; (define x 10)
; (+ x 15) => 15 - expression value depends on when it is evaluated
; ...
;(set! x 94)
; ...
; (+ x 5) => 99

;; Compound data

; Constructor
;(cons x y) ; creates a new pair p

; selectiors
;(car p) ; first of p
;(cdr p) ; rest of p

;mutations:
; (set-car! p new-x) ; changes car pointer in pair
; (set-cdr! p new-y) ; changes cdr pointer in pair
; Pair, anytype -> undef -- side-effect only!!!

(define a (list 1 2))
(define b a)

(set-car! a 10)
(set-cdr! b (list 20))


;; (define x (list 'a 'b))
;; x
;; (set-cdr! x '((1 2)))
;; x

;; (define x '(3 4))
;; (define y '(1 2))
;;
;; (set-car! x y) ; (1 2 3)
;; x ; ((1 2) 4)
;; y ; (1 2)
;; (set-cdr! y (cdr x)) ; (1 2 4)
;; x
;; y

; Stack data abstraction

; constructors
; (make-stack) ; returns empty Stack

; selectorps
; (top stack) ;returns current top element from stack

; opreations
; (insert stack elt) ; returns a new stack with the element added top of stack

;(delete stack) ; returns new stack with the top element removed

; (empty-stack? stack) ; returns t if no elements f otherwise


;; (define (make-stack) nil)
;;
;; (define (empty-stack? stack) (null? stack))
;; (define (insert stack elt) (cons elt stack))
;;
;; (define (delete stack)
;;   (if (empty-stack? stack)
;;       (error "stack underflow - delete")
;;       (cdr stack)))
;;
;; (define (top stack)
;;   (if (empty-stack? stack)
;;       (error "stack underflow -top")
;;       (car stack)))
;;
;; (define s (make-stack))
;; (insert s 'a)  ; => (a)
;; s ; => ()
;; (set! s (insert s 'b))
;s ; => (b)

(define (make-stack) (cons 'stack nil))

(define (stack? stack)
  (and (pair? stack) (eq? 'stack (car stack))))

(define (empty-stack? stack)
  (if (not (stack? stack))
      (error "objecct not a stack: " stack)
      (null? (cdr stack))))


(define (insert! stack elt)
  (cond ((not (stack? stack))
         (error "Object not a stack : " stack))
        (else
         (set-cdr! stack (cons elt (cdr stack)))
         stack)))

(define (delete! stack)
  (if (empty-stack? stack)
      (error "stack underflow - delete")
      (set-cdr! stack (cddr stack)))
  stack)


(define (top stack)
  (if (empty-stack? stack)
      (error "stack underflow - top")
      (cadr stack)))

;; Queue data abstraction

; constructor
; (make-queue) ; returns empty queue
; accessors
; (front-queue q) ; returns the object at front
; mutators
; (insert-queue q elt) ; returns new queue with elt at the rear of queue
; (delete-queue q ) ; returns a new queue with the item at the front removed
; operations
; (empty-queue? q) ; tests if empty

;; (define (make-queue) nil)
;; (define (empty-queue? q ) (null? q))
;; (define (front-queue q)
;;   (if (empty-queue? q)
;;       (error "Front of empty queue: " q)
;;       (car q)))
;;
;; (define (delete-queue q)
;;   (if (empty-queue? q)
;;       (error "delete of empty queue: " q)
;;       (cdr q)))
;;
;; (define (insert-queue q elt)
;;   (if (empty-queue? q)
;;       (cons elt nil)
;;       (cons (car q) (insert-queue (cdr q) elt))))

; Queue data abstraction (mutating)

; constructor:
; (make-queue)
;accessors:
; (front-queue q)

;mutators:
; (insert-queue! q elt)
; (delete-queue! q)
;operations:
;(queue? q)
;(empty-queue? q)

; Helpers
(define (front-ptr q) (cadr q))
(define (rear-ptr q) (cddr q))
(define (set-front-ptr! q item)
  (set-car! (cdr q) item))
(define (set-rear-ptr! q item)
  (set-cdr! (cdr q) item))

(define (make-queue)
  (cons 'queue (cons nil nil)))
(define que (make-queue))
que
(define (queue? q)
  (and (pair? q) (eq? 'queue (car q))))
(queue? que) ; t

(define (empty-queue? q)
  (if (not (queue? q))
      (error "object not a queue: " q)
      (null? (front-ptr q))))
(empty-queue? que) ; t

(define (front-queue q)
  (if (empty-queue? q)
      (error "front of empty queue: " q)
      (car (front-ptr q))))

;(front-queue que) error

(define (insert-queue! q elt)
  (let ((new-pair (cons elt nil)))
    (cond ((empty-queue? q)
           (set-front-ptr! q new-pair)
           (set-rear-ptr! q new-pair)
           q)
          (else
           (set-cdr! (rear-ptr q) new-pair)
           (set-rear-ptr! q new-pair)
           q))))
(insert-queue! que '2)
que
(insert-queue! que '4)

(define (delete-queue! q)
  (cond ((empty-queue? q)
         (error "delete of empty queue" q ))
        (else
         (set-front-ptr! q
                         (cdr (front-ptr q)))
         q)))
(delete-queue! que)
(delete-queue! que)
