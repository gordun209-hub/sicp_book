#lang sicp

(define (find-assoc key alist)
  (cond
    ((null? alist) #f)
    ((equal? key (caar alist)) (cadar alist))
    (else (find-assoc key (cdr alist)))))

(define a1 '((x 15) (y 20)))

;(find-assoc 'y a1)

(define (add-assoc key val alist)
  (cons (list key val) alist))

(add-assoc 'z 30 a1)

(define table1-tag 'table1)
(define (make-table1) (cons table1-tag nil))

(define (table1-get tbl key)
  (find-assoc key (cdr tbl)))

(define (table1-put! tbl key val)
  (set-cdr! tbl (add-assoc key val (cdr tbl))))

; Stack Data Abstraction
;constructor:
; (make-stack)

;selectors
; (top stack)

;operations
;(insert stack elt)

;(delete stack)
;(empty-stack? stack)

;; (define (make-stack) nil)

;; (define (empty-stack? stack) (null? stack))

(define (insert stack elt) (cons elt stack))
(define (delete stack)
  (if (empty-stack? stack)
      (error "Stack underflow - delete")
      (cdr stack)))

;; (define (top stack)
;;   (if (empty-stack? stack)
;;       (error "Stack underflow - top")
;;       (car stack)))


(define mystack (make-stack))

(insert mystack 'ab)

(define s (make-stack))

(insert s 'a)
s
(set! s (insert s 'b))
(set! s (insert s 'a))
s ; (a b)
(set! s (delete s))
s ; (a)

(define (insert! stack elt)
  (cond ((not (stack? stack))
         (error "Object not a stack: " stack))
        (else
         (set-cdr! stack (cons elt (cdr stack)))
         stack)))

(define (delete! stack)
  (if (empty-stack? stack)
      (error "stack underflow - delete")
      (set-cdr! stack (cddr stack)))
  stack)
(define (make-stack ) (cons 'stack nil))
(define (stack? stack)
  (and (pair? stack) (eq? 'stack (car stack))))

(define (empty-stack? stack)
  (if (not (stack? stack))
      (error "Object not a stack :" stack)
      (null? (cdr stack))))
(define (top stack)
  (if (empty-stack? stack)
    (error "Stack underflow - top")
    (cadr stack)))

