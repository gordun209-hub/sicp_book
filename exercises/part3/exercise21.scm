#lang sicp
(define front-ptr car)
(define rear-ptr cdr)
(define (set-front-ptr! queue item)
  (set-car! queue item))

(define set-rear-ptr! set-cdr!)

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "error" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "laa" queue))
        (else (set-front-ptr! queue (cdr (front-ptr queue)))
              queue)))
(define (print-queue q)
  (define (iter x)
    (if (null? x)
        (newline)
        (begin (display (car x))
               (iter (cdr x)))))
  (iter (front-ptr q)))
