#lang sicp



(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (emtpy-queue?) (null? front-ptr))
    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr! item) (set! rear-ptr item))
    (define (front-queue)
      (if (emtpy-queue?)
          (error "FRONT called with an empty queue")
          (car front-ptr)))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((emtpy-queue?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair))
              (else
               (set-cdr! rear-ptr new-pair)
               (set-rear-ptr! new-pair)))))
    (define (delete-queue!)
      (cond ((emtpy-queue?)
             (error "DELETE called with an emtpy queue"))
            (else (set-front-ptr! (cdr front-ptr)))))

    (define (print-queue) front-ptr)

    (define (dispatch m)
      (cond ((eq? m 'empty-queue) emtpy-queue?)
            ((eq? m 'front-queue) front-queue)
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) delete-queue!)
            ((eq? m 'print-queue) print-queue)
            (else (error "undefined operation -- QUEUE" m))))
    dispatch))


