#lang sicp


(define (front-ptr queue) (car queue))

(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item)
  (set-car! queue item))

(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  ;; construct new pair that [item | nil]
  (let ((new-pair (cons item '())))
    ;; check empty if so, set front and rear ptr to new-pair
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          ;; if not, set cdr of the rear-ptr queue to new-pair
          ;; and set-rear-ptr! the queue to new-pair
          (else
           ;; first line does that:
           ;; set pointer to new pair
           ;; and update queue with set-rear-ptr!
           ;; aboo f3n4
           ;; modify final pair the queue to point to the new pair,
           ;; and also set the rear pointer to the new pair
           ;; rear-ptr queue takes last item and set-cdr updates the
           ;; nil to point the new pair
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else (set-front-ptr! queue (cdr (front-ptr queue)))
              queue)))

(define q1 (make-queue))

(insert-queue! q1 'a)
(insert-queue! q1 'b)

(define (print-queue q)
  (define (iter x)
    (if (null? x)
        (newline)
        (begin (display (car x))
               (iter (cdr x)))))
  (iter (front-ptr q)))


(print-queue q1)


(define (make-queuee)
  (let ((front-ptr '()) (rear-ptr '()))
    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr! item) (set! rear-ptr item))

    (define (empty-queue?) (null? front-ptr))
    ;;(define (make-queue) (cons '() '()))
    (define (front-queue)
      (if (empty-queue?)

          (car front-ptr)))

    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair))
              (else
               (set-cdr! rear-ptr new-pair)
               (set-rear-ptr! new-pair)))
        front-ptr))

    (define (delete-queue!)
      (cond ((empty-queue?))

            (else
             (set-front-ptr! (cdr front-ptr))))
      front-ptr)

    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) (empty-queue?))
            ((eq? m 'front-queue) (front-queue))
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) (delete-queue!))
            (else (error "Undefined oepration"))))

    dispatch))

(define qww (make-queue))
(qww 'insert-queue! 'a)
