#lang sicp

(define (enumerate-interval a b)
  (cond ((> a b) '())
        (else
         (cons a (enumerate-interval (+ a 1) b)))))

(define (prime? n)
  (let loop ((d 2))
    (cond ((< n (* d d)) #t)
          ((zero? (modulo n d)) #f)
          (else (loop (+ d 1))))))

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

(define (sum-primes a b)
  (define (iter count accum)
    (cond ((> count b) accum)
          ((prime? count)
           (iter (+ count 1) (+ count accum)))
          (else (iter (+ count 1) accum))))
  (iter a 0))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (filter pred lst)
  (cond ((null? lst) '())
        (else (if (pred (car lst)) (cons (car lst) (filter  pred (cdr lst)))
                  (filter pred (cdr lst))))))


(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
        (apply proc (map stream-car argstreams))
        (apply stream-map 
               (cons proc (map stream-cdr argstreams))))))

(define (sum-primess a b)
  (accumulate + 0
              (filter prime?
                      (enumerate-interval a b))))


(define the-empty-stream '())
(define stream-null? null?)

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s ) (- n 1))))

;; (define (stream-map proc s)
;;   (if (stream-null? s)
;;       the-empty-stream
;;       (cons-stream (proc (stream-car s))
;;                    (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x) (newline) (display x))


(define (cons-stream a b) (cons a (delay b)))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))
(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter
                       pred
                       (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

;; (stream-car
;;  (stream-cdr
;;   (stream-filter prime? (stream-enumerate-interval
;;                          10000 1000000))))


(define (show x)
  (display-line x)
  x)

;; (define x 
;;   (stream-map show 
;;               (stream-enumerate-interval 0 10)))


(define sum 0)

(define (accum x) (set! sum (+ x sum)) sum)
;; 1 den 20 ye kadar olan sayilari sumda accumla
;; istenmedigi iicin eldeki sey (1 . <promise>)
(define seq 
  (stream-map accum 
              (stream-enumerate-interval 1 20)))
;; 
(define y (stream-filter even? seq))
(define z 
  (stream-filter (lambda (x) (= (remainder x 5) 0)) seq))

(stream-ref y 7)
(display-stream z)
