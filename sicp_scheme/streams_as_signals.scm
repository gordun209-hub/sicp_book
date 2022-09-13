#lang sicp

;; helpers ==================================


(define (stream-cdr stream) (force (cdr stream)))
(define (stream-car stream) (car stream))
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))
;; memoize?


(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))
(define (add-streams s1 s2) (stream-map + s1 s2))
;;===========================================



;; kitaptan sekline bak cok hos
;; (define (RC r c dt)
;;   (define (proc i v)
;;     (add-streams (scale-stream i r)
;;                  (integral (scale-stream i (/ 1 c)) v dt)))
;;   proc)


;; 3.74


(define (new-integral delayed-integral initial-value dt)
  (define int
    (cons-stream
     initial-value
     (let ((integrand (force delayed-integral)))
       (add-streams (scale-stream integrand dt) int))))
  int)

;; (define (solve f y0 dt)
;;   (define y (integral dy y0 dt))
;;   (define dy (stream-map f y))
;;   y)

(define (fixed-solve f y0 dt)
  (define y (new-integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

;; (stream-ref (fixed-solve (lambda (y) y)
;;                          1
;;                          0.001)
;;             1000)

;; (define (integral integrand initial-value dt)
;;   (cons-stream
;;    initial-value
;;    (if (stream-null? integrand)
;;        the-empty-stream
;;        (integral (stream-cdr integrand)
;;                  (+ (* dt (stream-car integrand))
;;                     initial-value)
;;                  dt))))




(define (integral delayed-integrand initial-value dt)
  (cons-stream initial-value
               (let ((integrand (force delayed-integrand)))
                 (if (stream-null? integrand)
                     the-empty-stream
                     (integral (delay (stream-cdr integrand))
                               (+ (* dt (stream-car integrand))
                                  initial-value)
                               dt)))))



(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))

(define (stream-withdraw balance amount-stream)
  (cons-stream 
    balance 
    (stream-withdraw (- balance (stream-car amount-stream))
                     (stream-cdr amount-stream))))


