#lang sicp
;; get first element simply car
(define (stream-car stream) (car stream))
(define (square x) (* x x))

;; set rest of element with force
(define (stream-cdr stream) (force (cdr stream)))

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))
(define (delay exp) (memo-proc (lambda () exp)))
;; Stream utilities
;; enumerate
(define (stream-enumerate-interval low high)
  ;; if done return empty stream
  (if (> low high)
      the-empty-stream
      ;; else cons-stream low and
      (cons-stream
       low
       ;; rest of the stream
       (stream-enumerate-interval (+ low 1) high))))


(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter
                       pred
                       (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

;;(define evens (stream-filter even? (stream-enumerate-interval 1 100)))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))


(define (prime? n)
  (let loop ((d 2))
    (cond ((< n (* d d)) #t)
          ((zero? (modulo n d)) #f)
          (else (loop (+ d 1))))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))



(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x) (newline) (display x))


(define (show x)
  (display-line x)
  x)

;; (define x
;;   (stream-map show (stream-enumerate-interval 0 10)))
;; hatirlio :DD
;; (stream-ref x 5)
;;
;;
;; (stream-ref x 7)
;; (stream-ref x 7)

;; TODO blmm important onemli
;; (define sum 0)
;;
;; (define (accum x) (set! sum (+ x sum)) sum)
;; (display "after accum = ")
;; sum
;; (newline)
;; ;; on this line because of 1 is created already, sum is 1
;; (define seq
;;   (stream-map accum
;;               (stream-enumerate-interval 1 20)))
;; (display "after seq = ")
;; sum
;; (newline)
;; (define y (stream-filter even? seq))
;; (display "after y = ")
;; sum
;; (newline)
;; (define z (stream-filter (lambda (x) (= (remainder x 5) 0)) seq))
;;
;; (display "after z = ")
;; sum
;; (newline)
;; (stream-ref y 7)
;;
;; (display "after stream-ref = ")
;; sum
;; (newline)
;; (display-stream z)
;;
;; (display "after display-stream = ")
;; sum
;; (newline)
;;


;; Infiinite streams

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(stream-ref (integers-starting-from 1) 10)
(define integers (integers-starting-from 1))

(define (divisible? x y) (= (remainder x y) 0))

(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7))) integers))


(define (fibgen a b) (cons-stream a (fibgen b (+ a b))))

(define fibs (fibgen 0 1))

;; TODO anla
(define (sieve stream)
  (cons-stream
   (stream-car stream)
   ;; filter if divisible by current x
   (sieve (stream-filter
           (lambda (x)
             ;; stream-car is next element
             ;; her recurda current eleemente bolunenleri ele
             ;; bolece ilerledikce primelar kalir ab00
             ;; IMPORTANT
             (not (divisible? x (stream-car stream))))
           ;; recur with rest of integers
           (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))

(define ones (cons-stream 1 ones))

(define (add-streams s1 s2) (stream-map + s1 s2))
(define (mul-streams s1 s2) (stream-map * s1 s2))
(define factorials
  (cons-stream 1 (mul-streams integers factorials)))

(stream-ref factorials 4)
;; TODO?
(define (partial-sums s)
  (add-streams s (cons-stream 0 (partial-sums s))))

(stream-ref (partial-sums integers) 10)

(define integerss
  (cons-stream 1 (add-streams ones ones)))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))

(define double (cons-stream 1 (scale-stream double 2)))

(define primess
  (cons-stream 2 (stream-filter prime? (integers-starting-from 3))))

(define (prime?? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n ) true)
          ((divisible? n (stream-car ps)) false)
          (else (iter (stream-cdr ps)))))
  (iter primess))

(define twos (cons-stream 2 twos))
(define s (cons-stream 1 (add-streams s s)))
(stream-ref s 2)

;; (cons-stream 1 (stream-map + (cons-stream 1 (add-streams


;; exc 3.56

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car
                               (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream
                   s2car
                   (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream
                   s1car
                   (merge (stream-cdr s1)
                          (stream-cdr s2)))))))))

;; easymis mq
(define S (cons-stream 1 (merge (merge (scale-stream S 2) (scale-stream S 3))
                                (scale-stream S 5))))

;;(stream-ref S 20)


;; exc 5.58
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

;;(display-stream (expand 1 7 10))
(display "laaa")
;; (newline)
;; (stream-ref (expand 1 7 10)0)
;; (stream-ref (expand 1 7 10)1)
;; (stream-ref (expand 1 7 10)2)
;; (stream-ref (expand 1 7 10)3)
;; (stream-ref (expand 1 7 10)4)
;; (stream-ref (expand 1 7 10)5)
;; (stream-ref (expand 1 7 10)6)
;; (stream-ref (expand 1 7 10)7)
;; (stream-ref (expand 1 7 10)8)
;; (stream-ref (expand 1 7 10)9)
;; (stream-ref (expand 1 7 10)10)
;; (stream-ref (expand 3 7 10)0)
;; ;  4 2 8 5 7 1 4 2 8  5 7 1
;;
;;
;; (stream-ref (expand 3 8 10)0)
;; (stream-ref (expand 3 8 10)1)
;; (stream-ref (expand 3 8 10)2)
;; (stream-ref (expand 3 8 10)3)
;; (stream-ref (expand 3 8 10)4)
;; (stream-ref (expand 3 8 10)5)

;; exc 3.59

;; TODO do rest
