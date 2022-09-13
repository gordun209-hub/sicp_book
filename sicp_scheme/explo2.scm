#lang sicp

;; selectors for streams
(define square
  (lambda (x) (* x x)))
(define (stream-cdr stream) (force (cdr stream)))
(define (stream-car stream) (car stream))
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-take s n)
  (cond ((zero? n) '())
        (else (cons (stream-car s) (stream-take (stream-cdr s) (- n 1))))))

;; ============

(define (stream-enumerate-interval low high)
  ;; if done return empty stream
  (if (> low high)
      the-empty-stream
      ;; else cons-stream low and
      (cons-stream
       low
       ;; rest of the stream
       (stream-enumerate-interval (+ low 1) high))))
;; (stream-enumerate-interval 1 4)



(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter
                       pred
                       (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))


(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))


(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-line x) (newline) (display x))
(define (display-stream s)
  (stream-for-each display-line s))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))

(define (add-streams s1 s2) (stream-map + s1 s2))
(define (partial-sums s)
  (add-streams s (cons-stream 0 (partial-sums s))))

;; =================================================
(define (average x y) (/ (+ x y) 2))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

;;(stream-ref (integers-starting-from 1) 10)
(define integers (integers-starting-from 1))
(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream
     1.0
     (stream-map (lambda (guess) (sqrt-improve guess x))
                 guesses)))
  guesses)

;; (stream-take (sqrt-stream 2) 5)
;; (sqrt-improve 1.0 2)
;; (sqrt-improve 1.5 2)
;; (sqrt-improve 1.41666 2)
;; 1.0 1.5 1.4166 14

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))
;; (stream-take (pi-summands 1) 10)

;; SEQUENCE ACCELERATOR???

;; accelerated sequence

;;         (Sn+1 - Sn)^2
;;Sn+1 -  -----------------
;;        Sn-1 - 2Sn + Sn+1

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

;; (stream-take (euler-transform pi-stream) 20)

;; accelerate more

(define (make-tableau transform s)
  (cons-stream s (make-tableau transform (transform s))))

;; has the form
;; s00 s01 s02 s03 s04 ...
;;     s10 s11 s12 s13 ...
;;         s20 s21 s22 ...
;;             ...

;; we form a sequence by taking the first term in each row of the tableau

(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))

;; (stream-take (accelerated-sequence euler-transform pi-stream) 10)


;; (define (stream-limit stream tolerance)
;;         (if (< (abs (- (stream-ref stream 1) (stream-ref stream 0))) tolerance)
;;                 (stream-ref stream 1)
;;                 (stream-limit (stream-cdr stream) tolerance)))



(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2-summands (+ n 1)))))
(define ln2-stream
  (partial-sums (ln2-summands 1)))


;; Infinite streams of pairs


;; generalize prime-sum-pairs

;; (stream-filter
;;   (lambda (pair) (prime? (+ (car pair) (cadr pair))))
;;   int-pairs)

;; suppose we have two streams
;; S = (Si) and T = (Tj) imagine the infinite rectangular array
;; (S0,T0) (S0,T1) (S0,T2) ...
;; (S1,T0) (S1,T1) (S1,T2) ...
;; (S2,T0) (S2,T1) (S2,T2) ...
;; ...

;; we wish to generate a stream that contains all pairs in the array
;; that lie on or above the diagonal, i.e., the pairs

;; (S0,T0) (S0,T1) (S0,T2) ...
;;         (S1,T1) (S1,T2) ...
;;                 (S2,T2) ...
;;                         ...
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))
;; (define (pairs s t)
;;   (interleave
;;    (stream-map (lambda (x) (list (stream-car s) x))
;;                t)
;;    (pairs (stream-cdr s) (stream-cdr t))))
;; (define (pairs s t)
;;   (cons-stream
;;    (list (stream-car s) (stream-car t))
;;    (interleave
;;     (stream-map (lambda (x) (list (stream-car s) x))
;;                 (stream-cdr t))
;;     (pairs (stream-cdr s) t))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))
;; (stream-take (stream-filter
;;               (lambda (x) (eq? (car x) 1))
;;               (pairs integers integers) )100)

;; (stream-take (pairs integers integers) 50)





;; (define (triples s t u)
;;         (cons-stream (list
;;                         (stream-car s)
;;                         (stream-car t)
;;                         (stream-car u))
;;                 (interleave
;;                         (stream-map (lambda (x) (cons (stream-car s) x))
;;                                 (stream-cdr (pairs t u)))
;;                         (triples (stream-cdr s)
;;                                 (stream-cdr t)
;;                                 (stream-cdr u)))))
;;
;; (define (phythagorean-numbers)
;;         (define (square x) (* x x))
;;         (define numbers (triples integers integers integers))
;;         (stream-filter (lambda (x)
;;                                                 (= (square (caddr x))
;;                                                 (+ (square (car x)) (square (cadr x)))))
;;                                 numbers))
;; (stream-take (phythagorean-numbers) 20)


;; Helper function
(define (stream->list stream n) ;; n is number of elements to add to list
  (if (= n 0)
      '()
      (cons (stream-car stream) (stream->list (stream-cdr stream) (- n 1)))))

;; Exercise 3.70
;; Part A
(define (sum-weight p)
  (+ (car p) (cadr p)))

(define (merge-weighted s1 s2 proc)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (let ((w1 (proc s1car))
                 (w2 (proc s2car)))
             (if (< w1 w2)
                 (cons-stream s1car (merge-weighted (stream-cdr s1) s2 proc))
                 (cons-stream s2car (merge-weighted s1 (stream-cdr s2) proc))))))))

(define (weighted-pairs s1 s2 proc)
  (cons-stream
   (list (stream-car s1) (stream-car s2))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s1) x)) (stream-cdr s2))
    (weighted-pairs (stream-cdr s1) (stream-cdr s2) proc)
    proc)))

;; test
;; (define ordered-pairs (weighted-pairs integers integers sum-weight))
;; (map sum-weight (stream->list ordered-pairs 50))
;; (2 3 4 4 5 5 6 6 6 7 7 7 8 8 8 8 9 ... )

;; Part B
(define (weight p)
  (+ (* 2 (car p))
     (* 3 (cadr p))
     (* 5 (car p) (cadr p))))

(define (not-divisible? dividend divisor)
  (not (= 0 (remainder dividend divisor))))

(define (compound-not-divisible? dividend x y z)
  (and (not-divisible? dividend x)
       (not-divisible? dividend y)
       (not-divisible? dividend z)))

(define filtered-integers
  (stream-filter (lambda (x) (compound-not-divisible? x 2 3 5)) integers))

;; test
(define ordered-conditional-pairs
  (weighted-pairs filtered-integers filtered-integers weight))

;; (map weight (stream->list ordered-conditional-pairs 50))
;; (10 58 90 106 138 154 186 234 250 280 298 330 346...)



(define (Ramanujan s)
  (define (stream-cadr s) (stream-car (stream-cdr s)))
  (define (stream-cddr s) (stream-cdr (stream-cdr s)))
  (let ((scar (stream-car s))
        (scadr (stream-cadr s)))
    (if (= (sum-triple scar) (sum-triple scadr))
        (cons-stream (list (sum-triple scar) scar scadr)
                     (Ramanujan (stream-cddr s)))
        (Ramanujan (stream-cdr s)))))

(define (triple x) (* x x x))
(define (sum-triple x) (+ (triple (car x)) (triple (cadr x))))

(define Ramanujan-numbers
  (Ramanujan (weighted-pairs integers integers sum-triple)))

(4104 (2 16) (9 15))
(13832 (2 24) (18 20))
(20683 (10 27) (19 24))
(32832 (4 32) (18 30))
(39312 (2 34) (15 33))


;; ===========================================

;; TODO?
 ;; Exercise 3.72 
 (define (sum-of-squares p) 
   (+ (square (car p)) (square (cadr p)))) 
  
 (define ordered-pairs 
   (weighted-pairs integers integers sum-of-squares)) 
  
 (define (equiv-sum-squares-stream s) 
   (let ((next-1 (stream-cdr s)) 
         (next-2 (stream-cdr (stream-cdr s)))) 
     (let ((p1 (stream-car s)) 
           (p2 (stream-car next-1)) 
           (p3 (stream-car next-2))) 
       (let ((x1 (sum-of-squares p1)) 
             (x2 (sum-of-squares p2)) 
             (x3 (sum-of-squares p3))) 
         (if (= x1 x2 x3) 
             (cons-stream 
              (list x1 p1 p2 p3) 
              (equiv-sum-squares-stream (stream-cdr next-2))) 
             (equiv-sum-squares-stream next-1)))))) 
  
 (stream->list (equiv-sum-squares-stream ordered-pairs) 5) 
 ;; ((325 (10 15) (6 17) (1 18)) 
 ;; (425 (13 16) (8 19) (5 20)) 
 ;; (650 (17 19) (11 23) (5 25)) 
 ;; (725 (14 23) (10 25) (7 26)) 
 ;; (845 (19 22) (13 26) (2 29)))
