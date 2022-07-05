#lang sicp


(define add1
  (lambda (n)
    (+ n 1)))


(define sub1
  (lambda (n)
    (- n 1)))

(define add
  (lambda (n m)
    (cond
      ((zero? m ) n)
      (else  (add (add1 n) (sub1 m))))))

;; (add 5 2 )

(define sub
  (lambda (n m)
    (cond
      ((zero? m ) n)
      (else
       (sub
        (sub1 n)
        (sub1 m))))))

;; (sub 5 2)

(define addtup
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else
       (+ (car lat) (addtup (cdr lat)))))))
;; (addtup '(1 2 3 4)) => 10

(define multup
  (lambda (lat)
    (cond
      ((null? lat) 1)
      (else
       (* (car lat) (multup (cdr lat)))))))

(multup '(1 2 3 4)) ; => 24

(define mult
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (+ n (mult n (sub1 m)))))))

;; (mult 5 2) ; ==> 10

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1)
       tup2)
      ((null? tup2)
       tup1)
      (else
       (cons (+ (car tup1) (car tup2))
             (tup+
              (cdr tup1) (cdr tup2)))))))

;(tup+ '(1 2 3) '(3 4 5 2)) => (4 6 8 2)

(define isBiggerThan
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else
       (isBiggerThan (sub1 n) (sub1 m))))))

;(isBiggerThan 5 2) => t
(define isEqual?
  (lambda (n m)
    (cond
      ((and (not (isBiggerThan n m)) (not (isBiggerThan m n  )))
       #t)
      (else #f))))
(isEqual? 6 6)


(define eqq?
  (lambda (n m)
    (cond
      ((zero? m) (zero? n))
      ((zero? n) #f)
      (else (eqq? (sub1 n) (sub1 m))))))

(define division
  (lambda (n m)
    (cond
      ((< n m ) 0)
      (else (add1 (division (- n m) m))))))

;; (division 8 2)

(define length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else
       (add1 (length (cdr lat)))))))

;(length '(qwie qoijwe qiwoje)) easy peasy lemon squjes

(define pick
  (lambda (n lat)
    (cond
      ((zero? n) (quote ()))
      (else
       (cons (car lat) (pick (sub1 n) (cdr lat)))))))

;; (pick 2 '(ww qq ee rr))

(define rempick
  (lambda (n lat)
    (cond
      ((zero? n ) (cdr lat))
      (else
       (cons (car lat) (rempick (sub1 n) (cdr lat)))))))
(rempick 2 '(la mq sa o2))

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      ((number? (car lat)) (no-nums (cdr lat)))
      (else
       (cons (car lat) (no-nums (cdr lat)))))))

(no-nums '(m w l 2 3 k 2 l))

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      ((number? (car lat) (cons (car lat) (all-nums (cdr lat)))))
      ((not (number? (car lat) (all-nums (cdr lat))))))))

(all-nums '( w e q 2  3 4 s))

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      (else
       (cond
         ((eq? (car lat ) a)
          (add1 (occur a (cdr lat))))
         (else (occur a (cdr lat))))))))


(define one?
  (lambda (n)
    (= n 1)))


(define rempick-with-one
  (lambda (n tat)
    (cond
      (( one? n) (cdr tat))
      (else (cons (car tat)
                  (rempick (sub1 n)
                           (cdr tat)))))))



