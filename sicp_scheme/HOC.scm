#lang sicp

(define (cube x )(* x x x))



(define (sum-integers-1 a b)
  (if (> a b)
      0
      (+ a (sum-integers-1(+ a 1) b))))

(define (sum-cubes-1 a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes-1 (+ a 1) b))))


(define (pi-sum-1 a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ 2 a))) (pi-sum-1 (+ a 4) b))))


;; term = function to apply
;; a = initial
;; next = next?
;; b = final


(define (inc n) (+ n 1))

(define (sum-cubes a b)
  (sum cube a inc b))

;(sum-cubes 1 10) => 3025

(define identity
  (lambda (x) x))


;;(identity 2)
;(define (identity x) x)


(define (sum-integers a b)
  (sum identity a inc b))

;;(sum-integers 1  10)
;;=> 55


(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

;;(* 8 (pi-sum 1 1000)) ; 3.139592

(define (integral f a b dx)
  (define (add-dx x) (+ x dx)
    (* (sum f (+ a (/ dx 2.0)) add-dx b)))
  dx)

;; (integral cube 0 1 0.01)
;;
;; (integral cube 0 1 0.001)

;; h = (b-a)/n yk=f(a + kh)

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (product term a next b)
  (if (> a b) 1 (* (term a) (product term (next a) next b))))


;; TODO cok onemli TODO TODO TODO
(define (simpson-integral f a b n)
  ;; calc h
  (define h (/ (- b a) n))
  ;; idk whats dhat
  (define (yk k) (f (+ a (* h k))))
  ;; calculating term with conditionals
  (define (simpson-term k)
    ;; if k = 0 or k = n, return 1
    (* (cond ((or (= k 0) (= k n)) 1)
             ;;if k is odd return 4
             ((odd? k) 4)
             ;; else (even) return 2
             (else 2))
       ;; yk?
       (yk k)))
  ;; h/3 * sum simp-term 0 inc n
  (* (/ h 3) (sum simpson-term 0 inc n)))

;; Testing
(simpson-integral cube 0 1 100)
(simpson-integral cube 0 1 1000)

(define (sum-iterative term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))


(sum-iterative (lambda (x) (+ x 1)) 1 inc 20)




(define (product-iterative term a next b)
  (define (product-iter n result)
    (cond ((> n b) result)
          (else
           (product-iter (next n) (* result (term n))))))
  (product-iter a 1))

(product-iterative + 2 inc 5)


(define (accumulate combiner null-value term a next b)
  (cond ((> a b) null-value)
        (else (combiner (term a) (accumulate combiner null-value term (next a)
                                             next b)))))

(accumulate + 0 identity 1 inc 5)

(define (accumulate-iterative combiner null-value term a next b)
  (define (iter a result)
    (cond ((> a b) result)
          (else
           (iter (next a) (combiner result (term a))))))
  (iter a null-value))


(define (filter pred combiner null-value term a next b)
  (cond ((> a b) null-value)
        ((pred (term a)) (combiner (term a) (filter pred combiner null-value
                                                    term (next a) next b)))
        (else (filter pred combiner null-value term (next a) next b))))

(filter (lambda (x) (> x 2)) cons 0 identity 1 inc 10)


