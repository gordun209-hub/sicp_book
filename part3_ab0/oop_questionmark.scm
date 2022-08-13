#lang sicp

;; assume we have a procedure rand-update

; x2 = (rand-update x1)
; x3 = (rand-update x2)

;; (define rand
;;   (let ((x random-init))
;;     (lambda ()
;;       (set! x (rand-update x))
;;       x)))
;;
;; (define (estimate-pi trials)
;;   (sqrt (/ 6 (monte-carlo trials cesaro-test))))
;;
;; (define (cesaro-test)
;;   (= (gcd (rand) (rand)) 1))
;;
;; (define (monte-carlo trials experiment)
;;   (define (iter trials-remaining trials-passed)
;;     (cond ((= trials-remaining 0)
;;            (/ trials-passed trials))
;;           ((experiment)
;;            (iter (- trials-remaining 1) (+ trials-passed 1)))
;;           (else
;;            (iter (- trials-remaining 1) trials-passed))))
;;   (iter trials 0))

;; Pitfalls of imperative programming

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))


(define (factorial-2 n)
  (let ((product 1)
        (counter 1))
    (define (iter)
      (if (> counter n)
          product
          (begin (set! product (* counter product))
                 (set! counter (+ counter 1))
                 (iter))))
    (iter)))



(define (make-account balance password)
  (define incorrect-count 0)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "insufficient"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (issue-warning)
    (if (> incorrect-count 7)
        (error "the cops are on their way")
        (error (- 7 incorrect-count) 'more 'attempts)))
  (define (auth-layer pw . m)
    (cond ((null? m) (eq? pw password))
          ((eq? pw password) (dispatch (car m)))
          (else (begin (set! incorrect-count (+ incorrect-count 1))
                       (issue-warning)))))
  (define (dispatch m)
    (set! incorrect-count 0)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request" m))))
  auth-layer)

(define (make-joint acc pw-prev pw-next)
  (define (dispatch pw . m)
    (if (null? m)
        (eq? pw pw-next)
        (acc (if (eq? pw pw-next) pw-prev pw-next) (car m))))
  (if (acc pw-prev)
      dispatch
      (error "Incorrect password to original account" pw-prev)))

(define peter-acc (make-account 100 'open-sesame))
(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))
(define pan-acc (make-joint paul-acc 'rosebud 'vvv))

;; tests
((pan-acc 'vvv 'deposit) 100)  ;; 200
((peter-acc 'open-sesame 'deposit) 100) ;; 300
((paul-acc 'rosebud 'deposit) 100) ;; 400
((peter-acc 'rosebud 'deposit) 100)  ;; error as intended

(define (square x)
  (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))
