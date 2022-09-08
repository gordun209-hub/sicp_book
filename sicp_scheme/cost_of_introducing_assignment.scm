#lang sicp




(define (make-decrementer balance)
  (lambda (amount)
    (- balance amount)))

(define D (make-decrementer 25))

(D 20)
;; [=> 5
(D 10)
;; [=> 15

;; we can use substitution model to explain how that works
;; observe.
;; ((make-decrementer 25) 20)

;; first simplify the operator of the combination

;;((lambda (amount) (- 25 amount)) 20)

;; now we apply the operator
;; (- 25 20)

;; answer is 5
;; Observe again

(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))
(define W (make-simplified-withdraw 25))

(W 20)
;; [=> 5
(W 10)
;; [=> -5


;; ((make-simplified-withdraw 25) 20)

;; first simplify the operator by substituting 25 for balance in the
;; body of make-simplified-withdraw
;; that reduces to
;;((lambda (amount) (set! balance (- 25 amount)) 25) 20)

;; now apply
;; (set! balance (- 25 20)) 25

;; if using substituting model, we need to distinguish the first occurance
;; of balance (before the effect of the set!) and second occurance
;; (after the effect of the set!) and substitution model cant do this




(define (factorial-functional n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product) (+ counter 1))))
  (iter 1 1))

(define (factorial-imperative n)
  (let ((product 1)
        (counter 1))
    (define (iter)
      (if (> counter n)
          product
          (begin (set! product (* counter product))
                 (set! counter (+ counter 1))
                 (iter))))
    (iter)))


(define (factorial-imperative-buggy n)
  (let ((product 1)
        (counter 1))
    (define (iter)
      (if (> counter n)
          product
          (begin (set! counter (+ counter 1))
                 (set! product (* counter product))
                 (iter))))
    (iter)))

(factorial-imperative-buggy 5) ;; 720
(factorial-imperative 5)       ;; 120
(factorial-functional 5)      ;; 120

;; ;; Exc 3.7
;; (define (make-account balance password)
;;
;;   (define incorrect-count 0)
;;
;;   (define (withdraw amount)
;;     (if (>= balance amount)
;;         (begin (set! balance (- balance amount))
;;                balance)
;;         "insufficient"))
;;   (define (deposit amount)
;;     (set! balance (+ balance amount))
;;     balance)
;;
;;   (define (issue-warning)
;;     (if (> incorrect-count 7)
;;         (error "the cops are on their way")
;;         (error (- 7 incorrect-count) 'more 'attempts)))
;;
;;   (define (auth-layer pw . m)
;;     (cond ((null? m) (eq? pw password))
;;           ((eq? pw password) (dispatch (car m)))
;;           (else (begin (set! incorrect-count (+ incorrect-count 1))
;;                        (issue-warning)))))
;;
;;   (define (dispatch m)
;;     (set! incorrect-count 0)
;;     (cond ((eq? m 'withdraw) withdraw)
;;           ((eq? m 'deposit) deposit)
;;           (else (error "Unknown request" m))))
;;   auth-layer)
;;
;; (define (make-joint acc pw-prev pw-next)
;;   (define (dispatch pw . m)
;;     (if (null? m)
;;         (eq? pw pw-next)
;;         (acc (if (eq? pw pw-next) pw-prev pw-next) (car m))))
;;   (if (acc pw-prev)
;;       dispatch
;;       (error "Incorrect password to original account" pw-prev)))
;;
;; (define peter-acc (make-account 100 'open-sesame))
;; (define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))
;; (define pan-acc (make-joint paul-acc 'rosebud 'vvv))
;;
;; ;; tests
;; ((pan-acc 'vvv 'deposit) 100)  ;; 200
;; ((peter-acc 'open-sesame 'deposit) 100) ;; 300
;; ((paul-acc 'rosebud 'deposit) 100) ;; 400
;; ((peter-acc 'rosebud 'deposit) 100  ;; error as intended


(define (make-account balance password)

  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
        "Insufficient funds"))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (dispatch pwd m)
    (if (eq? m 'auth)
        (lambda () (eq? pwd password))
        (if (eq? pwd password)
            (cond ((eq? m 'withdraw) withdraw)
                  ((eq? m 'deposit) deposit)
                  (else (error "Unknown request -- MAKE-ACCOUNT" m)))
            (error "Incorrect password"))))

  dispatch)

(define (make-joint account orig-pwd joint-pwd)

  (define (withdraw amount) ((account orig-pwd 'withdraw) amount))
  (define (deposit amount) ((account orig-pwd 'deposit) amount))

  (define (dispatch pwd m)
    (if (eq? m 'auth)
        (lambda () (eq? pwd joint-pwd))
        (if (eq? pwd joint-pwd)
            (cond ((eq? m 'withdraw) withdraw)
                  ((eq? m 'deposit) deposit)
                  (else (error "Unknown request -- MAKE-ACCOUNT" m)))
            (error "Incorrect password"))))

  (if ((account orig-pwd 'auth))
      dispatch
      (error "Incorrect original password of target account")))

; test
(define peter-acc (make-account 100 'open-sesame))
(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud)) ;joint account
(define woofy-acc (make-joint paul-acc 'rosebud 'longleg)) ; joint joint account

((peter-acc 'open-sesame 'withdraw) 50) ;50
((paul-acc 'rosebud 'withdraw) 10) ;40
((peter-acc 'open-sesame 'deposit) 0) ;40
((woofy-acc 'longleg 'withdraw) 33) ;7
((peter-acc 'open-sesame 'deposit) 0) ;7

;; env model of procedure application:

;; A procedure object is applied to a set of arguments by constructing a frame
;; binding the formal parameters of the procedure to the arguments of the call,
;; and then evaluating the body of the procedure in the context of the new
;; environment constructed. The new frame has as its enclosing environment the
;; environment part of the procedure object being applied.

;; A procedure is created byt evaluating a lambda expression relative to a
;; given environment. The resulting procedure object is a paor of consisting
;; of the text of the lambda exp and a pointer to the environment in which
;; the procedure was created.

(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))


(define (factorial n)
  (if (= n 1) 1 (* n (factorial (- n 1)))))

(define (factorial-iter n) (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))
