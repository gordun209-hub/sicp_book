#lang sicp

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
