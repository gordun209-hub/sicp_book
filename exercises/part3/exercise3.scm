#lang sicp

(define (make-account money password)
  (define (withdraw amount)
    (begin (set! money (- money amount))money))

  (define (deposit amount)
    (begin (set! money (+ money amount))money)
    "Insufficient funds")
  (define (dispatch pw proc)
    (if (eq? pw password)
        (cond ((eq? proc 'deposit) deposit)
              ((eq? proc 'withdraw) withdraw))
        ((error "wrong password" pw))))
  dispatch)



(define acc (make-account 100 'secret-password))


((acc 'secret-password 'withdraw) 40) ;; 60

((acc 'some-other-password 'deposit) 50) ;;Incorrect password
