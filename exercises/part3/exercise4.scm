#lang sicp


(define (make-account money password)
  (let ((wrong-pw-count 0))
    (define (withdraw amount)
      (begin (set! money (- money amount))
             (set! wrong-pw-count 0)
             money))
    (define (deposit amount)
      (begin (set! money (+ money amount))
             (set! wrong-pw-count 0)
             money)
      "Insufficient funds")
    (define (dispatch pw proc)
      (if (> wrong-pw-count 7)
          (error "failed")
          (cond ((not (eq? pw password))
                 (begin (set! wrong-pw-count (+ wrong-pw-count 1))
                        (error "laa")))
                ((eq? proc 'withdraw) withdraw)
                ((eq? proc 'deposit) deposit))))


    dispatch))

(define A1 (make-account 100 'ww))

((A1 'ww 'withdraw) 20)
((A1 'ww 'withdraw) 20)
((A1 'ww 'withdraw) 20)

