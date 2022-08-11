#lang sicp

(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
;;(withdraw 20)
;;(withdraw 30)

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

;;(new-withdraw 50)
;;(new-withdraw 30)


(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

(define W1 (make-withdraw 100))
(define W2 (make-withdraw 100))

;(W1 50); 50
;(W2 70); 30

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

(define (make-account-2 balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Not enough money"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pass m)
    (if (not (eq? pass password))
        (lambda (amount) "Wrong password")
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown call -- MAKE-ACCOUNT"
                           m)))))
  dispatch)


(define (make-account-3 balance password)
  (define (withdraw amount)
    (if (>= balance amount) (begin (set! balance (- balance amount)) balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount)) balance)
  (define (dispatch p m)
    (cond ((not (eq? p password)) (lambda () "Incorrect password"))
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT" m))))
  dispatch)


(define (make-account-4 balance initial-password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (get-method m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown method" m))))

  (define (valid-password? p)
    (eq? p initial-password))

  (define (dispatch password method)
    (if (valid-password? password)
        (get-method method)
        (lambda _ "Incorrect password")))

  dispatch)

(define (make-accumulator acc)
  (lambda (x)
    (set! acc (+ acc x))
    acc))

;; WITH COPS
(define (make-account-5 balance secret-password)
  (define (call-the-cops) "The cops have been caled!")
  (define attempts (make-accumulator 0))
  (define (attempts-made) (attempts 0))
  (define (reset-attempts) (attempts (- (attempts 0))))

  (define (is-correct-password? password)
    (cond ((equal? secret-password password)
           (reset-attempts) true)
          (else (attempts 1)
                false)))

  (define (withdraw amount)
    (cond ((>= balance amount)
           (set! balance (- balance amount))
           balance)
          (else
           "Insufficient funds")))


  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (dispatch password m)
    (cond ((not (is-correct-password? password))
           (if (> (attempts-made) 7)
               (lambda (x) (call-the-cops))
               (lambda (x) "Incorrect password")))
          ((equal? m 'withdraw) withdraw)
          ((equal? m 'deposit) deposit)
          (else (error "Unknown request: MAKE-ACCOUNT" m))))
  dispatch)


(define (make-account-6 balance password)
  (define bad-password-count 0)
  (define (correct-password? p)
    (if (eq? p password)
        (set! bad-password-count 0)
        (begin
          (if (= bad-password-count 7)
              (display "call-the-cops")
              (set! bad-password-count (+ bad-password-count 1)))
          (display "Incorrect password")
          #f)))
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch p m)
    (if (correct-password? p)
        (cond
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT" m)))
        (lambda (x) (display ""))))
  dispatch)





(define acc (make-account-3 100 '123))
((acc '123 'withdraw) 40)

(define (make-accumulator-3 initial-value)
  (let ((sum initial-value))
    (lambda (n)
      (set! sum (+ sum n))
      sum)))

(define (make-accumulator-2 acc)
  (lambda (x)
    (set! acc (+ x acc))
    acc))
;; bunu bn yaptm 2 lambda
(define make-acc-my
  (lambda (x)
    (lambda (y)
      (set! x (+ x y)) x)))

(define square
  (lambda (x)
    (lambda (y)
      (+ x y))))

;; ((square 2) 3)

;;(define A (make-accumulator-2 5))
;; (A 2)


; TODO tekrr TODO
(define make-monitored
  (let ((count 0))
    (lambda (f)
      (lambda (arg)
        (cond ((eq? arg 'how-many-calls?) count)
              ((eq? arg 'reset-count)
               (set! count 0)
               count)
              (else (set! count (+ count 1))
                    (f arg)))))))

(define s (make-monitored sqrt))
;; (s 100)
;; (s 'how-many-calls?)
(define (make-monitored-1 function)
  (define times-called 0)
  (define (mf message)
    (cond ((eq? message 'how-many-calls?) times-called)
          ((eq? message 'reset-count) (set! times-called 0))
          (else (set! times-called (+ times-called 1))
                (function message))))
  mf)

(define (make-monitored-2 proc)
  (let ((count 0))
    (lambda (first . rest)
      (cond ((eq? first 'how-many-calls?) count)
            ((eq? first 'reset-count) (set! count 0))
            (else (begin (set! count (+ count 1))
                         (apply proc (cons first rest))))))))

(define m+ (make-monitored-2 +))
;; (m+ 40 2)
;; (m+ 2 3)
;; (m+ 'how-many-calls?)


