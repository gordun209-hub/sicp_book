#lang sicp

(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))
(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

(define W1 (make-withdraw 100))
(define W2 (make-withdraw 100))

(W1 50) ;; [=> 50

(W2 70) ;; [=> 30


;; (define (make-account balance password)
;;   (define (withdraw amount)
;;     (if (>= balance amount)
;;         (begin (set! balance (- balance amount))
;;                balance)
;;         "Insufficient funds"))
;;   (define (deposit amount)
;;     (set! balance (+ balance amount))
;;     balance)
;;   (define (dispatch m pw)
;;     (if (eq? pw password)
;;         (cond ((eq? m 'withdraw) withdraw)
;;               ((eq? m 'deposit) deposit)
;;               (else (error "Unknown request: MAKE-ACCOUNT" m)))
;;         2))
;;   dispatch)




(define (accumulator num)
  (lambda (amount) (begin (set! num (+ num amount) )num)))

(define accs (accumulator 1))



(define (make-monitored proc)
  (let ((count 0))
    (define (update-count) (set! count (+ count 1)))
    (define (dispatch m)
      (cond ((eq? m 'how-many-calls?) count)
            (else (begin (set! m (proc m)) (update-count) m))))
    dispatch))
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount) (begin (set! balance (- balance amount)) balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount)) balance)
  (define (dispatch p m)
    (cond ((not (eq? p password)) (lambda (x) "Incorrect password"))
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT" m))))
  dispatch)

(define acc (make-account 100 'sss))

((acc 'withdraw 'sss) 50)

((acc 'withdraw 'ssss) 50)

(define (make-accountt balance password)
  (let ((bad-passwords 0))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          (display "Insufficient funds")))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (good-password? p)
      (cond ((eq? p password)
             (set! bad-passwords 0)
             true)
            ((< bad-passwords 7)
             (set! bad-passwords (+ bad-passwords 1))
             false)
            (else
             (call-the-cops))))
    (define (call-the-cops)
      (display "Cops called!")
      false)
    (define (dispatch p m)
      (if (good-password? p)
          (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                (else (error "Unknown request -- MAKE-ACCOUNT" m)))
          (lambda (x) (display "Incorrect password") (newline))))
    dispatch))


