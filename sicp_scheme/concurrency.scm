#lang sicp



(define (make-mutex)
  ;; initialize first with false
  (let ((cell (list false)))
    (define (the-mutex m)
      ;; if called with acquire
      (cond ((eq? m 'acquire)
             ;; test if mutex is available (true)
             (if (test-and-set! cell)
               ;; then call with acquire
                 (the-mutex 'acquire)))
            ;; if we call it with release, clear cell set to false
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell) (set-car! cell false))

(define (test-and-set! cell)
  (if (car cell) true (begin (set-car! cell true) false)))

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      ;; take arguments buttfist
      (define (serialized-p . args)
        ;; call mutex with acquire
        (mutex 'acquire)
        ;; wait for applying
        (let ((val (apply p args)))
          ;; then release
          (mutex 'release)
          ;; return wal
          val))
      ;; return serialized-p
      serialized-p)))


(define x 10)
(parallel-execute
 (lambda () (set! x (* x x)))
 (lambda () (set! x (+ x 1))))


(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protected withdraw))
            ((eq? m 'deposit) (protected deposit))
            ((eq? m 'balance) balance)
            (else (error  "Unknown request: MAKE-ACCOUNT" m))))
    dispatch))
