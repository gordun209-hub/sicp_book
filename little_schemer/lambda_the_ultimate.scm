#lang sicp

(define rember-f
  (lambda (test? a l)
    (cond
      ((null? l) (quote()))
      (else (cond
              ((test? (car l) a) (cdr l))
              (else (cons (car l)
                          (rember-f test? a (cdr l)))))))))

(define rember-f-but-short
  (lambda (test? a l)
    (cond
      ((null? l) (quote ()))
      ((test? (car l) a) (cdr l))
      (else (cons (car l)
                  (rember-f-but-short test? a (cdr l)))))))

; what is
(lambda (a)           ; It is a function that, when passed an
  (lambda (x)         ; argument a ,returns the function
    (eq? x a)))       ; (lambda (x) (eq? x a)) where a is just that argument

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(define eq?-salad (eq?-c 'salad))
(eq?-salad 'salad)

(define remberf
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l ) (quote()))
        ((test? (car l) a) (cdr l))
        (else (cons (car l)
                    ((remberf test?) a
                                     (cdr l))))))))
