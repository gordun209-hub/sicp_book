#lang sicp
(define (cons x y)
  (lambda (msg)
    (cond ((eq? msg 'CAR) x)
          ((eq? msg 'CDR) y)
          ((eq? msg 'PAIR) #t)
          (else (error "pair cannot" msg)))))

(define (car p) (p 'CAR))
(define (cdr p) (p 'CDR))
(define (pair? p)
  (and (procedure? p) (p 'PAIR)))

(define laa (cons 'laa nil))

(car laa)
(cdr laa)
(pair? laa)