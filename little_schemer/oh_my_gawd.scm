#lang sicp

(define atom?
  (lambda (l)
    (and (not (pair? l)) (not (null? l)))))

(define rember*
  (lambda (a l)
    (cond
      ((null? l ) (quote ()))
      ((atom? (car l))
       (cond
         ((eq? (car l ) a)
          (rember* a (cdr l)))
         (else (cons (car l)
                     (rember* a (cdr l))))))
      (else (cons (rember* a (car l))
                  (rember* a (cdr l)))))))

(rember* 'sauce '((tomato sauce)
                  ((bean) sauce)))
(car '((tomato sauce) ((bean ) sauce)))  ;=> (tomato sauce) not atom go next
(cdr '((tomato sauce) ((bean) sauce)))
     ; (cdr '((tomato
     ; (cons (rember* 'sauce '(tomato sauce)
     ;       (rember* 'sauce (

