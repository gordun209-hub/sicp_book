#lang scheme

(cons
 (cons 'a 'b)
 (cons (cons '() '())
       (cons (cons 'd '())
             (cons 'c '()))))


(let ((list1 '(a b c)) (list2 '(d e f)))
  (cons (cons (car list1)
              (car list2))
        (cons (car (cdr list1))
              (car (cdr list2)))))

(let ((double (lambda ( x) (+ x x))))
  (list (double (* 3 4))
        (double (/ 99 11))
        (double (- 2 7))))

(let ((f (lambda (x) x)))
  (f 'a)) ; => a

(let ((f (lambda x x)))
  (f 'a)) ; => a

(let ((f (lambda (x . y) x)))
  (f 'a))
(let ((f (lambda (x . y) y )))
  (f 'a 'b))
