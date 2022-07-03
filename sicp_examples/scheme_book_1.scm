#lang racket/base
;; ; Let expressions
;;
;; (let ([x 2]) (+ x 3)) ; => 5
;;
;; (let ([y 3]) (+ 2 y)) ; => 5
;;
;; (let ([x 2] [y 3]) (+ x y)) ; => 5
;;
;; ; (let ((var val) ..) exp1 exp2 ...)
;;
;; (+ (* 4 4) (* 4 4)) ; => 32
;;
;; (let ([a (* 4 4)]) (+ a a)) ; => 32
;;
;; (let ([list1 '(a b c)] [list2 '(d e f)])
;;   (cons (cons (car list1) (car list2)) (cons (car (cdr list1)) (car (cdr list2)))))
;;
;; (let ([f +]) (f 2 3)) ; => 5
;;
;; (let ([f +] [x 2]) (f x 3)) ; => 5
;;
;; (let ([f +] [x 2] [y 3]) (f x y)) ; => 5
;;
;; ;The variables bound by let are visible only within the body of the let.
;; ; + = *
;; (let ([+ *]) (+ 2 3)) ; => 6
;;
;; (+ 2 3) ; => 5
;;
;; (let ([a 4] [b -3])
;;   (let ([a-squared (* a a)] [b-squared (* b b)])
;;     (+ a-squared b-squared)))

(let ((x 1))
  (let ((x (+ x 1)))
    (+ x x))) ; => 4

(let ((x 1))
  (let ((new-x (+ x 1)))
    (+ new-x new-x))) ; => 4

(let ((x 9))
  (* x
     (let ((x (/ x 3)))
       (+ x x))))

