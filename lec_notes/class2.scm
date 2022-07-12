#lang sicp

;; 1. Evaluation For each expression:
;; (a) Write the type of the expression
;; (b) Write your guess as to the expression’s return value. If the expression is erroneous
;; simply indicate “error” for the value. If the expression returns an unspecified value,
;; write whatever you want! If the expression returns a procedure, indicate “procedure”
;; for the value.
;; (c) Evaluate the expression, and copy the response from the *scheme* buffer.


(lambda (x) x)
((lambda (x) x) 17); 17
((lambda (x y) x) 42 17) ; 42
;; ((lambda (x y) y) (/ 1 0) 3) ; div by zero
((lambda (x y) (x y 3)) (lambda (a b) (+ a b)) 14) ;17



;; 2. Writing Procedures Write the procedure indicated. Then test it in scheme to make sure it
;; works.
;; (a) Write a procedure cube that returns the cube of it’s input.
(define cube
  (lambda (x) (* x (* x x))))
(cube 2)
;; (b) Write a procedure theanswer?, which returns true (#t) if the input is the number 42.
(define theanswer?
  (lambda (x) (if (= x 42) #t #f)))
(theanswer? 42)
;; (c) Write a procedure sign that returns 1 if it’s input is positive, 1 if it’s input is negative,
;; and 0 if it’s input is 0.
(define sign
  (lambda (x) (cond ((= x 0) 0)
                    (else 1))))
(sign 0)
;; 36.090, IAP 2005—Lecture 2
;; (d) Given a margin width m, which is both the top, bottom, left, and right margin of the
;; page, write a procedure that computes the ”usable” (non margin) area of the 8.5in by
;; 11in sheet of paper.
(define width 8.5)
(define height 11)
(define usablepage
  (lambda (x) (* (- width (+ x x)) (- height (+ x x)))))
(usablepage 0)
(usablepage 1)
;; (usablepage 0)
;; ;Value: 93.5
;; (usablepage 1)
;; ;Value: 58.5
;; (define usablepage
;; (e) Write a procedure that when given a width, returns the length of the most beautiful
;; rectangle having that width. According to studies, the most beautiful rectangle is one
;; whose ratio of length to width is the golden ratio. The golden ratio can be most easily
;; be expressed as (sqrt(5)+1)/2.
;; (* (/  (+ (sqrt 5) 1 )2) 34.5)
(define mostbeautifulrectangle
  (lambda (x)
    (* (/ (+ (sqrt 5) 1 ) 2) x)))
(mostbeautifulrectangle 34.5)
;; (beautifulrectangle 1)
;; ;Value: 1.618033988749895
;; (beautifulrectangle 34.5)
;; ;Value: 55.82217261187137
;; (define beautifulrectangle

; TODO
;; (f) Write a procedure that computes the positive root of the quadratic polynomial using the
;; quadratic formula. The positive root is the larger of the two roots. If the polynomial
;; has complex roots, your procedure should return the string ”complex roots”.
;; (postiveroot 1 2 1)
;; ;Value: 1
;; (positiveroot 3 1 3)
;; ;Value: "complex roots"
;; (define postiveroot
;; 46.090, IAP 2005—Lecture 2

; TODO
;; 3. BiggieSizing!
;; Suppose we’re designing an pointofsale and ordertracking system for Wendy’s 1 . Luckily
;;  ̈the UberQwuick drive through supports only 4 options: Classic Single Combo (hamburger
;; with one patty), Classic Double With Cheese Combo (2 patties), and Classic Triple with
;; Cheese Combo (3 patties), AvantGarde Quadruple with Guacamole Combo (4 patties). We
;; shall encode these combos as 1, 2, 3, and 4 respectively. Each meal can be biggiesized to
;; acquire a larger box of fries and drink. A biggiesized combo is represented by 5, 6, 7, and 8
;; respectively.
;; (a) Write a procedure named biggiesize which when given a regular combo returns a
;; biggiesized version.
;; (b) Write a procedure named unbiggiesize which when given a biggiesized combo returns
;; a non biggiesized version.
;; (c) Write a procedure named biggiesize? which when given a combo, returns true if the
;; combo has been biggiesized and false otherwise.
;; (d) Write a procedure named comboprice which takes a combo and returns the price of
;; the combo. Each patty costs $1.17, and a biggiesized version costs $.50 extra overall.
;; 16.090 and MIT do not endorse and are not affiliated with Wendy’s in any way. They merely



; Recursion
;; 4. Write expt, a procedure that raises x to the nth power. You may assume that n is nonnegative
;; and integer

;(expt 2 2) ; 4

(define expts
  (lambda (x n)
    (cond
      ((= n 0) 1)
      (else
       (* x (expt x (- n 1)))))))

;; (expts 2 3)

; (remainder 2 5)
; 2 
;(remainder 7 5)
; 2 
(define remainder 
  (lambda (x y)
    (cond 
      ((< (- x y) 0) x)
      (else 
        (remainder (- x y) y)))))

;; (remainder 2 5)
;;
;; (remainder 7 5)

(define fib 
  (lambda (n)
    (cond ((< n 2) 1)
          (else 
            (+ (fib (- n 1)) (fib (- n 2)))))))

;; (fib 0)
;;
;; (fib 1)
;; (fib 6)
(fib 2)
