#lang sicp

; Values
; Numbers Booleans Strings Procedures Lists

;Basic elements
; (a) slef-evaluationg - expressions whose value is the same as the expression

;(b) names - Name is looked up in the symbol table to find the value associated
; with it. Names may be made of any collection of characters that doesn't start
; with a number


;Combination
; (procedure arguments-seperated-by-spaces )

; Special Forms
; (a) define - (define name value)
; The name is bound to the result of evaluating the value. Return value is
; unspesificed


;(b) if - (if test consequent alternative)
; If the value of the test is not false (#f). evaluate the consequent, otherwise
; evaluate the alternative.

; Problems

; 1. Evaluation - For each expression:

;(a) Write the type of the expression
;(b) Write your guess as to the expression's return value. If the expression
; is erroneous simply indicate "error" for the value. If the expression returns
; unspesificied value, write whatever yout want!
;(c) Evaluate the expression, and copy the response from the *scheme* buffer

4 ; 4

5.5 ; 5.5

4.2e1 ; 42.0

(+ 1 2) ; 3

;; (7) ; err

(* (+ 7 8) (    - 5 6)) ; -15


(define one 1)
(define two (+ 1 one))
(define five 3)

(+ five two) ; 5

(define biggie-size *)

(define hamburger 1)

(biggie-size hamburger five) ; 3


(= 7 (+ 3 4)) ; #t

;; (= #t #f) ; expected number

;((+ 5 6)) ; expected procedure error

biggie-size ; <procedure:*>

(if #t 1 (+ 3 0))

(if (if #f #t #f) #f #t)
(if (if (= hamburger five) 3 7)
    (+ (if (= (+ 1 one) two)
           3
           5)
       7)
    "yay")

(/ 256
   (if (> five (+ two 1))
       (/ 7 (- hamburger 1))
       2))

(define fivv (- 12
                (if (> 5 2) 9 12)))
fivv



; 4. Define X - for each of the following expressions:

;(a) Identify the variables are are unbound.
; (b) Supply definitions (ie (define x ...)) for each of the variables that make
; the expression evaluate to the target value.
;(c) Type in the expressions and verify that your solution gives the correct result


;; (define x 1)
;; (define y 4)
;; (+ x (* y 3))


(define q #t)
(define r #f)
(if q (if r 3 4) 7)  ; 4

(define z 2)
(define a 2)
(+ 7 (if z (+ a z) 3))

(define yum -1)
(= yum (* -1 (+ yum 2)))

(define seventy-seven 77)
(define thirty-four 34)

(< (if (not thirty-four) 34 thirty-four)
   (+ thirty-four (* seventy-seven (if seventy-seven -1 1))))


;; 5. Primitive Procedures for each of the following expressions:
;; (a) Identify the primitive procedures which you don’t already know
;; (b) Write down a guess as to what the primitive procedure does.
;; (c) Look it up in the MITScheme reference manual (you may find the index handy).
;; (d) Write an example usage of the procedure and test it to see that it works as you suspect
;; it does.
;; (e) Fill in the blanks in the original expression such that it evaluates to the target value.

(+ 3 (abs -2)) ; 5
(expt 5 2) ; 5 uzeri 2
(if (> (expt 2 4 ) 15)
    (sqrt 2)
    (sqrt 4))


(string-append "foo" "bar"  "baz")

(string-append
 (number->string
  (gcd 35  7 ))
 " rules!" )

;; (define s  "yayrahyowzah" )
;; (if (string-suffix? "yowzah" s)
;;     (+ 7 (string-search-forward "ow" s))
;;     #f)




