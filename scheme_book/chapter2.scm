#lang sicp
; nil onemli
(cons 'a  'b) ; => (a . b)
(cons 'a (cons 'b nil)) ; => (a  b)
(cdr '(a b)) ; (b)
(cdr '(a . b)) ; => b
(cons 'a '(b c)); => (a b c)
(cons 'a '(b . c)) ; => (a b . c)
'(a . (b . (c . ())))
'(a . (c .()))
; Exercises
;a .
(cons 'car 'cdr) ; (car . cdr)
;b
(list 'this '(is silly)); (this (is silly))
;c
; burda proper list oldugu icin cons sorun cikarmadi
(cons 'is '(this silly?)) ; (is this silly)
; burda proper list deil die bole consluiyo
(cons '(is this) 'silly?) ; ((is this) . silly?)
;d
(quote (+ 2 3)); (+ 2 3)

(cons '+ '(2 3)) ; (+ 2 3)
(car '(+ 2 3)); +
(cdr '(+ 2 3)); (2 3)

cons  ; procedure
(quote cons) ; (cons)
(quote (quote cons)) ;(quote cons)
(car (quote (quote cons))) ; (quote)
(+ 2 3) ; 5
(+ (car '(2 3)) (car (cdr '(2 3)))) ;

((car (list + - * /)) 2 3) ; 5


((car (cdr (list + - * /))) 17 5)
;((car (- * /) 17 5)
; (- 17 5)
; 12
;; Let exercises
(let ([ x 2])
  (+ x 3)) ; => 5

(let ([y 3])
  (+ 2 y)) ; => 5

(let ([x 2]
      [y 3])
  (+ x y)) ; => 5

(+ (* 4 4) (* 4 4)) ; => 32
(let [(a (* 4 4))]
  (+ a a))


;; brackets are often used in place of parantheses to delimit bindings of a let exp

(let ([list1 '(a b c)] [list2 '(d e f)])
  (cons (cons (car list1)
              (car list2))
        (cons (car (cdr list1))
              (car (cdr list2)))))


(let ([f +])
  (f 2 3)) ; => 5
(let ([f +] [x 2])
  (f x 3)) ; => 5

(let ([f +] [x 2] [y 3])
  (f x y))

;; variables bound by let are visible only within the body of the let

(let ([+ *])
  (+ 2 3)) ; => 6

(+ 2 3) ; => 5

(let ([a 4] [b -3])
  (let
      (
       [a-squared (* a a)]
       [b-squared (* b b)])
    (+ a-squared b-squared)))

(let ([x 1])
  (let ([x (+ x 1)])
    (+ x x))) ; => 4

(let ([x 1])
  (let ([new-x (+ x 1)])
    (+ new-x new-x)))

(let ([x 'a] [y 'b])
  (display x) ; a
  (newline)
  (display y) ;b
  (newline)
  (list (let ([x 'c]) (cons x y))
        (let ([y 'd]) (cons x y))))


(let ([x '((a b) c)])
  (cons (let ([x (cdr x)])
          (car x))
        (let ([x (car x)])
          (cons (let ([x (cdr x)])
                  (car x))
                (cons (let ([x (car x)])
                        x)
                      (cdr x))))))
;; Lambda exc
((lambda (x) (+ x x)) (* 3 4)) ; => 24


(let ([double (lambda (x) (+ x x))])
  (list (double (* 3 4))
        (double (/ 99 11))
        (double (- 2 7))))

(let ([double-cons (lambda (x) (cons x x))])
  (double-cons 'a))


(let ([double-any (lambda (f x) (f x x))])
  (list (double-any + 13)
        (double-any cons 'a)))

(let ([x 'a])
  (let ([f (lambda (y) (list x y))])
    (f 'b)))

(let ([f (let ([x 'sam])
           (lambda (y z) (list x y z)))])
  (f 'i 'am))



(let ([f (let ([x 'sam])
           (lambda (y z) (list x y z)))])
  (let ([x 'not-sam])
    (f 'i 'am)))

(let ([f (lambda x x)])
  (f '(1 2) '(3 4)))


(let ([f (lambda x x)])
  (f  2))

(let ([g (lambda (x . y) (list x y))])
  (g 1 2 3 4))

(let ([h (lambda (x y . z) (list x y z))])
  (h 'a 'b 'c 'd))

;ex 2.5.1

(let ([f (lambda (x) x)])
  (f 'a)) ; => a

(let ([f (lambda x x)])
  (f 'a)) ; => (a)

(let ([f (lambda (x . y) x)])
  (f 'a)) ; => a

(let ([f (lambda (x . y) y)])
  (f 'a)) ; => ()


;; (lambda (f x) (f x))
;; (lambda (x) (+ x x))
;(lambda (x y) (f x y))
;; (lambda (x)
;;   (cons x (f x y)))
;; (lambda (x)
;;   (let ([z (cons x y)])
;;     (x y z)))

;; (define lists (lambda x x))
;; (lists 2 3 4)
;; cddr and cadr
(define cadr
  (lambda (x)
    (car (cdr x))))

(define cddr
  (lambda (x)
    (cdr (cdr x))))

(define (lists . x) x)
(lists 2 3 4)

(define doubler
  (lambda (f)
    (lambda (x) (f x x))))

(define proc1
  (lambda (x y)
    (proc2 x y)))

(define proc2 cons)
(proc1 'a (proc1 'b nil))

;; abs
(define abs
  (lambda (n)
    (if (< n 0)
        (- 0 n)
        n)))

(define abs-2
  (lambda (n)
    (if (>= n 0)
        n
        (- 0 n))))

(define abs-3
  (lambda (n)
    (if (not (< n 0))
        n
        (- 0 n))))

(define abs-4
  (lambda (n)
    (if (= n 0)
        0
        (if (< n 0)
            (- 0 n)
            n))))

(define abs-5
  (lambda (n)
    ((if (>= n 0) + -)
     0
     n)))

;; boolean statements
(if #t 'true 'false)
(if #f 'true 'false)
(if '() 'true 'false)
(if 1 'true 'false)
(if '(a b c) 'true 'false)


(not #t)
(not "false")
(not #f)

(or)
(or #f)
(or #f #t)
(or #f 'a #f)


(null? '()) ;#t
(null? 'abc) ; #f
(null? '(x y z)) ; #f
(null? (cdddr '(x y z))); #t


(eqv? 'a 'a)
(eqv? 'a 'b)
(eqv? #f #f)
(eqv? 3 3)
(eqv? 3 2)
(let ([x "Hi mom!"])
  (eqv? x x))

(let ([x (cons 'a 'b)])
  (eqv? x x))

(eqv? (cons 'a 'b) (cons 'a 'b))

(pair? '(a . c)) ; #t
(pair? '(a b c)) ; #t
(pair? '()) ; #f
(pair? 'abc) ;#f
(pair? "Hi Mom!");#f
(pair? 1234567890) ; #f
;; Signs
(define sign
  (lambda (n)
    (if (< n 0)
        -1
        (if (> n 0)
            +1
            0))))

(define sign-2
  (lambda (n)
    (cond
      [(< n 0) -1]
      [(> n 0) +1]
      [else 0])))
;; tax
(define income-tax
  (lambda (income)
    (cond
      [(<= income 10000) (* income .05)]
      [(<= income 20000) (+ (* (- income 10000) .58) 500.00)]
      [(<= income 30000) (+ (* (- income 20000) .13) 1300.00)]
      [else (+ (* (- income 30000) .21) 2600.00)])))

;; (income-tax 5000)
;; (income-tax 15000)
;;(income-tax 19999)
;;(income-tax 20000)

(define length
  (lambda (ls)
    (if (null? ls)
        0
        (+ (length (cdr ls)) 1))))
(length '(a b c d))


(define list-copy
  (lambda (ls)
    (if (null? ls)
        '()
        (cons (car ls)
              (list-copy (cdr ls))))))
(list-copy '())
(list-copy '(a b c))


(define memv
  (lambda (x ls)
    (cond
      [(null? ls) #f]
      [(eqv? (car ls) x ) ls]
      [else (memv x (cdr ls))])))
(memv 'a '(a b b d))
(memv 'b '(a b b d))
(memv 'c '(a b b d))
(memv 'd '(a b b d))
(if (memv 'b '(a b b d))
    "yes"
    "no")
;; some procedures
(define remv
  (lambda (x ls)
    (cond
      [(null? ls) '()]
      [(eqv? (car ls) x) (remv x (cdr ls))]
      [else (cons (car ls) (remv x (cdr ls)))])))

(remv 'a '(a b b d))
(remv 'b '(a b b d))
(remv 'c '(a b b d))
(remv 'd '(a b b d))


(define tree-copy
  (lambda (tr)
    (if (not (pair? tr))
        tr
        (cons (tree-copy (car tr))
              (tree-copy (cdr tr))))))

(tree-copy '((a . b) .c))
;; MAP
(define abs-all
  (lambda (ls)
    (if (null? ls)
        '()
        (cons (abs (car ls))
              (abs-all (cdr ls))))))
(abs-all '(1 -2 3 -4 5 -6))


(define abs-all-map
  (lambda (ls)
    (map abs ls)))

(map abs '(1 -2 3 -4 5 -6))


(map (lambda (x) (* x x))
     '(1 -3 -5 7))

(map cons '(a b c) '(1 2 3))


(define map1
  (lambda (p ls)
    (if (null? ls)
        '()
        (cons (p (car ls))
              (map1 p (cdr ls))))))

; Exercise

; (make-list 7 '())

; exc 2.8.3
(define make-list
  (lambda (len obj)
    (cond
      [(= len 0) '()]
      [else (cons obj (make-list (- len 1) obj))])))
(make-list 7 '(s))

; exc 2.8.4

(define listref
  (lambda (ls index)
    (cond
      [(= index 0) (car ls)]
      [else (listref (cdr ls) (- index 1))])))

(define listtail
  (lambda (ls tail)
    (cond
      [(= tail 0) ls]
      [else (listtail (cdr ls) (- tail 1))])))

(listref '(1 2 3 4) 0)
(list-ref '(1 2 3 4) 0) ; => 1
(list-tail '(1 2 3 4) 0) ; => (1 2 3 4)
(listref '(a short (nested) list) 2)
(list-ref '(a short (nested) list) 2) ; => (nested)
(listtail '(a short (nested) list) 2) ; => ((nested) list)



;TODO
;;(transpose '((a . 1) (b . 2) (c . 3)))




(define abcde '(a b c d e))
(set! abcde (cdr abcde)) ; abcde = (b c d e)

(let ([abcde '(a b c d e)])
  (set! abcde (reverse abcde))
  abcde)


;; 2 type of quadratic formula proc
(define quadratic-formula
  (lambda (a b c)
    (let ([root1 0] [root2 0] [minusb 0] [radical 0] [divisor 0])
      (set! minusb (- 0 b))
      (set! radical (sqrt (- (* b b ) (* 4 (* a c)))))
      (set! divisor (* 2 a))
      (set! root1 (/ (+ minusb radical) divisor))
      (set! root2 (/ (- minusb radical) divisor))
      (cons root1 root2))))

(define quadratic-formula-2
  (lambda (a b c)
    (let ([minusb (- 0 b)]
          [radical (sqrt (- (* b b) (* 4 (* a c))))]
          [divisor (* 2 a)])
      (let ([root1 (/ (+ minusb radical) divisor)]
            [root2 (/ (- minusb radical) divisor)])
        (cons root1 root2)))))


;; Assignments

(define kons-count 0)

(define kons
  (lambda (x y)
    (set! kons-count (+ kons-count 1))
    (cons x y)))

(kons 'a '(b c))
kons-count
(kons 'a (kons 'b (kons 'c '())))
kons-count

(define next 0)
(define count
  (lambda ()
    (let ([v next])
      (set! next (+ next 1))
      v)))

(define make-counter
  (lambda ()
    (let ([next 0])
      (lambda ()
        (let ([v next])
          (set! next (+ next 1))
          v)))))

(define count1 (make-counter))
(define count2 (make-counter))

(count1) ; 0
(count2) ; 0
(count1) ; 1
(count1) ; 2
(count2) ; 1

(define shhh #f)
(define tell #f)
(let ([secret 0])
  (set! shhh
        (lambda (message)
          (set! secret message)))
  (set! tell
        (lambda ()
          secret)))

(shhh "sally likes harry?? gone sus?")
(tell)

;; Stack implementationg
(define make-stack
  (lambda ()
    (let ([ls '()])
      (lambda (msg . args)
        (cond
          [(eqv? msg 'empty?)( null? ls)]
          [(eqv? msg 'push!)(set! ls (cons (car args) ls))]
          [(eqv? msg 'top)(car ls)]
          [(eqv? msg 'pop!)(set! ls (cdr ls))])))))

(define stack1 (make-stack))
(define stack2 (make-stack))
(list (stack1 'empty?) (stack2 'empty?))
(stack1 'push! 'a)
(list (stack1 'empty?) (stack2 'empty?))
(stack1 'push! 'b)
(stack2 'push! 'c)
(stack1 'top)
(stack2 'top)

(stack1 'pop!)
(stack1 'top)
(list (stack1 'empty?) (stack2 'empty?))
(stack1 'pop!)
(list (stack1 'empty?) (stack2 'empty?))



(define p (list 1 2 3))
(set-car!  (cddr p) 'two)
p
(set-car!  (cdr p) 'two)
p
(set-cdr! p '())
p
;; interesting queue
(define make-queue
  (lambda ()
    (let ([end (cons 'ignored '())])
      (cons end end))))
(define putq!
  (lambda (q v)
    (let ([end (cons 'ignored '())])
      (set-car! (cdr q) v)
      (set-cdr! (cdr q) end)
      (set-cdr! q end))))
(define getq
  (lambda (q)
    (car (car q))))
(define delq!
  (lambda (q)
    (set-car! q (cdr (car q)))))

(define myq (make-queue))

(putq! myq 'a)
(putq! myq 'b)
(getq myq)
(delq! myq)
(getq myq)
(delq! myq)
(putq! myq 'c)
(putq! myq 'd)
(getq myq)
(delq! myq)
(getq myq)


(define-syntax let
  (syntax-rules ()
    [(_ ((x e) ...) b1 b2 ...)
     ((lambda (x ...) b1 b2 ...) e ...)]))

(define-syntax and
  (syntax-rules ()
    [(_) #t]
    [(_ e) e]
    [(_ e1 e2 e3 ...)
     (if e1 (and e2 e3 ...) #f)]))


(define-syntax or
  (syntax-rules ()
    [(_) #f]
    [(_ e) e]
    [(_ e1 e2 e3 ...)
     (let ([t e1])
       (if t t (or e2 e3 ...)))]))


