#lang sicp
;;
;; ;; (define (cons x y)
;; ;;   (let ((new (get-new-pair)))
;; ;;     (set-car! new x)
;; ;;     (set-cdr! new y)
;; ;;     new))
;;
;; ;; Exercise 3.12
;;
;; (define (append x y)
;;   (if (null? x)
;;       y
;;       (cons (car x) (append (cdr x) y))))
;;
(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)
;;
(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))
;;
;; (define xx (list 'a 'b))
;;
;; (define y (list 'c 'd))
;; (define z (append x y))
;; z
;;
;; (cdr xx)
;; ; (b)
;;
;; (define w (append! xx y))
;; (cdr xx)
;;
;; (define (make-cycle xx)
;;   (set-cdr! (last-pair xx) xx)
;;   xx)

;; (last-pair '(2 3 4))
;; (define zz (make-cycle (list 'a 'b 'c)))


(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        ;; temp = rest of x
        (let ((temp (cdr x)))
          ;; set cdr of x to y so y is '()
          ;; leave 1 element inside x that is first
          ;; rest is inside temp
          (set-cdr! x y)
          ;; recur with temp that is rest of x and
          ;; x that is first element of x
          (loop temp x))))
  (loop x '()))


;; (loop '(3 4) '(2)
;; temp - '(4)
;; '(2)
(define v (list 'a 'b 'c 'd))

;; (define w (mystery v))


;; Sharing and identity

;; (define x (list 'a 'b))
;;
;; (define z1 (cons x x))
;;
;; (define z2 (cons (list 'a 'b) (list 'a 'b)))
;;
;; (define (set-to-wow! x) (set-car! (car x) 'wow) x)

;; z1
;; (set-to-wow! z1)
;;
;; z2

;; Exercise 3.16

;; count number of pairs "its easy" :D:D

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(count-pairs (list 'a 'b 'c))  ;; => 3
;; (define second (cons 'a 'b))
;; (define third (cons 'a 'b))
;; (define first (cons second third))
;; second
;; third

;; (set-car! third second)
;; third
;; (count-pairs first)  ;; => 4

;; (define third (cons 'a 'b))
;; (define second (cons third third))
;; second
;; (define first (cons second second))
;; first
;; (count-pairs first  ;; => 7

;; (define lst (list 'a 'b 'c))
;; (set-cdr! (cddr lst) lst)
;; (count-pairs lst)  ;; never returns


(define e (list 'a))
(define zz (cons e e))
zz
;; (define x (list 'a 'b))
;; (define y (list 'c 'd))
;; (define z (append x y))
;; (define w (append! x y))
(count-pairs (list (list 'a) 'b))
(count-pairs zz)


(define x '(foo))
(define y (cons x x))
(define str3 (cons y y))
(count-pairs str3)
str3
