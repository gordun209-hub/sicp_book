#lang sicp

;; we can do multiplication by successive adding
;; Linear steps
(define *
  (lambda (a b)
    (if (= b 0)
        0
        (+ a (* a (- b 1))))))

;; define this like fast-exp with halve and double

(define (double x) (* x 2))

(define (halve x) (/ x 2))

;; * a b = ?
;; if a and b are both even,  double a and halve b
;; else

;; lets say
(* 4 8) ;; same
(* (* 4 2) (/ 8 2)) ;; same
(* (* (* 4 2) (/ 2 2)) 4)


(define (** a b)
  (cond ((= b 0) 0)
        ((even? b) (double (** a (halve b))))
        (else (+ a (** a (- b 1))))))

; (fast-mult 3 7)
; (+ 3 (fast-mult 3 6))
; (+ 3 (double (fast-mult 3 3))
; (+ 3 (double (+ 3 (fast-mult 3 2))))
; (+ 3 (double (+ 3 (double (fast-mult 3 1)))))
; (+ 3 (double (+ 3 (double (+ 3 (fast-mult 3 0))))))
; (+ 3 (double (+ 3 (double (+ 3 0)))))
; (+ 3 (double (+ 3 (double 3))))
; (+ 3 (double (+ 3 6)))
; (+ 3 (double 9))
; (+ 3 18)
; 21

(define (fast-mult a b)
  (cond ((= b 0) 0)
        ((even? b) (fast-mult (double a) (halve b)))
        (else (+ a (fast-mult a (- b 1))))))

