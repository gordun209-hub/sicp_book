#lang sicp

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))

;; (list-ref squares 3)

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define odds (list 1 3 5 7))
;; (length odds)

(define (length2 items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

;; (append '(2 3 4) '(5 6 7))
;; (cons 2 (append '(3 4) '(5 6 7)))
;; (cons 2 (cons 3 (append '(4) '(5 6 7))))
;; (cons 2 (cons 3 (cons 4 (append '() '(5 6 7)))))
;; (cons 2 (cons 3 (cons 4 '(5 6 7))))

(define (newappend list1 list2)
  (if (null? list1)
      list2
      (cons (append (cdr list1) list2) (car list1))))
;; (append '(2 3 4) '(5 6 7))
;; (newappend '(2 3 4) '(5 6 7))

;; Exc 2.17 define a procedure last-pairs returns the list that contains
;; only the last element of a given list

(define (last-pairs l)
  (cond ((null? (cdr l)) (car l))
        (else (last-pairs (cdr l)))))

;; (last-pairs '(2 3 4 5))
;;
;; (last-pairs (list 23 72 149 34))



;; Exc 2.18 define reverse (bu zorms:D)

(define (reverse ls)
  (define (reverse-iter ls answer)
    (cond ((null? ls) answer)
          (else
           (reverse-iter (cdr ls) (cons (car ls) answer)))))
  (reverse-iter ls nil))


;; (reverse (list 1 4 9 16 25))

;(reverse (list 1 3 4 5))
;; (reverse-iter (list 1 3 4 5) nil)
;; (reverse-iter (3 4 5) (cons 1 nil))
;; (reverse-iter (4 5) (cons 4 (cons 1 nil))

;; attempt to write recursive version
;; thats slow
(define (reverse2 ls)
  (cond ((null? ls) nil)
        (else (append (reverse2 (cdr ls)) (list (car ls))))))

;;(reverse2 (list 1 4 9 ))
;; (cons (reverse2 (4 9) 1))
;;(cons (cons (reverse2 (9)) 4) 1)
;; (cons (cons (cons (reverse2 '() 9) 4) 1))
;; (cons (cons (cons (nil
;; (reverse2 (list 1 4 9 16 25))

;; Exc 2.20

(define (f x y . z) x)
(f 1 2 3 4 5 6)
;; wtf
(define (g . w) w)

(g 1 2 3 4 5 6)

(define (same-parity head . tail)
  (let ((selectparity ((even? head) even? odd?)))
    (cond ((null? tail) '())
          ((selectparity head)
           (cons  head (same-parity (cdr tail))))
          (else (same-parity (cdr tail))))))

;; (same-parity 1 2 3 4 5)

(define (same-parityy first . rest)
  (define (same-parity-iter source dist remainder-val)
    (if (null? source)
        dist
        (same-parity-iter (cdr source)
                          (if (= (remainder (car source) 2) remainder-val)
                              (append dist (list (car source)))
                              dist)
                          remainder-val)))

  (same-parity-iter rest (list first) (remainder first 2)))


