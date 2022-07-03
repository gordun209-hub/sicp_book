#lang sicp

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items ) (- n 1))))

(define squares (list 1 4 9 16 25))
(list-ref squares 3) ; 16

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define odds (list 1 3 5 7))

;; (length odds) ; 4

(define (iterative-length items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0 ))

;; (iterative-length odds) ; 4

;; Another conventional programming technique is to “cons up” an an-
;; swer list while cdring down a list, as in the procedure append, which
;; takes two lists as arguments and combines their elements to make a new
;; list:
;; append is also implemented using a recursive plan. To append lists list1
;; and list2, do the following:
;; • If list1 is the empty list, then the result is just list2.
;; • Otherwise, append the cdr of list1 and list2, and cons the car
;; of list1 onto the result:

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

;; Exercise 2.17: Define a procedure last-pair that returns
;; the list that contains only the last element of a given (nonempty)
;; list:

;(last-pair (list 23 72 149 34)) -> (34)
;; (define (last-pair list)
;;   (define (last-iter last list)
;;     (if (null? list)
;;         last
;;         (last-iter (car list) (cdr list))))
;;   (last-iter (car list) (list)))
; sonunda yaptk mq :q
(define (last-pair list last)
  (if (= (length list) 0)
      last
      (last-pair (cdr list) (car list))))

(last-pair (list 1 2 3 5) (car (list 1 2 3 5)))


;; Exercise 2.18: Define a procedure reverse that takes a list
;; as argument and returns a list of the same elements in re-
;; verse order:
;; (reverse (list 1 4 9 16 25))
;; (25 16 9 4 1)

;; (define (reverse list )
;;   (if (null? list)
;;       nil
;;       (cons (cdr list) )))
;; (reverse (list 1 4 9 16 5))


(define nil '())

(define (reverse items)
  (define (iter items result)
    (if (null? items)
        result
        (iter (cdr items) (cons (car items) result))))

  (iter items nil))
;; (reverse (list 1 2 3 4))
;; (iter (2 3 4) (cons 1 nil)) -> cons 1 nil === result oldu
;; (iter (3 4) (cons 2 (cons 1 nil) ******* ONEMLI ************* ab

;; Usage
(reverse (list 1 2 3 4))
;;
;; (define (count-change amount) (cc amount 5))
;; (define (cc amount kinds-of-coins)
;;   (cond ((= amount 0) 1)
;;         ((or (< amount 0) (= kinds-of-coins 0)) 0)
;;         (else (+ (cc amount
;;                      (- kinds-of-coins 1))
;;                  (cc (- amount
;;                         (first-denomination
;;                          kinds-of-coins))
;;                      kinds-of-coins)))))
;; (define (first-denomination kinds-of-coins)
;;   (cond ((= kinds-of-coins 1) 1)
;;         ((= kinds-of-coins 2) 5)
;;         ((= kinds-of-coins 3) 10)
;;         ((= kinds-of-coins 4) 25)
;;         ((= kinds-of-coins 5) 50)))

(define us-coins (list 50 25 10 5 1))

(define uk-coins (list 100 50 20 10 5 2 1 0.5))
; we could then call cc as follows
;; (cc 100 us-coins) ; 292



(define (except-first-denomination list)
  (cdr list))
(define (first-denomination denominations) (car denominations))
(define (except-first-denom denominations) (cdr denominations))
(define (no-more? denominations) (null? denominations))

(define (new-cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (new-cc amount
                    (except-first-denomination
                     coin-values))
            (new-cc (- amount
                       (first-denomination
                        coin-values))
                    coin-values)))))

(new-cc 100 (list 50 25 10 5 1))
