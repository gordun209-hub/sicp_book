#lang sicp
(define square
  (lambda (x) (* x x)))
(define (square-list items)
  (define (iter things answer)
    (display things )
    (display answer )
    (newline)
    (if (null? things)
        answer
        ; bura sondan gidio
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))

;; (square-list (list 3 4 5 6))

(define (square-listt items)
  (define (iter things answer)
    (display things )
    (display answer )
    (newline)
    (if (null? things)
        answer
        ; bura deisk
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))
;(cons (cons nil 9) 16)
;; (square-listt (list 3 4 5 6))

(define (square-listtt items)
  (display items)
  (cond
    ((null? items) nil)
    (else
     (cons (square (car items)) (square-listtt (cdr items))))))

;; (square-listtt (list 3 4 5 6))

(define (square-listttt items)
  (define (iter l pick)
    (define r (square (car l)))
    (if (null? (cdr l))
        (pick (list r))
        (iter (cdr l) (lambda (x) (pick (cons r x))))))
  (iter items (lambda (x) x)))

(define (reverse l)
  (define (iter remaining result)
    (if (null? remaining)
        result
        (iter (cdr remaining) (cons (car remaining) result))))
  (iter l nil))
;; (reverse (list 3 4 5 6))

;; Based on this we can say that what Louis has done is to simply extend the 
;; reverse procedure so that it squares each value during the reversal procedure.
;; He's starting with an empty list as the result and then going through the list 
;; to square starting with the head, squaring the current value and putting the
;; result onto the head of the result list. So the square of the first element
;; becomes the last item in the list, the square of the second element becomes the
;; second-last item in the list and so on.

