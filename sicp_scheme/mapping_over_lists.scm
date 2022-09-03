#lang sicp

(define (scale-list items factor)
  (if (null? items)
      nil
      (cons (* (car items) factor)
            (scale-list (cdr items)
                        factor))))

;; (scale-list (list 1 2 3 4 5) 10)


(define (map proc items)
  (cond ((null? items) '())
        (else
         (cons (proc (car items)) (map proc (cdr items))))))

;; (map abs (list -10 2 3))

(define (scale-list-with-map items factor)
  (map (lambda (x) (* x factor)) items))

;; (define (square-list list)
;;   (map (lambda (x) (* x x)) list))

;; (square-list (list 1 2 3 4))

(define (square-list-without-map items)
  (if (null? items) nil (cons
                         (* (car items) (car items))
                         (square-list-without-map (cdr items)))))

;; (square-list-without-map (list 1 2 3 4))
;; Revise this TODO
;; Exercise 2.22
(define square (lambda (x) (* x x)))
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))

(define (square-list-2 items)
  (define (iter things answer)
    (if (null? things) answer
        (iter (cdr things)
              (cons answer (square (car things))))))
  (iter items nil))


;; (square-list (list 1 2 3 4))
;; (square-list-2 (list 1 2 3 4))


(define (for-each proc ls)
  (cond ((null? ls) nil)
        (else 
          (proc (car ls)) (for-each  proc (cdr ls)))))

(for-each (lambda (x) (newline) (display x)) (list 57 321 88))



