#lang sicp

(define (scale-list items factor)
  (if (null? items)
      nil
      (cons (* (car items) factor)
            (scale-list (cdr items)
                        factor))))

(scale-list (list 1 2 3 4 5) 10) ; -> (10 20 30 40 50)

;; e can abstract this general idea and capture it as a common paern
;; expressed as a higher-order procedure, just as in Section 1.3. e higher-
;; order procedure here is called map. map takes as arguments a procedure
;; of one argument and a list, and returns a list of the results produced by
;; applying the procedure to each element in the list

(map + (list 1 2 3) (list 40 50 60) (list 700 800 900)) ; -> (741 852 963)

(map (lambda (x y) (+ x (* 2 y)))
     (list 1 2 3)
     (list 4 5 6))


;; Now we can give a new definition of scale-list in terms of map:
;; (define (scale-list items factor)
;; (map (lambda (x) (* x factor))
;; items))
(define (square-list list)
  (map (lambda (x) (* x x)) list))

(square-list (list 1 2 3 4))

(define (square-list-without-map list)
  (if (null? list)
      nil
      (cons (* (car list) (car list) ) (square-list-without-map (cdr list)))))

(square-list-without-map (list 1 2 3 4))
