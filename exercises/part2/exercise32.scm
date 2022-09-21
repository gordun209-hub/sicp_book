#lang sicp
;; (define (subsets s)
;;   (if (null? s)
;;       (list nil)
;;       ;;
;;       (let ((rest (subsets (cdr s))))
;;         (append rest (map (lambda (x) (cons x x)) rest)))))




(define (subsets s)
  (if (null? s)
      (list nil)   ;; initially had nil, always got () back!
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(subsets (list 1 2 3))
(subsets (list 1 2 3 4 5))
