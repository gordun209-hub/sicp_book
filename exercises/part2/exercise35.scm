#lang sicp


(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define (count-leaves t)

  ;; 12093120391 IQ
  (accumulate + 0 (map (lambda (x) (if (pair? x)
                                       (length x)
                                       1)) t)))

(count-leaves (list  (list 2 3) (list 4 5)))
(count-leaves (list (list 3 4 5) (list 2 3)))
