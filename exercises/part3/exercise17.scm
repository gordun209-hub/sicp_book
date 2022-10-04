#lang sicp


;; (define (count-pairs x)
;;   (if (not (pair? x))
;;     0
;;     (+ (count-pairs (car x))
;;        (count-pairs (cdr x))
;;        1)))

(define (count-pairs x)
  (let ((counted '()))
    (define (uncounted? x)
      (if (memq x counted)
          0
          (begin
            (set! counted (cons x counted))
            1)))

    (define (count x)
      (if (not (pair? x))
          0
          (+ (count (car x))
             (count (cdr x))
             (uncounted? x))))
    (count x)))

