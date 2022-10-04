#lang sicp


(define (make-monitored proc)
  (let ((count 0))
     (lambda (input)
         (cond ((number? input) (begin (set! count (+ count 1))
                                       (proc input)))
               ((eq? input 'how-many-calls?) count)))))

(define s (make-monitored sqrt))

(s 100) ;; 10
(s 'how-many-calls?) ;; 1
