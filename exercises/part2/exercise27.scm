#lang sicp 

(define x (list (list 1 2) (list 3 4)))

;;(reverse x)
;; ((3 4) (1 2))

(define (deep-reverse lst)
  (map reverse lst))

(define (eli-deep-reverse lst) 
  (cond ((null? lst) nil) 
        ((pair? (car lst)) 
         (append 
          (eli-deep-reverse (cdr lst)) 
          (list (eli-deep-reverse (car lst))))) 
        (else 
         (append 
          (eli-deep-reverse (cdr lst)) 
          (list (car lst))))) 
  
 (eli-deep-reverse x))
(deep-reverse x)
