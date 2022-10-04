#lang sicp

(define (cycle? x) 
  (define visited nil) 
  (define (iter x) 
    (set! visited (cons x visited)) 
    (cond ((null? (cdr x)) false) 
          ((memq (cdr x) visited) true) 
          (else (iter (cdr x))))) 
  (iter x)) 
