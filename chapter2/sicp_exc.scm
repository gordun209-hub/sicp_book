#lang sicp
((lambda (x y z) (+ x y  (lambda (z) z z) ) 3) 1 2 3)
