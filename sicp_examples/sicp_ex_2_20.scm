#lang scheme

;; e procedures +, *, and list take arbitrary
;; numbers of arguments. One way to define such procedures
;; is to use define with doed-tail notation. In a procedure
;; definition, a parameter list that has a dot before the last pa-
;; rameter name indicates that, when the procedure is called,
;; the initial parameters (if any) will have as values the initial
;; arguments, as usual, but the final parameter’s value will be
;; a list of any remaining arguments. For instance, given the
;; definition

; (define (f x y . z) <body>)
; the procedure f can be called with two or more arguments
; if we evaluate
; (f 1 2 3 4 5 6)

; then in the body of f, x will be 1, y will be 2, and z will be list ( 3 4 5 6)
; given the the definition
;(define (g . w) <body>)

; the procedure g can be called with zero or more args
; (g 1 2 3 4 5 6)
; then in the body of g, w will be the list (1 2 3 4 5 6)
; use this notation to write a procedure same-parity that takes one or more
; integers and returns a list of all the arguments that have same even-odd parity
; as the first argument. for example


; (same-parity 1 2 3 4 5 6 7)
; (1 3 5 7)
; (same-parity 2 3 4 5 6 7)

(define (same-parity first . rest)
  (define (same-parity-iter source dist remainder-val)
    (if (null? source)
        dist
        (same-parity-iter (cdr source)
                          (if (= (remainder (car source) 2) remainder-val)
                              (append dist (list (car source)))
                              dist)
                          remainder-val)))

  (same-parity-iter rest (list first) (remainder first 2)))

; (2 4 6)
