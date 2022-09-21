#lang sicp

;; Obsere that our model of evaluation allows for combinations
;; whose operators are compound expressions.
;; Describe the behavior of the following procedure
(define (a-plus-abs-b a b)
  ;; if b is bigger than 0 return + else - so result is
  ;;  > b 0 ? + a b
  ;; <= b 0 ? - a b
  ((if (> b 0) + -) a b))

(a-plus-abs-b 2 -3)
