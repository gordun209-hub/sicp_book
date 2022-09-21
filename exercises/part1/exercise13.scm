#lang sicp

;; write procedure for finding pascal triangle
;; simply if c = 1 or r = c return 1
;; else sum up
(define (pascal r c)
  (if (or (= c 1) (= c r))
      1
      (+ (pascal (- r 1) (- c 1)) (pascal (- r 1) c))))

(pascal 5 3)
;; (+ (pascal 4 2) (pascal 4 3))
;; (+ (+ (pascal 3 1) (pascal 3 2)) (+ (pascal 3 2) ..

;; this is efficent
;; TODO anlasam ii olr
(define (pascals-triangle number row)
  (define (factorial n)
    (define (iter n acc)
      (if (< n 2) acc
          (iter (- n 1) (* n acc))))
    (iter n 1))
  (/
   (factorial (- row 1))
   (* (factorial (- number 1))
      (factorial (- (- row 1)
                    (- number 1))))))
