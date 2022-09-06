#lang racket
;; aldigi row col ve resti consluyo?
(define (adjoin-position row col rest)
  (cons (list row col) rest))

(define (check a b)    ; returns true if two positions are compatible
  (let ((ax (car a))   ; x-coord of pos a
        (ay (cadr a))  ; y-coord of pos a
        (bx (car b))   ; x- coord of pos b
        (by (cadr b))) ; y-coord of pos b
    (and (not (= ax bx)) (not (= ay by))  ; checks col / row
         (not (= (abs (- ax bx)) (abs (- ay by))))))) ; checks diag



(define (accumulate op initial sequence)
  (if (null? sequence)
      ;; burda nil yani initial sona gidiyo
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (safe? y)
  (= 0 (accumulate + 0
                   (map (lambda (x)
                          (if (check (car y) x) 0 1))
                        (cdr y)))))
(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))


(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list '())
        (filter
         (lambda (positions) (safe? positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                    new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(length (queens 8))  ; 92

