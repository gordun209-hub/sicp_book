#lang sicp
(#%require sicp-pict)
(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

;(define wave4 (flipped-pairs einstein))
(define (two-pairs painter)
  (below painter painter))

;(paint (two-pairs einstein))
;(paint (below einstein einstein))
;(paint einstein )

;(paint (flipped-pairs einstein))
;(right-split einstein 3)
;smaller = (right-split einstein 2)
;(beside einstein (below (right-split einstein 2)
;                        (right-split einstein 2))

; smaller = (right-split einstein 1)
;(beside einstein (below (beside einstein
;                                (below right-split einstein 1)
;                                (below right-split einstein 1)
;
;(beside einstein (below (beside einstein (beside einstein (                         

;(paint (beside einstein (below einstein einstein)))

;(paint (flipped-pairs einstein))
; beside = yanina koy
; below altina koy

(define einstein2 (beside einstein (flip-vert einstein)))
;(paint einstein2)
(define einsten4 (below einstein2 einstein2))
;(paint einsten4)
;(paint (flipped-pairs einstein))
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))
;(paint (right-split einstein 3))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (beside painter (below smaller smaller)))))



(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

;(paint (corner-split einstein 10))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half ) half))))

;(paint (square-limit einstein 10))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(paint ((square-of-four identity flip-vert
                        identity flip-vert) einstein))


(define (split f g) 
  (define (rec painter n) 
    (if (= n 1) 
        painter 
        (let ((smaller (rec painter (- n 1)))) 
          (f painter (g smaller smaller))))) 
  rec)


