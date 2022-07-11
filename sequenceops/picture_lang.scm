#lang sicp
(#%require sicp-pict)
(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

;(define wave4 (flipped-pairs wave))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))
;(paint (right-split einstein 3))

;(paint (flipped-pairs einstein))
; beside = yanina koy
; below altina koy

(define einstein2 (beside einstein (flip-vert einstein)))
;(paint einstein2)
(define einsten4 (below einstein2 einstein2))
(paint einsten4)
(paint (flipped-pairs einstein))
