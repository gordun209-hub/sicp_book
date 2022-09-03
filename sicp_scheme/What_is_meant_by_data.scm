#lang sicp
;; That starts with exercise 2.3
;; "Internal accessors", not to be used directly by clients.  Not sure
;; how to signify this in scheme.
;; TODO incele?
(define (bottom-left rect) (car rect))
(define (bottom-right rect)
  (make-point (x-point (cdr rect))
              (y-point (car rect))))
(define (top-left rect)
  (make-point (x-point (car rect))
              (y-point (cdr rect))))
(define (top-right rect) (cdr rect))

(define (width-rect rect)
  (abs (- (x-point (bottom-left rect))
          (x-point (bottom-right rect)))))
(define (height-rect rect)
  (abs (- (y-point (bottom-left rect))
          (y-point (top-left rect)))))

;; Public methods.
(define (area-rect rect)
  (* (width-rect rect) (height-rect rect)))
(define (perimeter-rect rect)
  (* (+ (width-rect rect) (height-rect rect)) 2))


;; Usage:
(define r (make-rect (make-point 1 1)
                     (make-point 3 7)))
(area-rect r)
(perimeter-rect r)


;; ---------

;; Alternate implementation of rectangle.  Note that this would screw
;; up clients that call make-rect directly, since it uses a different
;; number of args and different arg meanings, but it's generally bad
;; form for clients to call constructors directly anyway, they should
;; call some kind of factory method (cf "Domain Driven Design").

;; assuming, not checking width, height > 0.
(define (make-rect bottom-left width height)
  (cons bottom-left (cons width height)))

;; (define (height-rect rect) (cdr (cdr rect)))
;; (define (width-rect rect) (car (cdr rect)))

;; area and perimeter ops remain unchanged.  The internal methods from
;; the first implementation won't work now.


;; Usage for second implementation:
;; (define r (make-rect (make-point 1 1) 2 6))
;; (area-rect r)
;; (perimeter-rect r)

;; Alternative Implementation II
;; -----------------------------
;;
;; The above implementations are limited to rectangles that have sides
;; parallel to the major axes of the plane. This implementation generalizes
;; to allow all rectangles. Conveniently enough, you can still use the above
;; area and perimeter definitions. Abstraction barrier for the win!
;;
;; DO NOTE -- As above all sanity/error checking has been ignored. IRL, you
;; you would want to ensure that parallel sides are actually parallel, etc.

;; Helpful to have this
(define (square x) (* x x))

;; Point library
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (point-dist p1 p2)
  (sqrt (+ (square (- (x-point p1) (x-point p2)))
           (square (- (y-point p1) (y-point p2))))))

;; Segment library
(define (make-segment p1 p2) (cons p1 p2))
(define (start-seg p) (car p))
(define (end-seg p) (cdr p))
(define (seg-len seg) (point-dist (start-seg seg)
                                  (end-seg seg)))

;; Rectangle library
;; (define (make-rect side parallel-side)
;;   (cons side parallel-side))
(define (side1 rect) (car rect))
(define (side2 rect) (cdr rect))
(define (side-legths rect)
  (cons (seg-len (side1 rect))
        (min (abs (point-dist (start-seg (side1 rect))
                              (start-seg (side2 rect))))
             (abs (point-dist (start-seg (side1 rect))
                              (end-seg (side2 rect)))))))

;; Same as above
;; (define (width-rect rect) (car (side-legths rect)))
;; (define (height-rect rect) (cdr (side-legths rect)))

;; Usage
;; (define r (make-rect (make-segment (make-point 0 1)
;;                                 (make-point 0 0))
;;                   (make-segment (make-point 1 0)
;;                                 (make-point 1 1))))

;; As an alternative to this alternative, You can define you rectangles
;; as a pair of perpendicular segments:

;; (define (make-rect side perpendicular-side)
;;   (cons side perpendicular-side))
;; (define (side-legths rect)
;;   (cons (seg-len (side1 rect))
;;         (seg-len (side2 rect))))

;; And everything should still work.

;; Thus we now have 4 representations for rectangles, all of which can use the
;; same area and perimeter functions.
