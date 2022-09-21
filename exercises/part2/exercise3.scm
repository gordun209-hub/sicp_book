#lang sicp

;; Impelemnt a representation for rectangles in a plane

;; create constructors and selectors 
;; (define x-cord car)
;; (define y-cord cdr)
;; (define make-cord cons)
;;
;; (define x-axis-cord (make-cord 1 1))
;; (define y-axis-cord (make-cord 4 5))
;;
;; (define make-rectangle cons)
;; (define first-point car)
;; (define second-point cdr)
;;
;; (define myrect (make-rectangle x-axis-cord y-axis-cord))
;; myrect
;; (first-point myrect)
;; (second-point myrect)
;; (define (area rect)
;;   (let ((vertical-length (+ (first-point (x-cord rect))
;;                             (second-point (x-cord rect))))
;;         (horiz-length (+ (first-point (y-cord rect))
;;                          (second-point (y-cord rect)))))
;;     (* horiz-length vertical-length)))
;;
;; (area myrect)
                         
                         

 ;; ex 2.3.  Not bothering with error/sanity checking. 
  
 ;; Point 
(define (make-point x y) (cons x y)) 
(define (x-point p) (car p)) 
(define (y-point p) (cdr p)) 
 
;; Rectangle - 1st implementation 
 
(define (make-rect bottom-left top-right) 
  (cons bottom-left top-right)) 
 
;; "Internal accessors", not to be used directly by clients.  Not sure 
;; how to signify this in scheme. 
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

