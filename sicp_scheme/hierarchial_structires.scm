#lang sicp

(define x (cons (list 1 2) (list 3 4)))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define y (list 1 2 3))
(define z (list 4 5 6))

;; (append y z) ;; (1 2 3  4 5 6)
;; (cons y z);; ((1 2 3) 4 5 6)
;; (list y z) ;; ((1 2 3) (4 5 6))


;; (reverse (list 1 2 3))
;;
;; (define nil '())
;;
;; (define (reverse items)
;;   (define (iter items result)
;;     (if (null? items)
;;         result
;;         (iter (cdr items) (cons (car items) result))))
;;  (iter items nil))


(define (deep-reverse tree)
  (define (iter items result)
    (cond ((null? items) result)
          ((not (pair? items)) (iter (cdr items) (cons items result)))
          (else (iter (cdr items) result))))
  (iter tree nil))

(deep-reverse '((3 4) (1 2)))

;; TODO anla
(define (eli-deep-reverse lst)
  (cond ((null? lst) nil)
        ((pair? (car lst))
         (append
          (eli-deep-reverse (cdr lst))
          (list (eli-deep-reverse (car lst)))))
        (else
         (append
          (eli-deep-reverse (cdr lst))
          (list (car lst))))))

(eli-deep-reverse x)


