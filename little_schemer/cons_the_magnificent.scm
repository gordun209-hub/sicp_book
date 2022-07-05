#lang scheme
; Building abstractions with data
(define (linear-combination a b x y)
  (+
   (* a x)
   (* b y)))

;; (define (lat? l)
;;   (if ((pair? (car l)) #f)
;;       (and (lat? (cdr l))))
;;   else #t)
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

; True or false: (lat? l) where
; l is (Jack Sprat could eat no chicken fat)
;true
;; (lat? '(bacon and eggs))
; ((atom? (car (bacon and eggs)) (lat? (cdr l )))
; ((atom? (bacon) -> #t
; (lat? (and eggs))
; ((atom? (car (and eggs)) (lat? (eggs))
; #t
; (lat? (eggs))
; ((atom? eggs (lat? ()))
; #t
;  (lat? ())
; ((null? () #t))
; #t

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or
             (eq?
              (car lat)
              a)
             (member? a
                      (cdr lat)))))))

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? a (car lat)) (cdr lat))
      (else (cons (car lat) (rember a (cdr lat)))))))

(define not-mine-rember-also-false
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((eq? (car lat) a) (cdr lat))
              (else (rember a
                            (cdr lat))))))))
;;
;; (not-mine-rember-also-false 'and '(bacon lettuce and tomato))

;; (define firsts
;;   (lambda (lat)
;;     (cond
;;       ((null? lat) (quote ()))
;;       ((pair? (car lat))  (firsts (car lat)))
;;       (else (cons (car (car lat)) (firsts (cdr lat)))))))

; (firsts '((a b) (c d) (e f)))
; ( pair? (a b) (firsts (a b)))
;
;; (firsts '((apple peach pumpkin)
;;           (plum pear cherry)
;;           (grage raisin pea)
;;           (bean carrot eggplant)))
;;
;; (firsts '((a b) (c d) (e f)))
;;

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? old (car lat)) (cons old (cons  new (cdr lat))))
      (else (cons (car lat) (insertR new old (cdr lat)))))))

;; (insertR 'topping 'fudge '(ice cream with fudge for dessert))

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? old (car lat)) (cons new (cons old (cdr lat))))
      (else (cons (car lat) (insertR new old (cdr lat)))))))


(define insertLls
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((eq? (car lat) old)
               (cons new
                     (lat)))
              (else (cons (car lat)
                          (insertL new old
                                   (cdr lat)))))))))



;; (insertLls 'topping 'cream '(ice cream with fudge for dessert))
;; (insertR 'topping 'cream '(ice cream with fudge for dessert))

(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old) (cons new (cdr lat)))
      (else (cons (car lat) (subst new old (cdr lat)))))))
;; (subst 'topping 'fudge '(ice cream with fudge for dessert))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) (quote ()))
      ((or (eq? o1 (car lat)) (eq? o2 (car lat))))
      (cons new (cdr lat))
      (else (cons (car lat) (subst2 o1 o2 (cdr lat)))))))

;; (subst 'chocolate 'banana '(banana ice cream with chocolate topping))
(define doeseq?
  (lambda (a lat)
    ((eq? a (car lat)))))

(define miltirember
  (lambda (a lat)
    (cond
      ; predicate  ; result
      ((null? lat) (quote ()) )
      ((eq? a (car lat))
       (miltirember a (cdr lat)))
      (else
       (cons
        (car lat)
        (miltirember a
                     (cdr lat)))))))

; TEST
;; (miltirember 'cup '(coffe cup tea cup and hick cup))


(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat)
       (quote()))
      ((eq? old (car lat))
       (cons old
             (cons new
                   (multiinsertR new old
                                 (cdr lat)))))
      (else
       ( cons
         (car lat)
         (multiinsertR new old (cdr lat)))))))

;; (multiinsertR 'fried 'fish '(chips and fish or fish and fried))


(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat)
       (quote()))
      ((eq? old (car lat))
       (cons new (cons old (multiinsertL new old (cdr lat)))))
      (else
       (cons
        (car lat)
        (multiinsertL new old (cdr lat )))))))


;; (multiinsertL 'fried 'fish '(chips and fish or fish and fried))


(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat)
       (quote ()))
      ((eq? old (car lat))
       (cons new (multisubst new old (cdr lat))))
      (else (cons (car lat) (multisubst new old (cdr lat)))))))

