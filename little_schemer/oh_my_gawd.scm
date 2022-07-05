#lang sicp

(define atom?
  (lambda (l)
    (and (not (pair? l)) (not (null? l)))))

;; (define rember*
;;   (lambda (a l)
;;     (cond
;;       ((null? l ) (quote ()))
;;       ((atom? (car l))
;;        (cond
;;          ((eq? (car l ) a)
;;           (rember* a (cdr l)))
;;          (else (cons (car l)
;;                      (rember* a (cdr l))))))
;;       (else (cons (rember* a (car l))
;;                   (rember* a (cdr l)))))))
;;
;; (rember* 'sauce '((tomato sauce)
;;                   ((bean) sauce)))


(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
         ((eq? (car l) old)
          (cons new (cons old (insertR* new old (cdr l)))))
         (else (cons (car l) (insertR* new old (cdr l))))))
      (else (cons (insertR* new old (car l))
                  (insertR* new old (cdr l)))))))

(insertR* 'roast 'chuck '((how much (wood))
                          could
                          ((a (wood) chuck))
                          (((chuck)))
                          (if (a) ((wood chuck)))
                          could chuck wood))

(define insertR**
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
         ((eq? (car l) old)
          (cons old
                (cons new
                      ( insertR* new old
                                 (cdr l)))))
         (else (cons (car l)
                     ( insertR* new old
                                (cdr l))))))
      (else (cons (insertR* new old
                            (car l))
                  ( insertR* new old
                             (cdr l)))))))
(insertR** 'roast 'chuck '((how much (wood))
                           could
                           ((a (wood) chuck))
                           (((chuck)))
                           (if (a) ((wood chuck)))
                           could chuck wood))
(define add1
  (lambda (x)
    (+ x 1)))

(define occur*
  (lambda (a l)
    (cond
      ((null? l )   0)
      ((atom? (car l))
       (cond
         ((eq? (car l) a)
          (add1 (occur* a (cdr l))))
         (else (occur* a (cdr l)))))
      (else (+ (occur* a (car l))
               (occur* a (cdr l)))))))

;; (define occur**
;;   (lambda (a l)
;;     (cond
;;       (( null? l) 0)
;;       ((atom? (car l))
;;        (cond
;;          ((eq? (car l) a)
;;           (add1 (occur* a (cdr l))))
;;          (else (occur* a (cdr l)))))
;;       (else (+ (occur* a (car l))
;;                (occur* a (cdr l)))))))
(occur* 'banana '((banana)
                  (split ((((banana ice)))
                          (cream (banana))
                          sherbet)
                         ()
                         (bread)
                         (banana brandy))))
;; The First Commandment
;; (final version)
;; When recurring on a list of atoms, lat, ask two questions
;; about it: (null? tat) and else.
;; When recurring on a number, n, ask two questions about
;; it: (zero? n) and else.
;; When recurring on a list of S-expressions, l, ask three
;; question about it: (null? l), (atom? (car l», and else.
;;
;; (car '(
;;        (tomato sauce)
;;        ((bean )
;;         sauce)
;;        ))  ;=> (tomato sauce) not atom go next
;(cdr '((tomato sauce) ((bean) sauce)))
; (cdr '((tomato
; (cons (rember* 'sauce '(tomato sauce)
;       (rember* 'sauce (

(define subst*
  (lambda (new old l)
    (cond
      ((null? l ) (quote ()))
      ((atom? (car l))
       (cond
         ((eq? old (car l))
          (cons new (subst* new old (cdr l))))
         (else (cons (car l) (subst* new old (cdr l))))))
      (else (cons (subst* new old (car l))
                  (subst* new old (cdr l)))))))

;; (subst* 'pecker 'chuck ' ((how much (wood))
;;                           could
;;                           ((a (wood) chuck))
;;                           (( (chuck)))
;;                           (if (a) ((wood chuck)))
;;                           could chuck wood))

(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) (quote()))
      ((atom? (car l))
       (cond
         ((eq? old (car l))
          (cons new (cons old (insertL* new old (cdr l)))))
         (else (cons (car l ) (insertL* new old (cdr l))))))
      (else (cons (insertL* new old (car l))
                  (insertL* new old (cdr l)))))))

(insertL* 'pecker 'chuck '((how much (wood))
                           could
                           ((a (wood) chuck))
                           (( (chuck)))
                           (if (a) ((wood chuck)))
                           could chuck wood))

(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
       (cond
         ((eq? a (car l))
          (or #t))
         (else (or #f (member* a (cdr l))))))
      (else (or (member* a (car l))
                (member* a (cdr l)))))))

(define member*with-better-syntax
  (lambda (a l)
    (cond
      ((null? l ) #f)
      ((atom? (car l))
       (or (eq? (car l) a)
           (member*with-better-syntax a (cdr l))))
      (else (or (member*with-better-syntax a (car l))
                (member*with-better-syntax a (cdr l)))))))

(member*with-better-syntax 'chipsss '((potato) (chips ((with) fish) (chips))))

(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))

(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2))
       (= a1 a2))
      ((or (number? a1) (number? a2))
       #f)
      (else (eq? a1 a2)))))


(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((and (null? l1) (atom? (car l2)))
       #f)
      ((null? l1) #f)
      (( and (atom? (car l1)) (null? l2))
       #f)
      ((and (atom? (car l1))
            (atom? (car l2)))
       (and (eqan? (car l1) (car l2))
            ( eqlist? (cdr l1) (cdr l2))))
      (( atom? (car l1)) #f)
      ((null? l2) #f)
      ((atom? (car l2)) #f)
      (else
       (and (eqlist? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2)))))))
