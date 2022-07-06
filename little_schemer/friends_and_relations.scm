#lang sicp
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))

(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      (else
       (cond
         ((member? (car lat) (cdr lat))
          #f)
         (else (set? (cdr lat))))))))


(define makeset
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      ((member? (car lat)(cdr lat))
       (makeset (cdr lat)))
      (else (cons (car lat)
                  (makeset (cdr lat)))))))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? a (car lat))
       (multirember a (cdr lat)))
      (else
       (cons (car lat) (multirember a (cdr lat)))))))

(define makesett
  (lambda (lat )
    (cond
      ; null ise quote
      ((null? lat) (quote()))
      ; deilse car lat i consla + makeset with all car lat removed
      (else (cons (car lat)
                  (makeset
                   (multirember (car lat)
                                (cdr lat))))))))
;; (set? '(apple 3 pear 4 9 ))
;;
;; (makesett '(apple peach pear peach plum apple lemon peach))

(define subset?
  (lambda (set1 set2)
    (cond
      ; null ise #t
      ((null? set1) #t)
      ; deilse
      (else (cond
              ; member mi diye bak memberse subseti cair deilse zaten
              ; subset deildir false dondr
              ((member? (car set1) set2)
               (subset? (cdr set1) set2))
              (else #f))))))

(define shorter-subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      ((member? (car set1) set2)
       (subset? (cdr set1) set2))
      (else #f))))

(define subset-with-and
  (lambda (set1 set2)
    ((null? set1) #t)
    ((and
      (member? (car set1) set2)
      (subset? (cdr set1) (set2))))))

(define eqset?
  (lambda (set1 set2)
    (cond
      ((subset? set1 set2)
       (subset? set2 set1))
      (else #f))))

(define shorter-eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2) (subset? set2 set2))))

(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else
       (cond
         ((member? (car set1) set2) #t)
         (else (intersect?
                (cdr set1) set2)))))))

(define shorter-intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      ((member? (car set1) set2) #t)
      (else (shorter-intersect? (cdr set1) set2)))))

(define intersect-with-or
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else (or (member? (car set1) set2)
                (intersect-with-or
                 (cdr set1) set2))))))

(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2)
       (union (cdr set1) set2))
      (else (cons (car set1))
            (union (cdr set1) set2)))))


(define reverse-intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) (quote ()))
      ((member? (car set1) set2))
      (else (cons (car set1)
                  (reverse-intersect (cdr set1) set2))))))
(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) (quote ()))
      ((member? (car set1) set2)
       (cons (car set1)
             (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2)))))

(define intersectall
  (lambda (l)
    (cond
      ((null? (cdr l)) (car l))
      (else (intersect (car l)
                       (intersectall (cdr l)))))))

(intersectall '((a b c) (c a d e) (e f g h a b)))

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define a-pair?
  (lambda (x)
    (cond
      ((atom? x) #f)
      ((null? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cdr (cdr x))) #t)
      (else #f))))

(define first
  (lambda (p)
    (car p)))
(define second
  (lambda (p)
    (cdr p)))

(define build
  (lambda (x y)
    (cons x (cons y (quote())))))

(define firsts
  (lambda (l)
    (cond
      ((null? l) (quote()))
      (else (cons (car (car l))
                  (firsts (cdr l)))))))

(define seconds
  (lambda (l)
    ( cond
       ((null? l) (quote()))
       (else (cons  (car (cdr (car l))) (seconds (cdr l)))))))


(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define revrel
  (lambda (rel)
    (cond
      ((null? rel) (quote()))
      (else (cons (build
                   (second (car rel))
                   (first (car rel)))
                  (revrel (cdr rel)))))))

(define revrel-v2
  (lambda (rel)
    (cond
      ((null? rel) (quote()))
      (else (cons (cons
                   (car (cdr (car rel)))
                   (cons (car (car rel)))
                   (quote())))
            (revrel (cdr rel))))))
(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

(define revrel-v3
  (lambda (rel)
    (cond
      ((null? rel) (quote()))
      (else (cons (revpair (car rel)) (revrel-v3 (cdr rel)))))))

(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))

(fullfun? '((grape raisin) (plum prune) (stewed grape)))
