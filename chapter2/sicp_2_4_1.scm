#lang sicp
;; Helpers

(define square
  (lambda (x) (* x x)))

(define (add-complex z1 z2)
  ; takes two complex numbers and adds
  ;  (real-part z1 + real-part z2) , (imag-part z1 + imag-part z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

;; First Representation

; simply cons
; make imaginary number with magnitude and angle
; multiply magnitude with cos(a) and multply magnitude with sin a then cons em
;; (define (make-from-mag-ang r a)
;;   (cons (* r (cos a)) (* r (sin a))))



; Second representation
(define (real-part2 z) (* magnitude z) (cos (angle z)))

(define (imag-part2 z) (* (magnitude z) (sin (angle z))))

(define (magnitude2 z) (car z))

(define (angle2 z) (cdr z))

(define (make-from-real-imag2 x y)
  (cons (sqrt (+ (square x) (square y)))))

(define (make-from-mag-ang2 r a) (cons r a))



(define (attach-tag type-tag contents)
  (cons type-tag contents))


(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))

(define (contents datum)
  (cdr datum)
  (error "Bad tagged datum: CONTENTS" datum))


; these uses car of the structure for identifying shape
(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z) (eq? (type-tag z) 'polar))


; solve conflict between two procedures with suffix

(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))

(define (make-from-mag-ang-rectangular r a )
  (attach-tag 'rectangular
              (cons (* r (cos a)) (* r (sin a)))))



(define (real-part-generic z)
  (cond ((rectangular? z)
         (real-part-rectangular (contents z)))
        ((polar? z)
         (real-part-polar (contents z)))
        (else (error "Unknown type: REAL-PART" z))))


(define (imag-part-generic z)
  (cond ((rectangular? z)
         (imag-part-rectangular (contents z)))
        ((polar? z)
         (imag-part-polar (contents z)))
        (else (error "Unknown type: IMAG-PART" z))))


(define (magnitude-generic z)
  (cond ((rectangular? z)
         (magnitude-rectangular (contents z)))
        ((polar? z)
         (magnitude-polar (contents z)))
        (else (error "Unknown type :MAGNITUDE" z))))

(define (angle-generic z)
  (cond ((rectangular? z)
         (angle-rectangular (contents z)))
        ((polar? z)
         (angle-polar (contents z)))
        (else (error "Unknown type: ANGLE" z))))


;; (define (make-from-real-imag x y)
;;   (make-from-real-imag-rectangular x y))
;;
;; (define (make-from-mag-ang r a)
;;   (make-from-mag-ang-polar r a))


; ozet

; Operations : real-part, imag-part, magnitude, angle
; Types
; Polar : real-part-polar, imag-part-polar, magnitude-polar, angle-polar
; Rectangular: real-part-rectangular, imag-part-rectangular,
; magnitude-rectangular, angle-rectangular

; Operations
;; (define (real-part z ) (car z ))
;; (define (imag-part z ) (cdr z))
;; (define (magnitude z)
;;   (sqrt (+ (square (real-part z))
;;            (square (imag-part z)))))
;; (define (angle z)
;;   ; angle of complex number equals to arctan of ima-part z and real-part z
;;   (atan (imag-part z) (real-part z)))

; Polar type
(define (real-part-polar z)
  (* (magnitude-polar z) (cons (angle-polar z))))

(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))

(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))
(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
              (cons (sqrt (+ (square x) (square y)))
                    (atan y x))))


(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))

;Rectangular type
(define (real-part-rectangular z ) (car z))

(define (imag-part-rectangular z) (cdr z))

(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))

(define (angle-rectangular z)
  (atan (imag-part-rectangular z)
        (real-part-rectangular z)))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation - TABLE" m))))
    dispatch))

(define operation-table (make-table))

(define get (operation-table 'lookup-proc))

(define put (operation-table 'insert-proc!))

(define (install-rectangular-package)
  ;; internal procedures
  ;; pickers
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  ; constructors
  (define (make-from-real-imag x y) (cons x y))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; tag constructor i think
  (define (tag x) (attach-tag 'rectangular x))
  ;; idk what they are TODO kinda sus gone fortnite
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)

  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures

  ;; primitive procs
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (real-part z) (* (magnitude z) (cos (angle z))))
  (define (imag-part z) (* (magnitude z) (sin (angle z))))

  ;; constructor procs
  (define (make-from-mag-ang r a) (cons r a))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (apply-generic op . args)
  ; type-tags = map type-tag to args
  (let ((type-tags (map type-tag args)))
    ;; procedure eq get op from type-tags
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No method for these types: APPLY-GENERIC"
                 (list op type-tags))))))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))


(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))


(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))


(define (make-from-mag-ang-msg r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else (error "Unkown op --- MAKE-FROM-MAG-ANG" op))))
  dispatch)

;; Exercise 2.75: Implement the constructor make-from-mag-
;; ang in message-passing style. is procedure should be anal-
;; ogous to the make-from-real-imag procedure given above.

;; Exercise 2.76: As a large system with generic operations
;; evolves, new types of data objects or new operations may
;; be needed. For each of the three strategies—generic opera-
;; tions with explicit dispatch, data-directed style, and message-
;; passing-style—describe the changes that must be made to a
;; system in order to add new types or new operations. Which
;; organization would be most appropriate for a system in
;; which new types must oen be added? Which would be
;; most appropriate for a system in which new operations
;; must oen be added?
