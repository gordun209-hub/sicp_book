#lang sicp
;; Instructions

;; Name of data-path button to push to assign a value to a register

;; A test instruction, that performs a specified test

;; A conditional branch to a location indicated by controller label,
;; based on the result of the previous test
;; If the test is false controller should contionue with the next instruction
;; in the sequence. Otherwise, controller should continue with the instruction
;; after the label

;; An unconditional branch (goto instruction) naming a controller label at which
;; to continue execution

;; spesification of gcd machine

;; (data-paths
;;   (registers
;;     ((name a)
;;      (buttons ((name a<-b) (source (register b)))))
;;     ((name b)
;;      (buttons ((name b<-t) (source (register t)))))
;;     ((name t)
;;      (buttons ((name t<-r) (source (operation rem))))))
;;   (operations
;;     ((name rem) (inputs (register a) (register b)))
;;     ((name =) (inputs (register b) (constant 0)))))
;; (controller
;;   test-b  ;;label
;;   (test =);; test
;;   (branch (label gcd-done));;conditional branch
;;   (t<-r);; button push
;;   (a<-b);; button push
;;   (b<-t);; button push
;;   (goto (label test-b));;unconditional branch
;;   gcd-done);;label

;; Improved

;; (controller
;;   test-b
;;   (test (op =) (reg b) (const 0))
;;   (branch (label gcd-done))
;;   (assign t (op rem) (reg a) (reg b))
;;   (assign a (reg b))
;;   (assign b (reg t))
;;   (goto (label test-b))
;;   gcd-done)
(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2))
(define (remainder n d)
  (if (< n d)
      n
      (remainder (- n d) d)))
(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))


(define gcd-machine
  (make-machine
   '(a b t)
   (list (list 'rem remainder) (list '= =))
   '(test-b (test (op =) (reg b) (const 0))
            (branch (label gcd-done))
            (assign t (op rem) (reg a) (reg b))
            (assign a (reg b))
            (assign b (reg t))
            (goto (label test-b))
            gcd-done)))

(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each
     (lambda (register-name)
       ((machine 'allocate-register) register-name))
     register-names)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))


(define (make-register name)
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value) (set! contents value)))
            (else
             (error "Unknown request: REGISTER" message))))
    dispatch))
(define (get-contents register) (register 'get))
(define (set-contents! register value)
  ((register 'set) value))

(define (make-stack)
  (let ((s '()))
    (define (push x) (set! s (cons x s)))
    (define (pop)
      (if (null? s)
          (error "Empty stack: POP")
          (let ((top (car s)))
            (set! s (cdr s))
            top)))
    (define (initialize)
      (set! s '())
      'done)
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            (else (error "Unknown request: STACK" message))))
    dispatch))

(define (pop stack) (stack 'pop))
(define (push stack value) ((stack 'push) value))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register:" name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc (car insts)))
                (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq)
                 (set! the-instruction-sequence seq)))
              ((eq? message 'allocated-register)
               allocate-register)
              ((eq? message 'get-register)
               lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops)
                 (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              (else (error "Unknown request: MACHINE"
                           message))))
      dispatch)))
