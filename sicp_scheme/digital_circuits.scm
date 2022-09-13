#lang sicp

;; Digital circuits

;; a digital signal may at any moment have only one of two possible values
;; 0 and 1 , there are also various types of digital fucntion boxes, which
;; connect wires carrying input signals to other output wires.
;; such boxes produce output signals computed from their input signals


(define a (make-wire))
(define b (make-wire))
(define c (make-wire))
(define d (make-wire))
(define e (make-wire))
(define s (make-wire))

(or-gate a b d)

(and-gate a b c)

(inverter c e)

(and-gate d e s)

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    ;; ilk ikisi giren sonuncu cikan?
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))


(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire)) (c1 (make-wire)) (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))


(get-signal <wire>) ;; returns current value of signal

(set-signal! <wire> <new value>) ;; changes the value of signal

(add-action! <wire> <procedure of no arguments>)
;; asserts that the designated procedure should be run whenever the signal
;; on the wire changes value. Such procedures are the vehicles by which
;; changes in the signal value on the wire are communicated to other wires

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! input invert-input) 'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal"))))


(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay
       and-gate-delay
       (lambda () (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay
       or-gate-delay
       (labmda () (set-signal! output new-value)))))
  (add-action a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)



(define (or-gate-alternate a1 a2 output)
  (let ((c (make-wire))
        (d (make-wire)) (e (make-wire))
        (f (make-wire) (g (make-wire))))
    (and-gate a1 a1 d)
    (and-gate a2 a2 e)
    (inverter d f)
    (inverter e g)
    (and-gate f g c)
    (inverter c output)
    'ok))

(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures
            (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation: WIRE" m))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin ((car procedures))
             (call-each (cdr procedures)))))

(make-agenda) ; returns new empty agenda 

(empty-agenda? <agenda>) ;; true if empty

(first-agenda-item <agenda>) ;; first 

(remove-first-agenda-item! <agenda>) ;; removes firsth 

;; modifies the agenda by adding the given action procedure to be run at the 
;; sepesificied time
(add-to-agenda! <time> <action> <agenda>) 

(current-time <agenda>) ; returns current simulation time


(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action 
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
    'done)
  (let ((first-item (first-agenda-item the-agenda)))
    (first-item)
    (remove-first-agenda-item! the-agenda)
    (propagate)))


;  ; simdilik oteki chaptera geccm

