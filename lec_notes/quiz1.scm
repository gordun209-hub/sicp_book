#lang sicp

;; (define tower
;;   (lambda (x n)
;;     (cond
;;       ((= n 0) 1)
;;       ((> n 0) x)
;;       (else
;;         (expt x (tower  x (- n 1)))))))

(define tower
  (lambda (x n)
    (cond
      ((= n 0) 1)
      (else
       (expt x (tower x (- n 1)))))))

;;
;; (tower 2 3)
; (expt 2 (* 2 (tower 2 2)))
;; (expt 2 (expt  2 ( expt 2 1)))


;; Part 3: (30 points)
;; Ben Bitdiddle (a common character in 6.001 with no connection to your instructor) is implementing
;; a rating tracking system for people playing the card game Spades. This tracking system maintains
;; a count of wins and losses for each player. His first job is to implement a player abstraction that
;; wraps up the player’s name, number of wins, and number of losses. He’s managed to implement
;; the selectors, but the constructor continues to elude him. Help Ben Bitdiddle out by completing
;; the constructor in such a way that the contract is preserved (ie the selectors return the appropriate
;; values).

(define (make-player name wins loses)
  (cons name (cons wins loses)))


(define first
  (lambda (x)
    (car x)))
(define second
  (lambda (x)
    (car (cdr x))))

(define third
  (lambda (x)
    (cdr (cdr x))))

(cdr (cdr (cons "Ben" (cons 5 3))))
(define (player-name player)
  (first player))
;;
(define (player-wins player)
  (second player))
;;
(define (player-losses player)
  (third player))
;;
;; ; example usage
;;
(define p (make-player "Ben" 5 3))
;;
;;
(player-name p)
;; ; "Ben"
;;
(player-wins p)
;; ;Value 5
;;
(player-losses p)
;; ;Value 3
;;

(define (player-win-ratio player)
  (/ (second player) (+ (second player) (third player))))

(define (check-record list-of-players)
  (if (null? list-of-players)
      0
      (+ (- (second (car list-of-players)) (third (car list-of-players)))
         (check-record (cdr list-of-players)))))

(define list-players (list (make-player "Ben" 5 4) (make-player "la" 2 3)))
list-players

(check-record list-players)

((lambda (a b) (a b))
 (lambda (c) (* 2 c))
 10)

(lambda (m n)
  (if m (* 2 n) (/ 2 n))) ; procedure

;; (let ((x <)
;;       (y 10)
;;       (z 20))
;;   (z y x))  ; err



(define (double x) (* 2 x))
(define (check y)
  (if (< (double y) 10)
      "yip"
      (if (> (double y) 6)
          "yay"
          "yuck")))
check

;; (define three 
;;   (lambda (a b c) (* a b c)))
;; (three (three 1 ) (three 2) (three 3))
