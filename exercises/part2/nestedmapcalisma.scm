#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

;; (accumulate
;;  append nil (map (lambda (i)
;;                    (map (lambda (j) (list i j))
;;                         (enumerate-interval 1 (- i 1))))
;;                  '(1 2 3)))

;; (accumulate
;;  append nil (map (lambda (i)
;;                    ;; j al ve i ile birlestir
;;                    (map (lambda (j) (list i j))
;;                         ;; i den 1 eksigi kadar olan rangede olstr
;;                         (enumerate-interval 1 (- i 1))))
;;                  '(1 2 3)))

;; (accumulate append nil (map (lambda (i) (list i))
;;                             (enumerate-interval 1 10)))

(accumulate append nil (map (lambda (i)
                              (map (lambda (j) (list i j))
                                   ;; i den kucuk list olustur list bitene kadar j ile ayni liste koy
                                   (enumerate-interval 1 (- i 1))))
                            '(1 2 3 4)))
