#lang sicp

;; lookup implemented for unordered list
;; (define (lookup given-key set-of-records)
;;   (cond ((null? set-of-records) false)
;;         ((equal? given-key (key (car set-of-records)))
;;          (car set-of-records))
;;         (else (lookup given-key (cdr set-of-records)))))


;; Leaves of the tree are represented by a list consisting of the
;; symbol leaf, the symbol at the leaf and the weight
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

;Decoding tree

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    ;; bitlerin sonuna gelince bos list wer
    (if (null? bits)
        '()
        ;; next branchi sec bitlere gore
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          ;; eger ki next branch leaf ise onun sembolunu consla
          ;; ve decode-1 i bidaha cagir sonraki bit ile
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              ;; eger leaf deilse decode-1 i bidaha cagir sunlarla:
              ;; rest of bits ve sonraki branch
              (decode-1 (cdr bits) next-branch)))))
  ;; iterate over bunla
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit --CHOOSE-BRANCH" bit))))


(define (adjoint-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoint-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoint-set (make-leaf (car pair) ;symbol
                                (cadr pair)) ;frequency
                     (make-leaf-set (cdr pairs))))))


(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
sample-tree
(decode  sample-message sample-tree)

