#lang racket

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define leaf (make-leaf 'A 4))
(define (leaf? object) (eq? (car object) 'leaf))
(leaf? leaf)
(define (symbol-leaf x) (cadr x))
(symbol-leaf leaf)
(define (weight-leaf x) (caddr x))
(weight-leaf leaf)
;; caddr of the tree is list of symbols
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (make-code-tree left right)
  (list
   ;; car of tree is left
   left
   ;; cadr of tree is right
   right
   ;; caddr of tree is symbols
   (append (symbols left) (symbols right))
   ;; cadddr of tree is weight
   (+ (weight left) (weight right))))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit : CHOOSE-BRANCH" bit))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))


(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'D 1)
                    (make-code-tree
                     (make-leaf 'C 1)
                     (make-leaf 'Z 1))))))
;; (make-code-tree (make-leaf 'A 4)
;;                 (make-code-tree (make-leaf 'B 2) (make-leaf 'D 1)))
;; (leaf? sample-tree)
;; (cadddr sample-tree)
;; (weight sample-tree)
;; (define sample-tree
;;   (make-code-tree (make-leaf 'A 4)
;;                   (make-code-tree
;;                    (make-leaf 'B 2)
;;                    (make-code-tree)
;;                    (make-leaf 'D 1)
;;                    (make-leaf 'C 1))))
;; sample-tree

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        ;; choose branch with bits
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          ;; if leaf  cons symbol and recur with next bit , tree
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              ;; if not leaf dont cons
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))


(decode sample-message sample-tree)


(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))




(define (encode-symbol sym tree)

  ;; if its leaf
  (if (leaf? tree)
      ;; and symbol equals to symbol of tree
      (if (eq? sym (symbol-leaf tree))
          ;; return null
          '()
          (error "missing symbol: ENCODE-SYMBOL" sym))
      ;; else return error
      ;; choose left
      (let ((left (left-branch tree)))
        ;; if its inside of symbol left
        (if (memq sym (symbols left))
            ;; cons 0 with recur of encode
            (cons 0 (encode-symbol sym left))
            ;; else go right
            (cons 1 (encode-symbol sym (right-branch tree)))))))


(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (make-code-treee left right)
  (list
   ;; car of tree is left
   left
   ;; cadr of tree is right
   right
   ;; caddr of tree is symbols
   (append (symbols left) (symbols right))
   ;; cadddr of tree is weight
   (+ (weight left) (weight right))))

(define simplefln '(('A 2) ('B 1)))

(define (successive-merge set)
  (foldl make-code-tree (car set) (cdr set)))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(generate-huffman-tree simplefln)

