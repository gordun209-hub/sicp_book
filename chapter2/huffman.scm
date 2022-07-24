#lang sicp

;; (define (lookup given-key set-of-records)
;;   (cond
;;     ((null? set-of-records ) #f)
;;     ((equal? given-key (key (set-of-records)))
;;      (car set-of-records))
;;     (else (lookup given-key (cdr set-of-records)))))

;; (define (lookup-tree given-key set-of-records)
;;   (if (null? set-of-records) #f
;;       (let ((parent (entry set-of-records)))
;;         (cond ((eq? parent '()) #f)
;;               ((= given-key parent) parent)
;;               (else
;;                (lookup-tree given-key
;;                             (if (< given-key parent)
;;                                 (left-branch set-of-records)
;;                                 (right-branch set-of-records))))))))

; Example: Huffman Encoding Trees

(define (make-leaf symbol weight) (list 'leaf symbol weight))

(define (leaf? object) (eq? (car object) 'leaf))

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

(define (choose-branch bit branch)
  (cond
    ((= bit 0) (left-branch branch))
    ((= bit 1) (right-branch branch))
    (else (error "bad bit: CHOOSE-BRANCH" bit))))


(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits) '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))


(define (adjoint-set x set)
  (cond ((null? set) (list x))
        ((< (weight x ) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoint-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoint-set (make-leaf (car pair) ; symbol
                                (cadr pair)) ;freq
                     (make-leaf-set (cdr pairs))))))
