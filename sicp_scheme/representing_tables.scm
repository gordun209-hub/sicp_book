#lang sicp


;; that is constructor i think idk im 8
;; (define (lookup key table)
;;   (let ((record (assoc key (cdr table))))
;;     (if record
;;         (cdr record)
;;         false)))
;;
;; ;; that one is for searching
;; (define (assoc key records)
;;   (cond ((null? records) false)
;;         ((equal? key (caar records)) (car records))
;;         (else (assoc key (cdr records)))))
;;
;; (define (insert! key value table)
;;   ;; try to find record
;;   (let ((record (assoc key (cdr table))))
;;     (if record
;;         ;; if there is a record, replace its value to given value
;;         (set-cdr! record value)
;;         ;; if not, cons the new table at first
;;         (set-cdr! table
;;
;;                   (cons (cons key value)
;;                         (cdr table))))))
;;
;; ;; cdr table is records
;; ;; car table is string table
;; (define (make-table)
;;   (list '*table*))
;;
;; (define mytabl (make-table))
;;
;; (insert! 'ab 31 mytabl)
;; (insert! 'aa 32 mytabl)
;; (insert! 'ac 33 mytabl)
;; (cdr mytabl)
;; mytabl


;; Two dimensional tables

;; (define (lookup key-1 key-2 table)
;;   ;; first find subtable
;;   (let ((subtable
;;          (assoc key-1 (cdr table))))
;;     (if subtable
;;         ;; if subtable exists get record
;;         (let ((record
;;                (assoc key-2 (cdr subtable))))
;;           ;; if record exists get value of record
;;           (if record
;;               (cdr record)
;;               ;; else false
;;               false))
;;         ;; else false
;;         false)))
;;
;; (define (insert! key-1 key-2 value table)
;;   (let ((subtable (assoc key-1 (cdr table))))
;;     (if subtable
;;         (let ((record (assoc key-2 (cdr subtable))))
;;           (if record
;;               (set-cdr! record value)
;;               (set-cdr! subtable
;;                         (cons (cons key-2 value))
;;                         (cdr subtable))))
;;         (set-cdr! table
;;                   (cons (list key-1
;;                               (cons key-2 value))
;;                         (cdr table)))))
;;   'ok)
;;
;; ;; tables that procedures for internal methods?
;;
;; (define (make-table)
;;   (let ((local-table (list '*table*)))
;;     (define (lookup key-1 key-2)
;;       (let ((subtable
;;              (assoc key-1 (cdr local-table))))
;;         (if subtable
;;             (let ((record
;;                    (assoc key-2 (cdr subtable))))
;;               (if record (cdr record) false))
;;             false)))
;;     (define (insert! key-1 key-2 value)
;;       (let ((subtable
;;              (assoc key-1 (cdr local-table))))
;;         (if subtable
;;             (let ((record
;;                    (assoc key-2 (cdr subtable))))
;;               (if record
;;                   (set-cdr! record value)
;;                   (set-cdr! subtable
;;                             (cons (cons key-2 value))
;;                             (cdr subtable))))
;;             (set-cdr! local-table
;;                       (cons (list key-1 (cons key-2 value))
;;                             (cdr local-table)))))
;;       'ok)
;;     (define (dispatch m)
;;       (cond ((eq? m 'lookup-proc) lookup)
;;             ((eq? m 'insert-proc!) insert!)
;;             (else (error "Unknown operation: TABLE" m))))
;;     dispatch))
;;
;; ;; create table
;; (define operation-table (make-table))
;; ;; getter
;; (define get (operation-table 'lookup-proc))
;; ;; setter???
;; (define put (operation-table 'insert-proc!))
;;
;;
;; (define memo-fib
;;   (memoize
;;    (lambda (n)
;;      (cond ((= n 0) 0)
;;            ((= n 1) 1)
;;            (else (+ (memo-fib (- n 1))
;;                     (memo-fib (- n 2))))))))
;;
;; (define (memoize f)
;;   (let ((table (make-table)))
;;     (lambda (x)
;;       (let ((previously-computed-result
;;              (lookup x table)))
;;         (or previously-computed-result
;;             (let ((result (f x)))
;;               (insert! x result table)
;;               result))))))




(define (make-table)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) false)
            ((equal? key (caar records)) (car records))
            (else (assoc key (cdr records)))))

    (define (lookup key-list)
      (define (iter keys table)
        (cond ((null? keys) false) ;;为空时
              ((null? (cdr keys))  ;;只有一个key时
               (let ((record (assoc (car keys) (cdr table))))
                 (if record
                     (cdr record)
                     false)))
              (else                    ;;有多个key时,先取出一个，用于找到subtable，然后剩下的再循环再
               (let ((subtable (assoc (car keys) (cdr table))))
                 (if subtable
                     (iter (cdr keys) subtable)
                     false)))))
      (iter key-list local-table))

    (define (insert! value key-list)
      (define (iter keys table)
        (cond ((null? table)    ;;这是当没有找到key对应的subtable时，需要创建新的subtable
               (if (null? (cdr keys))
                   (cons (car keys) value)
                   (list (car keys) (iter (cdr keys) '()))))
              ((null? (cdr keys)) ;;只有一个key
               (let ((record (assoc (car keys) (cdr table))))
                 (if record
                     (set-cdr! record value)
                     (set-cdr! table
                               (cons (cons (car keys) value)
                                     (cdr table))))))
              (else            ;;有多个key
               (let ((subtable (assoc (car keys) (cdr table))))
                 (if subtable
                     (iter (cdr keys) subtable)
                     (set-cdr! table
                               (cons (list (car keys)
                                           (iter (cdr keys) '()))   ;;这里是关键，没找到subtable时，创建新的，然后循环(cdr keys)
                                     (cdr table))))))))
      (iter key-list local-table)
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define (lookup table . key-list) ((table 'lookup-proc) key-list))
(define (insert! table value . key-list) ((table 'insert-proc!) value key-list))
