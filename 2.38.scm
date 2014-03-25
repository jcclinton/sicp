#lang racket
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))


(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))


(define v1 '(1 2 3))
(define v2 '(5 7 6 8))

;(fold-right / 1 v1)
;(fold-left / 1 v1)

;(fold-right list null (list 1 2 3))
;(fold-left list null (list 1 2 3))

(define (reverse seq)
  (fold-right (lambda (hd tl) (append tl (list hd))) null seq))

(define (rev2 seq)
  (fold-left (lambda (hd tl) (cons tl hd)) null seq))

(rev2 v2)