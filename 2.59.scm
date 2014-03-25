#lang racket

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set-unique x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (adjoin-set x set)
  (cons x set))

(define (union-set set1 set2)
  (if (eq? set1 '())
      set2
      (let ((s2 (adjoin-set (car set1) set2)))
        (union-set (cdr set1) s2))))

(define set1 '(1 2 3 4))
(define set2 '(5 6 7 8))
(define set3 '(3 4 5 6))

(union-set set1 set2)
(union-set set2 set3)
(union-set set3 set1)