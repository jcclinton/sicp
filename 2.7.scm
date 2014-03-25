#lang racket

(define (make-interval a b) (cons a b))

(define (upper-bound interval)
  (let ((p1 (car interval))
        (p2 (cdr interval)))
    (if (> p1 p2)
        p1
        p2)))


(define (lower-bound interval)
  (let ((p1 (car interval))
        (p2 (cdr interval)))
    (if (< p1 p2)
        p1
        p2)))