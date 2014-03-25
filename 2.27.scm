#lang racket

(define x (cons (list 1 2) (list 3)))
(define y (cons (list 5 6) (list 7 8 9)))
(define z (cons x y))

(define (fringe lst)
  (cond ((null? lst) '())
        ((not (list? lst)) (list lst))
        (else (append (fringe (car lst)) (fringe (cdr lst))))))

z
(fringe z)