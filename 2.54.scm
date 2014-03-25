#lang racket

(define (equal? l1 l2)
  (cond ((and (eq? l1 '()) (eq? l2 '())) #t)
        ((or (eq? l1 '()) (eq? l2 '())) #f)
        ((and (pair? l1) (pair? l2))
         (and (equal? (car l1) (car l2)) (equal? (cdr l1) (cdr l2))))
        ((and (not (pair? l1)) (not (pair? l2)))
         (and (eq? l1 l2)))
        (else #f)))

(define l1 '(a b c))
(define l2 '(x y z))
(define l3 '(a y z))
(define l4 (list l1 '(a y z)))
(equal? l1 l1)
(equal? l1 l2)
(equal? l2 l3)
(equal? l1 l3)

(equal? l4 l4)
(equal? l4 l1)