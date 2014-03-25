#lang racket

(define (last-pair lst)
  (if (null? (cdr lst))
      (car lst)
      (last-pair (cdr lst))))


(define (reverse lst)
  (if (null? lst)
      '()
      (append (reverse (cdr lst)) (cons (car lst) '()))))

#(define (same-parity x . lst)
  (define (mymap f lst2)
  (if (even x))))

(define (myfilter test? lst)
  (define (recurse) (myfilter test? (cdr lst)))
  (cond ((null? lst) '())
        (else 
         (if (test? (car lst))
             (cons (car lst) (recurse))
             (recurse)))))

(define (map proc lst)
  (if (null? lst)
      '()
      (cons (proc (car lst)) (map proc (cdr lst)))))