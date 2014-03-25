#lang racket

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) 
    (lambda (x) 
      (f ((n f) x)))))


(define (get-1)
  (lambda (f) 
    (lambda (x) 
      (f (((lambda (f) (lambda (y) y)) f) x)))))