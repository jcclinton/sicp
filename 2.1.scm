#lang racket

(define (make-rat x y) (cons x y))

(define (make-rat2 x y)
  (let ((g (abs (gcd x y)))
        (absx (abs x))
        (absy (abs y)))
    (cons (/ (if (< (/ x y) 0) (* -1 absx) absx) g)
          (/ absy g))))

  

(define (numer rat) (car rat))

(define (denom rat) (cdr rat))

(define (one-half) (make-rat 1 2))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))