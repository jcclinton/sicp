#lang racket
#lang racket
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))

(define (fast-fib n)
 (iter n 0 0 0 0)
)

(define (iter n acc i l l2)
  (cond ((= i 0)
         (iter n 0 0 0 0))
        ((= i 1)
         (iter n 1 1 0 0))
        ((> i n)
         acc)
        ((= i n)
         acc)
        (else
         (iter n (+ l l2) (+ 1 i) acc l))))