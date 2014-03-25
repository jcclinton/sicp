#lang racket

(define (change n)
  (change-node n 50))

(define (change-node n max-denom)
  (define (check a)
    (if (>= max-denom a) (change-check n a) 0))
  (loop '(1 5 10 25 50) check 0)
  )

(define (change-check n denom)
  (cond ((< (- n denom) 0) 0)
        ((= (- n denom) 0) 1)
        (else (change-node (- n denom) denom))))

(define (loop lst fn acc)
  (if (pair? lst)
      (loop (cdr lst) fn (+ acc(fn (car lst))))
      acc
      )
 )
