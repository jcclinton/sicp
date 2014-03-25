#lang racket
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))
                                           
(define (even? n)
  (= (remainder n 2) 0))

(define (square n)
  (* n n))


(define (fast-expt2 b n)
  (fast-expt2-iter b n 1))

(define (fast-expt2-iter b n acc)
  (cond ((= n 0) 1)
        ((even? n) (fast-expt2-iter b (/ n 2) (* acc (square (/ n 2)))))
        (else )
  )
)