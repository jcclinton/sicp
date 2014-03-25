#lang racket

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car n)
  (find-a n 0))

(define (cdr n)
  (find-b (/ n (expt 2 (find-a n 0))) 0))
  
  
(define (find-a n i)
  (cond ((> 0 (remainder n (expt 2 i))) (find-a n (+ i 1)))
        (else (let ((n-over-2a (/ n (expt 2 i))))
                (if (= -1 (find-b n-over-2a 0))
                    (find-a n (+ i 1))
                    i)))))
             
             
             
(define (find-b n i)
  (cond ((= (expt 3 i) n) i)
         ((> (expt 3 i) n) -1)
         (else (find-b n (+ i 1)))))