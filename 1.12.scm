#lang racket

(define (tri level)
  (cond ((= level 1) '(1))
        ((= level 2) '(1 1))
        (else 
         (add1 (cons 1 (build (tri (- level 1)))))
        )
  )
)

(define (add1 lst)
  (if (pair? lst)
      (cons (car lst) (add1 (cdr lst)))
      '(1))
)

(define (build lst)
  (loop lst)
)

(define (loop lst)
  (if (eqv? '() (cdr (cdr lst))) (list (+ (car lst) (car (cdr lst))))
      (cons 
       (+ (car lst) (car (cdr lst)))
       (loop (cdr lst)))
      )
)