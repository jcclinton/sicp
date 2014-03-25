#lang racket

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cond ((eq? '() set) (cons x '()))
        ((eq? x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(define (union-set set1 set2)
  (cond ((eq? '() set1) set2)
        ((eq? '() set2) set1)
        (else
         (let ((x1 (car set1))
               (x2 (car set2))
               (tl1 (cdr set1))
               (tl2 (cdr set2)))
           (cond ((eq? x1 x2) (cons x1 (union-set tl1 tl2)))
                 ((> x1 x2) (cons x2 (union-set set1 tl2)))
                 ((< x1 x2) (cons x1 (union-set tl1 set2))))))))


(define set1 '(1 2 3 4))
(define set2 '(5 6 7 8))
(define set3 '(3 4 5 6))

(union-set set2 set3)