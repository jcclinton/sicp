#lang racket
(define (count-leaves lst)
  (cond ((eq? '() lst) 0)
        ((not (pair? lst)) 1)
        (else (+ (count-leaves (car lst)) (count-leaves (cdr lst))))))


(define (scale-tree tree factor)
  (cond ((eq? '() tree) '())
        ((integer? tree) (* tree factor))
        (else (cons (scale-tree (car tree) factor) (scale-tree (cdr tree) factor)))))


(define (map proc lst)
  (if (null? lst)
      '()
      (cons (proc (car lst)) (map proc (cdr lst)))))

(define (map-tree proc tree)
  (cond ((null? tree) '())
        ((integer? tree) (proc tree))
        (else (cons (map-tree proc (car tree)) (map-tree proc (cdr tree))))))

(define (square-list items)
  (map sqr items))

(define (square-tree tree )
  (map-tree sqr tree))

  

(define (subsets s)
  (if (null? s)
      (cons '() '())
      (let ((rest (subsets (cdr s)))
            (fn (lambda (el) (cons (car s) el))))
        (append rest (map fn rest)))))

   
   
(define x '(1 2 3))
(define y '(5 8 6 7))
(define xy (cons y x))

x
(subsets x)