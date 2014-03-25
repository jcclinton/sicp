#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define v1 '(1 2 3 4))
(define v2 '(5 7 6 8))
(define v3 '(9 10 11 12))
(define m (list v1 v2 v3))
(define m2 (list '(1 2) '(3 4) '(5 6)))
(define m3 (list '(7 8 9) '(10 11 12)))


(define (get-first seq)
  (if (eq? seq '())
      '()
      (cons (car (car seq)) (get-first (cdr seq)))))

(define (drop-first seqs)
  (if (eq? seqs '())
      '()
      (cons (cdr (car seqs)) (drop-first (cdr seqs)))))

   
   
(define (accumulate-n op init seqs)
  (if (eq? '() (car seqs))
      null
      (cons (accumulate op init (get-first seqs))
            (accumulate-n op init (drop-first seqs)))))


(define (dot-product v w)
  (accumulate + 0 (map * v w)))


;(dot-product v1 v2)

;(define (matrix-*-vector m v)
;  (map m))

(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product w v)) m))

;(matrix-*-vector m v1)


(define (transpose mat)
  (accumulate-n (lambda (hd tl) (cons hd tl)) '() mat))

;(transpose m)

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v)
           (map (lambda (w) (dot-product v w)) cols))
           m)))

m2
m3
(transpose m3)
(matrix-*-matrix m2 m3)