#lang racket
(require math/number-theory)

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))


(define s1 (enumerate-interval 1 6))
(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j)
                    (list i j))
                  (enumerate-interval 1 i)))
           (enumerate-interval 1 n)))

(define (sum-prime? pair)
  (prime? (+ (car pair) (cadr pair))))

;(map (lambda (p) (list (car p) (cadr p) (+ (car p) (cadr p)))) (filter sum-prime? (unique-pairs 6)))


(define (unique-triples n)
  (flatmap identity
           (flatmap (lambda (i)
                      (map (lambda (j)
                             (map (lambda (k)
                                    (list i j k))
                                  (enumerate-interval 1 j)))
                           (enumerate-interval 1 i)))
                    (enumerate-interval 1 n))))

(define (unique-set n interval)
  +
  ;creates a unique set of size n from 1..interval
  )


(define seq (unique-triples 5))

(define (sum-triple triple)
  (accumulate (lambda (hd tl) (+ hd tl)) 0 triple))

(define (sum-s? triple s)
  (eq? (sum-triple triple) s))


;seq
;(filter (lambda (tr) (sum-s? tr 6)) seq)