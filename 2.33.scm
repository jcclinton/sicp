#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


(define (map p seq)
  (accumulate (lambda (hd tl) 
                (cons (p hd) tl))
              '()
              seq))


(define s1 '(1 2 3 4))
(define s2 '(5 7 6 8))
(define s12 (cons s1 s2))

;(map (lambda (x) (* x 2)) s1)

(define (horner-eval x coefficient-seq)
  (accumulate (lambda (hd tl) (+ (* x tl) hd))
              0
              coefficient-seq))

;(horner-eval 2 '(1 3 0 5 0 1))

(define (count-leaves t)
  (accumulate (lambda (hd tl)
                (cond ((pair? hd) (+ (count-leaves hd) tl))
                      (else (+ 1 tl))))
              0
              t))

;(count-leaves (cons s1 s2))

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

(define ss1 (list s1 s2 s2))
ss1
(accumulate-n * 1 ss1)