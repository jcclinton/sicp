#lang racket

(define (for-each fn lst)
  (if (null? lst) null
      (cons (fn (car lst)) (for-each fn (cdr lst)))))



(define x (cons (list 1 2 3) (list 4 5 6)))
(define y (cons (list 5 6) (list 7 8)))

(define (deep-reverse2 x)
  (cond ((null? x) '())
        ((not (pair? x)) x)
        (else 
         (let ((tl (deep-reverse (cdr x)))
               (hd (deep-reverse (car x))))
           (cond ((eq? tl '()) hd)
                 ((pair? hd) (cons hd tl))
                 (else (list tl hd)))))))


(define (deep-reverse x)
  (cond ((null? x) '())
        ((not (pair? x)) x)
        (else 
         (let ((tl (deep-reverse (cdr x)))
               (hd (deep-reverse (car x))))
           (cond ((eq? tl '()) hd)
                 ((eq? hd '()) tl)
                 ((and (not (pair? hd)) (not (pair? tl))) (list tl hd))
                 ((and (not (pair? hd)) (pair? tl)) (append tl (list hd)))
                 ((and (pair? hd) (pair? tl)) (append tl (list hd)))
                 ((and (pair? hd) (not (pair? tl))) (append (list tl) hd))
                 (else (list tl hd)))))))

x
(deep-reverse x)
(newline)
(cons y(append x y))
(deep-reverse (cons y (append x y)))