#lang racket

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))


(define b1 (make-branch 5 100))
(define b2 (make-branch 4 125))
(define b3 (make-branch 2 150))

(define m1 (make-mobile b1 b2))
(define m2 (make-mobile b1 b3))
(define ms (make-mobile b1 b1))

(define b4 (make-branch 7 m1))
(define b5 (make-branch 6 m2))
(define bs (make-branch ms ms))

(define m (make-mobile bs bs))
(define sm (make-mobile b1 b1))


(define (left-branch m)
  (cond ((eq? m '()) '())
        (else (car m))))

(define (right-branch m)
  (cond ((eq? m '()) '())
        ((eq? (cdr m) '()) '())
        (else (car (cdr m)))))


(define (branch-length branch)
  (if (not (pair? branch))
      branch
      (branch-length (car branch))))


(define (branch-structure branch)
  (let ((b (cdr branch)))
    (cond ((eq? b '()) '())
          (else (car b)))))


(define (total-weight m)
  (let ((sl (branch-structure (left-branch m)))
        (sr (branch-structure (right-branch m))))
    (cond ((and (pair? sl) (pair? sr)) (+ (total-weight sl) (total-weight sr)))
          ((and (pair? sl) (not (pair? sr))) (+ (total-weight sl) sr))
          ((and (not (pair? sl)) (pair? sr)) (+ sl (total-weight sr)))
          ((and (not (pair? sl)) (not (pair? sr))) (+ sl sr)))))


(define (is-balanced m)
  (let ((sl (branch-structure (left-branch m)))
        (sr (branch-structure (right-branch m)))
        (wl (branch-length (left-branch m)))
        (wr (branch-length (right-branch m))))
    (cond ((and (pair? sl) (pair? sr)) (and (is-balanced sl) (is-balanced sr)))
          ((and (pair? sl) (not (pair? sr))) (and (is-balanced sl) (eq? (* wr sr) (* (total-weight sl) wl))))
          ((and (not (pair? sl)) (pair? sr)) (and (is-balanced sr) (eq? (* wl sl) (* (total-weight sr) wr))))
          ((and (not (pair? sl)) (not (pair? sr))) (eq? (* wr sr) (* wl sl))))))

sm
(branch-structure (left-branch sm))
(branch-structure (right-branch sm))
(branch-length (left-branch sm))
(branch-length (right-branch sm))
(is-balanced sm)
(newline)
(newline)
(newline)
m
(newline)
(branch-length (right-branch m))
(branch-length (left-branch m))
(newline)
(left-branch m)
(branch-structure (left-branch m))
(right-branch m)
(branch-structure (right-branch m))
(newline)
(total-weight m)
(is-balanced m)