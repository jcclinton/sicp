#lang racket

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

(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (new-row)
            (map (lambda (rest-of-queens)
                   (adjoin-position new-row k rest-of-queens))
                 (queen-cols (- k 1))))
          (enumerate-interval 1 board-size)))))
  (queen-cols board-size))

(define (in-following-rows n positions)
  (if (eq? null positions)
      #f
      (or (eq? (caar positions) n) (in-following-rows n (cdr positions)))))

(define (in-following-columns n positions)
  (if (eq? null positions)
      #f
      (or (eq? (cadr (car positions)) n) (in-following-columns n (cdr positions)))))

(define (in-following-diags k pair positions)
  (if (eq? positions null)
      #f
      (or
       (check-up-left-diag k pair positions)
       (check-down-left-diag k pair positions)
       (check-up-right-diag k pair positions)
       (check-down-right-diag k pair positions)
       (in-following-diags k (car positions) (cdr positions))
       )))


(define (check-up-right-diag k pair positions)
  (let ((row (car pair))
        (col (cadr pair)))
    (if (or (eq? null positions) (or (> row k) (> col k)))
         #f
         (let ((left (filter (lambda (pos)
                               (and (eq? row (car pos)) (eq? col (cadr pos))))
                             positions)))
           (if (eq? null left)
               (check-up-right-diag k (list (+ 1 row) (+ 1 col)) positions)
               #t)))))
       
(define (check-down-left-diag k pair positions)
  (let ((row (car pair))
        (col (cadr pair)))
    (if (or (eq? null positions) (or (eq? row 0) (eq? col 0)))
         #f
         (let ((left (filter (lambda (pos)
                               (and (eq? row (car pos)) (eq? col (cadr pos))))
                             positions)))
           (if (eq? null left)
               (check-down-left-diag k (list (- row 1) (- col 1)) positions)
               #t)))))



(define (check-down-right-diag k pair positions)
  (let ((row (car pair))
        (col (cadr pair)))
    (if (or (eq? null positions) (or (eq? row 0) (> col k)))
         #f
         (let ((left (filter (lambda (pos)
                               (and (eq? row (car pos)) (eq? col (cadr pos))))
                             positions)))
           (if (eq? null left)
               (check-down-right-diag k (list (- row 1) (+ 1 col)) positions)
               #t)))))
       
(define (check-up-left-diag k pair positions)
  (let ((row (car pair))
        (col (cadr pair)))
    (if (or (eq? null positions) (or (> row k) (eq? col 0)))
         #f
         (let ((left (filter (lambda (pos)
                               (and (eq? row (car pos)) (eq? col (cadr pos))))
                             positions)))
           (if (eq? null left)
               (check-up-left-diag k (list (+ row 1) (- col 1)) positions)
               #t)))))
;(check-up-left-diag 4 '(3 3) '((4 2) (2 1)))
                     

(define (safe? k positions)
  (if (eq? null positions)
      #t
      (and
       (not (in-following-rows (caar positions) (cdr positions)))
       (not (in-following-columns (cadr (car positions)) (cdr positions)))
       (not (in-following-diags k (car positions) (cdr positions)))
       )))
           
           
           

(define (adjoin-position new-row k rest-of-queens)
  (cons (list new-row k) rest-of-queens))

(define empty-board '())


(queens 6)
;(in-following-columns 4 '((1 1) (1 2) (1 3)))