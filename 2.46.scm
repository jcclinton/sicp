#lang racket

(define (make-vect x y)
  (list x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cadr v))

(define (add-vect v1 v2)
  (let ((x1 (xcor-vect v1))
        (x2 (xcor-vect v2))
        (y1 (ycor-vect v1))
        (y2 (ycor-vect v2)))
    (make-vect (+ x1 x2) (+ y1 y2))))

(define (sub-vect v1 v2)
  (let ((x1 (xcor-vect v1))
        (x2 (xcor-vect v2))
        (y1 (ycor-vect v1))
        (y2 (ycor-vect v2)))
    (make-vect (- x1 x2) (- y1 y2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v)) (* s (ycor-vect v))))


(define (half-vect v-start v-end)
  (let ((x-start (xcor-vect v-start))
        (x-end (xcor-vect v-end))
        (y-start (ycor-vect v-start))
        (y-end (ycor-vect v-end)))
    (make-vect (/ (+ x-end x-start) 2) (/ (+ y-end y-start) 2))))

;;;

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (ocor frame)
  (car frame))

(define (e1-cor frame)
  (cadr frame))

(define (e2-cor frame)
  (cddr frame))


;;;

(define (for-each op lst)
  (define (iter acc rest)
    (if (eq? '() rest)
        acc
        (iter (op (car rest)) (cdr rest))))
  (iter '() lst))


(define (make-segment start-vect end-vect)
  (list start-vect end-vect))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cadr seg))


;;;

(define (draw-line v1 v2)
  (print (list v1 v2)))

(define (frame-coord-map f)
  (lambda (v)
    (scale-vect 2 v)))


(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

(define (frame-outline->painter)
  (lambda (frame)
    (let ((v1 (ocor frame))
          (v2 (add-vect (ocor frame) (e1-cor frame)))
          (v3 (add-vect (add-vect (ocor frame) (e2-cor frame)) (add-vect (ocor frame) (e1-cor frame))))
          (v4 (add-vect (ocor frame) (e2-cor frame))))
      (draw-line v1 v2) (newline)
      (draw-line v2 v3) (newline)
      (draw-line v3 v4) (newline)
      (draw-line v4 v1))))

(define (frame-x->painter)
  (lambda (frame)
    (let ((origin ((frame-coord-map frame) (ocor frame)))
          (e1 ((frame-coord-map frame) (e1-cor frame)))
          (e2 ((frame-coord-map frame) (e2-cor frame))))
      (let ((v1 origin)
            (v2 (add-vect origin e1))
            (v3 (add-vect (add-vect origin e2) (add-vect origin e1)))
            (v4 (add-vect origin e2)))
        (let ((s13 (make-segment v1 v3))
              (s24 (make-segment v2 v4)))
          (for-each
           (lambda (segment)
             (draw-line
              (start-segment segment)
              (end-segment segment)))
           (list s13 s24)))))))
       
;;;


       
(define v1 (make-vect 4 3))
(define v2 (make-vect 2 5))
(define v3 (make-vect 5 7))
(define seg12 (make-segment v1 v2))
(define seg13 (make-segment v1 v3))
(define seg23 (make-segment v2 v3))
(define seg-list (list seg12 seg13 seg23))
(define frame (make-frame v3 v2 v1))
(define e1 (ocor frame))
(define e2 (add-vect (ocor frame) (e1-cor frame)))
(define e3 (add-vect (add-vect (ocor frame) (e2-cor frame)) (add-vect (ocor frame) (e1-cor frame))))
(define e4 (add-vect (ocor frame) (e2-cor frame)))
(define s12 (make-segment e1 e2))
(define s23 (make-segment e2 e3))
(define s34 (make-segment e3 e4))
(define s41 (make-segment e4 e1))

(define s24 (make-segment e2 e4))
(define s13 (make-segment e1 e3))


;(define e1-half (scale-vect 0.5 e1))
;(define e2-half (scale-vect 0.5 e2))
;(define e3-half (scale-vect 0.5 e3))
;(define e4-half (scale-vect 0.5 e4))

(define e12-half (half-vect e1 e2))
(define e23-half (half-vect e2 e3))
(define e34-half (half-vect e3 e4))
(define e41-half (half-vect e4 e1))

(define s12-half (make-segment e12-half e23-half))
(define s23-half (make-segment e23-half e34-half))
(define s34-half (make-segment e34-half e41-half))
(define s41-half (make-segment e41-half e12-half))

(define frame-list (list s12 s23 s34 s41))
(define frame-corners (list s13 s24))
(define frame-midpoints (list s12-half s23-half s34-half s41-half))

frame
;(define painter (frame-outline->painter))
;(define painter (segments->painter frame-list))
;(define painter (segments->painter frame-midpoints))
(define painter (frame-x->painter))
(painter frame)

