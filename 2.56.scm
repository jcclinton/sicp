#lang racket

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))
  
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

;(define (make-sum a1 a2) (list '+ a1 a2))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s)
  (if (eq? '() (cdddr s))
      (caddr s)
      (let ((first (caddr s))
            (rest (cdddr s))
            (tl (cdr (cdddr s))))
        (if (eq? '() tl)
            (make-sum first (car rest))
            (make-sum first rest)))))

;(augend '(+ 1 2))
;(augend '(+ 1 2 3))
;(augend '(+ 1 (+ 3 4) 2))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

;(define (multiplicand p) (caddr p))

(define (multiplicand p)
  (if (eq? '() (cdddr p))
      (caddr p)
      (let ((first (caddr p))
            (rest (cdddr p))
            (tl (cdr (cdddr p))))
        (if (eq? '() tl)
            (make-product first (car rest))
            (make-product first rest)))))
(multiplicand '(* 2 2))
(multiplicand '(* 2 2 3))
(multiplicand '(* 2 (+ 3 4) 2))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base x)
  (cadr x))

(define (exponent x)
  (caddr x))

(define (make-exponentiation base exp)
  (list '** base exp))


(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (make-product (exponent exp) (make-exponentiation (base exp) (- (exponent exp) 1))) (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define exp '(* x y (+ x 3)))
(define exp2 '(+ x x 3))
(deriv exp2 'x)




