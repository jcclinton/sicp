#lang racket

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))


(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (eq? '() bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((eq? '() set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (eq? '() pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define pairs '((A 4) (B 2) (C 1) (D 1)))
;(make-leaf-set pairs)

(define (successive-merge leaves)
  (cond ((eq? '() leaves) '())
        ((eq? '() (cdr leaves)) (car leaves))
        (else
         (make-code-tree
          (car leaves)
          (successive-merge (cdr leaves))
          ))))
;(generate-huffman-tree pairs)


(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))
;sample-tree


(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))


(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol x tree)
  (define (encode-symbol-bits x tree bits)
    (if (eq? '() tree)
        '()
        (if (leaf? tree)
            (if (eq? x (symbol-leaf tree))
                bits
                '())
            (append
             (encode-symbol-bits x (left-branch tree) (append bits (list 0)))
             (encode-symbol-bits x (right-branch tree) (append bits (list 1)))))))
  (encode-symbol-bits x tree '()))
          
          
   
;sample-tree
;(define sample-symbols (decode sample-message sample-tree))
;sample-message
;sample-symbols
;(encode sample-symbols sample-tree)
   
   

(define rock-pairs '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1)))
(define rock-tree (generate-huffman-tree rock-pairs))
;rock-tree

(encode '(WAH) rock-tree)
   
   
   