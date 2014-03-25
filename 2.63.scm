#lang racket

(define (entry tree) (car (car tree)))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list (list entry '(record)) left right))

(define (tree->list-1 tree)
  (if (eq? '() tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (eq? '() tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define (lookup key tree)
  (cond ((eq? '() tree) #f)
        ((eq? key (entry tree)) (car tree))
        ((> key (entry tree)) (lookup key (right-branch tree)))
        ((< key (entry tree)) (lookup key (left-branch tree)))))

(define tree1
  (make-tree 7 
             (make-tree 3
                        (make-tree 1 '() '())
                        (make-tree 5 '() '()))
             (make-tree 9
                        '()
                        (make-tree 11 '() '()))))
(define tree2
  (make-tree 3 
             (make-tree 1 '() '())
             (make-tree 7
                        (make-tree 5 '() '())
                        (make-tree 9
                                   '()
                                   (make-tree 11 '() '())))))

(define tree3
  (make-tree 5 
             (make-tree 3 
                        (make-tree 1 '() '())
                        '())
             (make-tree 9
                        (make-tree 7 '() '())
                        (make-tree 11
                                   '()
                                   '()))))


tree3
(lookup 5 tree3)
(lookup 2 tree3)