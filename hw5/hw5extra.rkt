#lang racket
(provide (all-defined-out))

(struct btree-leaf () #:transparent)
(struct btree-node (value left right) #:transparent)

(define tree-height
  (lambda (tree)
    (if (btree-leaf? tree)
        0
        (+ 1 (max (tree-height (btree-node-left tree))
                  (tree-height (btree-node-right tree)))))))

(define sum-tree
  (lambda (tree)
    (if (btree-node? tree)
        (+ (btree-node-value tree)
           (sum-tree (btree-node-left tree))
           (sum-tree (btree-node-right tree)))
        0)))

(define prune-at-v
  (lambda (tree v)
    (if (and (btree-node? tree)
             (not (equal? (btree-node-value tree) v)))
        (let ([left (prune-at-v (btree-node-left tree) v)]
              [right (prune-at-v (btree-node-right tree) v)])
          (btree-node (btree-node-value tree) left right))
        (btree-leaf))))

;; racket will/can not check the type of elments inside binary tree,
;; it only check whether the number of elments of binary tree object
;; conforms to the struct definition
(define well-formed-tree?
  (lambda (tree)
    (if (btree-node? tree)
        (and (and (not (btree-node? (btree-node-value tree)))
                  (not (btree-leaf? (btree-node-value tree))))
             (well-formed-tree?
              (btree-node-left tree))
             (well-formed-tree?
              (btree-node-right tree)))
        (btree-leaf? tree))))

(define fold-tree
  (lambda (f acc tree)
    (if (btree-node? tree)
        (f acc
           (+ (btree-node-value tree)
              (fold-tree f 0 (btree-node-left tree))
              (fold-tree f 0 (btree-node-right tree))))
        0)))

(define fold-tree-curried
  (lambda (f)
    (lambda (acc)
      (lambda (tree)
        (if (btree-node? tree)
            (f acc
               (+ (btree-node-value tree)
                  (fold-tree f 0 (btree-node-left tree))
                  (fold-tree f 0 (btree-node-right tree))))
            0)))))

(define fold-either
  (lambda (f acc listortree)
    (cond [(list? listortree)
           (letrec ([aux (lambda (acc lst)
                           (if (null? lst)
                               acc
                               (aux (f acc (car lst))
                                    (cdr lst))))])
             (aux acc listortree))]
          [(well-formed-tree? listortree)
           (fold-tree f acc listortree)]
          [else (error "expected a binary tree or list")])))

(define plus-one
  (lambda (x y)
    (+ x y 1)))

(define test-node
  (btree-node
   10
   (btree-node 20 (btree-leaf) (btree-leaf))
   (btree-node 9 (btree-leaf) (btree-leaf))))

(define flatten
  (lambda (lst)
    (if (null? lst)
        '()
        (if (list? (car lst))
            (letrec ([flatten-sublist
                      (lambda (lst next)
                        (if (null? lst)
                            next
                            (if (list? (car lst))
                                (flatten-sublist
                                 (car lst)
                                 (flatten-sublist (cdr lst) next))
                                (cons (car lst)
                                      (flatten-sublist (cdr lst) next)))))])
              (flatten-sublist (car lst) (flatten (cdr lst))))
            (cons (car lst) (flatten (cdr lst)))))))
