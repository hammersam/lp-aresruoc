#lang racket

(require "hw5extra.rkt")
(require rackunit)

(define tests
  (let ((sample-tree
         (btree-node 4
                     (btree-node 3
                                 (btree-node 1
                                             (btree-node 0
                                                         (btree-leaf)
                                                         (btree-leaf))
                                             (btree-leaf))
                                 (btree-node 2
                                             (btree-leaf)
                                             (btree-leaf)))
                     (btree-leaf))))
    (test-suite
     "tests for Extra Assignment 5"

     (check-equal? (tree-height (btree-leaf)) 0 "tree-height test1")
     (check-equal? (tree-height sample-tree) 4 "tree-height test2")
     (check-equal? (sum-tree sample-tree) 10 "sum-tree test1")
     (check-equal? (prune-at-v sample-tree 4) (btree-leaf) "prune-at-v test1")
     (check-equal? (prune-at-v sample-tree 2)
                   (btree-node 4
                               (btree-node 3
                                           (btree-node 1
                                                       (btree-node 0
                                                                   (btree-leaf)
                                                                   (btree-leaf))
                                                       (btree-leaf))
                                           (btree-leaf))
                               (btree-leaf))
                   "prune-at-v test2")
     (check-equal? (well-formed-tree? sample-tree) #t "well-formed-tree? test1")
     (check-equal? (well-formed-tree?
                    (btree-node 4
                                (btree-node 3
                                            (btree-node 1
                                                        (btree-node 0
                                                                    (btree-leaf)
                                                                    (btree-leaf))
                                                        10)
                                            (btree-leaf))
                                (btree-leaf)))
                    #f
                    "well-formed-tree? test2")
     (check-equal? (fold-tree (lambda (x y) (+ x y 1)) 7 sample-tree) 22 "fold-tree test1")
     (check-equal? (((fold-tree-curried (lambda (x y) (+ x y 1))) 7) sample-tree) 22 "fold-tree-curried test1")
     )))

(require rackunit/text-ui)
(run-tests tests)
