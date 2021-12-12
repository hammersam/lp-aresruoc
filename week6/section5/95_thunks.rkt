; Programming Languages, Dan Grossman
; Section 5: Delayed Evaluation and Thunks

#lang racket

(provide (all-defined-out))

(define (factorial-normal x)
  (if (= x 0)
      1
      (* x (factorial-normal (- x 1)))))

(define (my-if-bad e1 e2 e3)
  (if e1 e2 e3))

(define (factorial-bad x)
  (my-if-bad (= x 0)
             1
             (* x 
                (factorial-bad (- x 1)))))

(define (my-if-strange-but-works e1 e2 e3)
  (if e1 (e2) (e3)))

(define (factorial-okay x)
  (my-if-strange-but-works (= x 0)
                           (lambda () 1)
                           (lambda () (* x (factorial-okay (- x 1))))))

;; If you have a expression that you want to
;; evaluate later, just put that expression in
;; a function
;;
;; Wrapping the expression in a lambda function delays evalution,
;; which is what it means to thunk an expression.
;; Example: (define A (lambda () (+ 1 2)))
