; Programming Languages, Dan Grossman
; Section 5: Racket Lists

; always make this the first (non-comment, non-blank) line of your file
#lang racket

; not needed here, but a workaround so we could write tests in a second file
; see getting-started-with-Racket instructions for more explanation
(provide (all-defined-out))

; list processing: null, cons, null?, car, cdr
; we won't use pattern-matching in Racket
(define (sum xs)
  (if (null? xs)
      0
      (+ (car xs) (sum (cdr xs)))))

(define (my-append xs ys) ; same as append already provided
  (if (null? xs)
      ys
      (cons (car xs) (my-append (cdr xs) ys))))

(define (my-map f xs) ; same as map already provided
  (if (null? xs)
      null
      (cons (f (car xs)) (my-map f (cdr xs)))))

(define foo (my-map (lambda (x) (+ x 1)) (cons 3 (cons 4 (cons 5 null)))))

(define (my-filter f xs)
  (cond ((null? xs) null)
        ((f (car xs)) (cons (car xs) (my-filter f (cdr xs))))
        (else (my-filter f (cdr xs)))))

(equal? (my-filter (lambda (x) (= (remainder x 2) 0)) (list 1 2 3 4 5)) '(2 4))

(define (my-left-fold f acc xs)
  (if (null? xs)
      acc
      (my-left-fold f (f (car xs) acc) (cdr xs))))

(define (my-right-fold f acc xs)
  (if (null? xs)
      acc
      (f (car xs)
         (my-right-fold f acc (cdr xs)))))
