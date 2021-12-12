; Programming Languages, Dan Grossman
; Section 5: Using and Defining Streams

; [same code for segments on using streams and defining streams]

; A stream is an infinite sequence of values.
; - So cannot make a stream by making all the values
; - Key idea: Use a thunk to delay creating most of the sequence
; - Just a programming idiom
;
; A powerful concept for division of labor:
; - Stream producer knows how create any number of vaules
; - Stream consumer decides how many values to ask for

#lang racket

(provide (all-defined-out))

;; define some streams

;(define ones-really-bad (cons 1 ones-really-bad))
(define ones-bad (lambda () (cons 1 (ones-bad))))

; 1 1 1 1 1
(define ones (lambda () (cons 1 ones)))

; 2 3 5 7 11 ...
(define primes (lambda () (cons 2 )))

(define nats
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

(define powers-of-two
  (letrec ([f (lambda (x) (cons x (lambda () (f (* x 2)))))])
    (lambda () (f 2))))

(define (stream-maker fn arg)
  (letrec ([f (lambda (x) 
                (cons x (lambda () (f (fn x arg)))))])
    (lambda () (f arg))))
(define nats2  (stream-maker + 1))
(define powers2 (stream-maker * 2))

;; code that uses streams

(define (number-until stream tester)
  (letrec ([f (lambda (stream ans)
                (let ([pr (stream)])
                  (if (tester (car pr))
                      ans
                      (f (cdr pr) (+ ans 1)))))])
    (f stream 1)))

(define four (number-until powers-of-two (lambda (x) (= x 16))))
