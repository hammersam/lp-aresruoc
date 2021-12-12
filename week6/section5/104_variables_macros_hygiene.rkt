#lang racket

(provide (all-defined-out))

(define-syntax double1
  (syntax-rules ()
    [(double1 x) (+ x x)]))

(define-syntax double2
  (syntax-rules ()
    [(double2 x) (* x 2)]))

(define-syntax double3
  (syntax-rules ()
    [(double3 x) (let ([y 1]) (* 2 x y))]))

(let ([y 7]) (double3 y))

; naive expansion
(let ([y 7])
  (let ([y 1])
    (* 2 y y)))

(define-syntax db1
  (syntax-rules ()
    [(db1 x)
     (let ([y x]) (+ y y))]))

(define-syntax take
  (syntax-rules (from)
    [(take e1 from e2)
     (- e2 e1)]))

(define-syntax double4
  (syntax-rules ()
    [(double4 x) (* 2 x)]))

(let ([* +]) (double4 42))

; naive expansion
(let ([* +]) (* 2 42))

; How hygienic macros work
; A hygienic macro system:
; 1. Secretly renames local variables in macros with fresh names
; 2. Looks up variables used in macros where the macro is defined
