#lang racket

(provide (all-defined-out))

;; (define nat
;;   (letrec ([aux (lambda (n)
;;                   (cons n (lambda () (aux (+ n 1)))))])
;;     (lambda () (aux 1))))

(define (reverse xs)
  (letrec ([aux (lambda (xs ys)
                  (if (null? xs)
                      ys
                      (aux (cdr xs) (cons (car xs) ys))))])
    (aux xs '())))

;; palindromic
(define (palindromic xs)
  (letrec ([aux (lambda (xs ys)
                  (if (null? xs)
                      '()
                      (cons (+ (car xs) (car ys))
                            (aux (cdr xs) (cdr ys)))))])
    (aux xs (reverse xs))))

(equal? (palindromic (list 1 2 4 8)) (list 9 6 6 9))

;; fibonacci
(define fib
  (letrec ([f (lambda (x y) (cons y (lambda () (f y (+ x y)))))])
    (lambda () (f 0 1))))

(define (get-until s n)
  (if (= n 0)
      '()
      (cons (car (s)) (get-until (cdr (s)) (- n 1)))))

;; stream-until
(define stream-until
  (lambda (f s)
    (if (f (car (s)))
        (+ 1 (stream-until f (cdr (s))))
        1)))

;; stream-map
(define stream-map
  (lambda (f s)
    (letrec ([aux (lambda (f s) (cons (f (car (s))) (lambda () (aux f (cdr (s))))))])
      (lambda () (aux f s)))))

;; stream-zip
(define stream-zip
  (lambda (sx sy)
    (letrec ([aux (lambda (sx sy) (cons (cons (car (sx)) (car (sy)))
                                        (lambda () (aux (cdr (sx)) (cdr (sy))))))])
      (lambda () (aux sx sy)))))

;; Why can you not write a function stream-reverse that is like Racket's
;; reverse function for lists but works on stream?

(define append
  (lambda (xs x)
    (if (null? xs)
        (list x)
        (cons (car xs) (append (cdr xs) x)))))

(define interleave
  (lambda (slst)
    (letrec ([aux (lambda (slst)
                    (let ([s-val (car ((car slst)))]
                          [s-next (cdr ((car slst)))])
                      (cons s-val (lambda () (aux (append (cdr slst) s-next))))))])
      (lambda () (aux slst)))))

(define sqrt-stream
  (lambda (n)
    (letrec ([aux (lambda (x)
                    (let ([x (/ (+ x (/ n x)) 2)])
                      (cons x (lambda () (aux x)))))])
    (lambda () (aux 1.0)))))

(define pack
  (lambda (n s)
    (letrec ([aux (lambda (s)
                    (letrec ([get-n (lambda (n s)
                                      (if (= n 0)
                                          (cons s '())
                                          (let ([p (get-n (- n 1) (cdr (s)))])
                                            (cons (car p) (cons (car (s)) (cdr p))))))])
                      (let ([p (get-n n s)])
                        (cons (cdr p) (lambda () (aux (car p)))))))])
      (lambda () (aux s)))))
