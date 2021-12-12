
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; 1.
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

;; 2.
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

;; 3.
(define (list-nth-mod xs n)
  (cond ((null? xs) (error "list-nth-mod: empty list"))
        ((< n 0) (error "list-nth-mod: negative number"))
        (#t (car (list-tail xs (remainder n (length xs)))))))

;; 4.
(define (stream-for-n-steps s n)
  (letrec ((f (lambda (s n)
                (if (<= n 0)
                    null
                    (cons (car (s)) (f (cdr (s)) (- n 1)))))))
    (f s n)))

;; 5.
(define (funny-number-stream)
  (letrec ((f (lambda (x)
                (cons (if (= 0 (remainder x 5))
                          (- 0 x)
                          x) (lambda ()
                               (f (+ x 1)))))))
    (f 1)))

;; 6.
(define (dan-then-dog)
  (letrec ((f (lambda (s)
                (cons s (lambda () (f (if (string=? s "dog.jpg") 
                                          "dan.jpg"
                                          "dog.jpg")))))))
    (f "dan.jpg")))

;; 7.
(define (stream-add-zero stream)
  (letrec ((f (lambda (x) (cons (cons 0
                                      (car (x)))
                                (lambda () (f (cdr (x))))))))
    (lambda () (f stream))))

;; 8.
(define (cycle-lists xs ys)
  (letrec ((f (lambda (n)
                (cons (cons (list-nth-mod xs n)
                            (list-nth-mod ys n))
                      (lambda () (f (+ n 1)))))))
    (lambda () (f 0))))

;; 9.
(define (vector-assoc v vec)
  (letrec ((f (lambda (i)
                (if (>= i (vector-length vec))
                    #f
                    (let ((current-value (vector-ref vec i)))
                      (if (and (cons? current-value)
                               (equal? v (car current-value)))
                          current-value
                          (f (+ i 1))))))))
    (f 0)))

;; 10.
(define (cached-assoc input-list cache-size)
  (define cache (make-vector cache-size #f))
  (define cache-index 0)
  (lambda (v) (let ((check-cache (vector-assoc v cache)))
                (if check-cache
                    check-cache
                    (begin (vector-set! cache cache-index v)
                           (set! cache-index (remainder (+ cache-index 1) cache-size))
                           (assoc v input-list))))))

;; 11.
(define-syntax while-less
  (syntax-rules (do)
    ((while-less e1 do e2)
     (letrec ((target e1)
              (f (lambda (result) (if (< result target)
                                      (f e2)
                                      #t))))
       (f e2)))))
