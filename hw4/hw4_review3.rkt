
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (cond [(> low high) null]
        [else (cons low (sequence (+ low stride) high stride))]
        ))

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [else (car (list-tail xs (remainder n (length xs))))]))

(define (stream-for-n-steps s n)
  (cond [(= n 0) null]
        [else (cons (car (s)) (stream-for-n-steps (cdr (s))(- n 1)))]))


(define funny-number-stream
  (letrec ([f (lambda (x) (cons (if (= 0 (remainder x 5)) (- x) x) (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

(define dan-then-dog
  (letrec ([f (lambda (s) (cons s (lambda () (f (string=? s "dan.jpg") "dog.jpg" "dan.jpg"))))])
    (lambda () (f "dan.jpg"))))

(define (stream-add-zero s)
  (letrec ([f (lambda (x) (cons (cons 0 (car (x))) (lambda () (f (cdr (x))))))])
    (lambda () (f s))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (n) (cons (cons (list-nth-mod xs n) (list-nth-mod ys n)) (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))

(define (vector-assoc v vec)
  (letrec ([f (lambda (n)
                (cond [(>= n (vector-length vec)) #f]
                      [else (cond [(not (pair? (vector-ref vec n))) (f (+ n 1))]
                                  [(equal? (car (vector-ref vec n)) v) (vector-ref vec n)]
                                  [else (f (+ n 1))])]))])
    (f 0)))


(define (cached-assoc xs n)
  (letrec ([memo (make-vector n #f)]
           [memo-pos 0]
           [f (lambda (x)
                (let ([cache-vector (vector-assoc x memo)])
                  (if cache-vector
                      cache-vector
                      (let ([list-vector (assoc x xs)])
                        (begin
                          (when list-vector
                            (vector-set! memo memo-pos list-vector)
                            (set! memo-pos (remainder (+ memo-pos 1) n)))
                          list-vector)))))])
    f))
