
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below
;; produce a list of numbers from low to high separated by stride and in sorted order
;; Int Int Int -> IntList
(define (sequence low high stride)
    (cond [(> low high) null]
          [#t (cons low
                    (sequence (+ low stride) high stride))]))

;; takes a list of string xs and a string suffix and returns the list of strings where each element of xs is appended with suffix
;; StringList String -> String List
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

;; takes a list of ints xs and a number n
;; IntList Int -> Int
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs
                            (remainder n
                                       (length xs))))]))

(define (stream-maker fn arg)
  (letrec ([f (lambda (x) (cons x (lambda () (f (fn x)))))])
    (lambda () (f arg))))

;; takes a stream s and a number n and returns a list holding the first n values produced by s in order. Assume n is non-negative
;; Stream Int -> 'a List
(define (stream-for-n-steps s n)
  (cond [(= n 0) null]
        [#t (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))]))

;; stream that returns the natural numbers except that numbers divisible by 5 are negated
(define funny-number-stream
  (letrec ([f (lambda (x) (cons (if (= (remainder x 5) 0)
                                    (* x -1)
                                    x)
                                (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

;; stream where the elements alternate between "dan.jpg" and "dog.jpg"
(define dan-then-dog
  (letrec ([f1 (lambda () (cons "dan.jpg" (lambda () (f2))))]
           [f2 (lambda () (cons "dog.jpg" (lambda () (f1))))])
    (lambda () (f1))))

;; takes a stream s and returns another stream where, if s would produce val for its ith element, the function produced stream would produce the pair (0 . val)
;; Stream -> Stream
(define (stream-add-zero s)
  (letrec ([f (lambda (s) (cons (cons 0 (car (s)))
                                (lambda () (f (cdr (s))))))])
    (lambda () (f s))))

;; takes two lists xs and ys and returns a stream where the elements produced by the stream are pairs where the first part is from xs and the second part is from ys, cycling through the lists forever
;; lists do not need to be the same size but must both be non-empty
;; 'a List 'b List -> Stream
(define (cycle-lists xs ys)
  (letrec ([f (lambda (n) (cons (cons (list-nth-mod xs n)
                                      (list-nth-mod ys n))
                                (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))

;; takes a value v and a vector vec and behaves like Racket's assoc library function, except it processes a vector instead of a list, allows vector elements to not be pairs, in which case it skips them,
;;and it always takes exactly two arguments
(define (vector-assoc v vec)
  (letrec ([f (lambda (pos)
                (cond [(<= (vector-length vec) pos) #f]
                      [(pair? (vector-ref vec pos))
                       (let ([val (vector-ref vec pos)])
                         (if (equal? (car val) v)
                             val
                             (f (+ pos 1))))]
                      [#t (f (+ pos 1))]))])
    (f 0)))

;; takes a list xs and a number n and returns a function that takes one argument v. that function returns the same thing that (assoc v xs) would return.
;; uses an n-element cache of recent results 
(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [pos 0]
           [f (lambda (v) (let ([val (vector-assoc v cache)])
                            (cond [val val]
                                  [#t (let ([ans (assoc v xs)])
                                        (if ans
                                            (begin (vector-set! cache pos ans) (set! pos (remainder (+ pos 1) n)) ans)
                                            ans))])))])
    f))

;; macro (while-less e1 do e2)
(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (let ([x e1])
       (letrec ([loop (lambda ()
                        (if (< e2 x)
                            (loop)
                            #t))])
         (loop)))]))
