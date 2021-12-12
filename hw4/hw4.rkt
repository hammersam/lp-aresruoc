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
  (map (lambda (s) (string-append s suffix)) xs))

;; 3.
(define (list-nth-mod xs n)
  (cond ((< n 0) (error "list-nth-mod: negative number"))
        ((empty? xs) (error "list-nth-mod: empty list"))
        (else (car (list-tail xs (remainder n (length xs)))))))

;; 4.
(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let ([next (s)])
        (cons (car next) (stream-for-n-steps (cdr next) (- n 1))))))

(define nats
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

;; 5.
(define funny-number-stream
  (letrec ([f (lambda (x) (cons (if (= (remainder x 5) 0) (- x) x)
                                (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

;; 6.
(define dan-then-dog
  (letrec ([f (lambda (x) (cons (if (= (remainder x 2) 1) "dan.jpg" "dog.jpg")
                                (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

;; 7.
(define (stream-add-zero s)
  (letrec ([f (lambda (s)
                (let ([next (s)])
                  (cons (cons 0 (car next))
                        (lambda () (f (cdr next))))))])
    (lambda () (f s))))

;; 8.
(define (cycle-lists xs ys)
  (letrec ([f (lambda (lxs lys)
                (cons (cons (car lxs) (car lys))
                      (lambda () (f (if (empty? (cdr lxs)) xs (cdr lxs))
                                    (if (empty? (cdr lys)) ys (cdr lys))))))])
    (lambda () (f xs ys))))

;; 9.
(define (vector-assoc v vec)
  (letrec ([len (vector-length vec)]
           [aux (lambda (n)
                  (if (= n len)
                      #f
                      (let ([x (vector-ref vec n)])
                        (if (and (pair? x) (equal? (car x) v))
                            x
                            (aux (+ n 1))))))])
    (aux 0)))

;; 10.
(define (cached-assoc xs n)
  (let ([cache (make-vector n #f)]
        [pointer 0])
    (lambda (v)
      (let ([cached-res (vector-assoc v cache)])
        (if (not cached-res)
            (let ([assoc-res (assoc v xs)])
              (if (not assoc-res)
                  #f
                  (begin (vector-set! cache pointer assoc-res)
                         (print "Cache is not used.")
                         (set! pointer (if (= (+ pointer 1) n)
                                           0
                                           (+ pointer 1)))
                         assoc-res)))
            (begin (print "Cache is being used.")
                   cached-res))))))

;; (define f (cached-assoc (list (cons 1 2) (cons 3 4) (cons 5 6)) 2))
;; (f 1)
;; (f 1)
;; (f 3)
;; (f 2)
;; (f 5)

(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec ([tmp1 e1]
              [f (lambda ()
                   (let ([tmp2 e2])
                     (if (and (number? tmp2) (< tmp2 tmp))
                         (f)
                         #t)))])
       (f))]))

;; (define a 2)
;; (while-less 7 do (begin (set! a (+ a 1)) (print "x") a))
;; (while-less 7 do (begin (set! a (+ a 1)) (print "x") a))
