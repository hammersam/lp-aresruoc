; Programming Languages, Dan Grossman
; Section 6: What Your Interpreter Can and Cannot Assume

#lang racket

(provide (all-defined-out))

; a larger language with two kinds of values, booleans and numbers
; an expression is any of these:
(struct const (int) #:transparent) ; int should hold a number
(struct negate (e1) #:transparent)  ; e1 should hold an expression
(struct add (e1 e2) #:transparent) ; e1, e2 should hold expressions
(struct multiply (e1 e2) #:transparent) ; e1, e2 should hold expressions
(struct bool (b) #:transparent) ; b should hold #t or #f
(struct eq-num (e1 e2) #:transparent) ; e1, e2 should hold expressions
(struct if-then-else (e1 e2 e3) #:transparent) ; e1, e2, e3 should hold expressions
; a value in this language is a legal const or bool

(define (eval-exp-own e)
  (cond [(const? e) e]
        [(negate? e)
         (let ([evaled (eval-exp-own (negate-e1 e))])
           (if (const? evaled)
               (const (- (const-int evaled)))
               (error "expected const expression")))]
        [(add? e)
         (let ([evaled1 (eval-exp-own (add-e1 e))]
               [evaled2 (eval-exp-own (add-e2 e))])
           (if (and (const? evaled1)
                    (const? evaled2))
               (const (+ (const-int evaled1)
                         (const-int evaled2)))
               (error "expected const expression")))]
        [(multiply? e)
         (let ([evaled1 (eval-exp-own (multiply-e1 e))]
               [evaled2 (eval-exp-own (multiply-e2 e))])
           (if (and (const? evaled1)
                    (const? evaled2))
               (const (* (const-int evaled1)
                         (const-int evaled2)))
               (error "expected const expression")))]
        [(bool? e) e]
        [(eq-num? e)
         (let ([evaled1 (eval-exp-own (eq-num-e1 e))]
               [evaled2 (eval-exp-own (eq-num-e2 e))])
           (if (and (const? evaled1)
                    (const? evaled2))
               (bool (= (const-int evaled1)
                        (const-int evaled2)))
               (error "expected const expression")))]
        [(if-then-else? e)
         (let ([loob (eval-exp-own (if-then-else-e1 e))])
           (if (bool? loob)
               (if (bool-b loob)
                   (eval-exp-own (if-then-else-e2 e))
                   (eval-exp-own (if-then-else-e3 e)))
               (error "if-then-else-e1 expected bool expression")))]
        [else (error "expected legal expression")]))

(equal?
 (eval-exp-own (const 1))
 (const 1))

(eval-exp-own
 (eq-num (const 1)
         (const 1)))

(equal?
 (eval-exp-own (bool #t))
 (bool #t))

(equal?
 (eval-exp-own (negate (const 10)))
 (const -10))

(eval-exp-own
 (eq-num
  (negate (const 10))
  (const -10)))

(equal?
 (eval-exp-own (add (const 10)
                    (negate (const 10))))
 (const 0))

(eval-exp-own
 (eq-num
  (add (const 10)
       (negate (const 10)))
  (const 0)))

(equal?
 (eval-exp-own (multiply (add (const 10)
                              (const 11))
                         (negate (const 2))))
 (const -42))

(eval-exp-own
 (eq-num
  (multiply (add (const 10)
                 (const 11))
            (negate (const 2)))
  (const -42)))

(equal?
 (eval-exp-own
  (if-then-else
   (bool #t)
   (add (const 3)
        (const 4))
   (negate (const 7))))
 (const 7))

(equal?
 (eval-exp-own
  (if-then-else
   (bool #f)
   (add (const 3)
        (const 4))
   (negate (const 7))))
 (const -7))

;; (define test1 (multiply (negate (add (const 2)
;;                                      (const 2)))
;;                         (const 7)))
;;
;; (define test2 (multiply (negate (add (const 2)
;;                                      (const 2)))
;;                         (if-then-else (bool #f)
;;                                       (const 7)
;;                                       (bool #t))))
;;
;; (define non-test (multiply (negate (add (const #t)
;;                                         (const 2)))
;;                            (const 7)))
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;; (define (eval-exp-wrong e)
;;   (cond [(const? e)
;;          e]
;;         [(negate? e)
;;          (const (- (const-int (eval-exp-wrong (negate-e1 e)))))]
;;         [(add? e)
;;          (let ([i1 (const-int (eval-exp-wrong (add-e1 e)))]
;;                [i2 (const-int (eval-exp-wrong (add-e2 e)))])
;;            (const (+ i1 i2)))]
;;         [(multiply? e)
;;          (let ([i1 (const-int (eval-exp-wrong (multiply-e1 e)))]
;;                [i2 (const-int (eval-exp-wrong (multiply-e2 e)))])
;;            (const (* i1 i2)))]
;;         [(bool? e)
;;          e]
;;         [(eq-num? e)
;;          (let ([i1 (const-int (eval-exp-wrong (eq-num-e1 e)))]
;;                [i2 (const-int (eval-exp-wrong (eq-num-e2 e)))])
;;            (bool (= i1 i2)))] ; creates (bool #t) or (bool #f)
;;         [(if-then-else? e)
;;          (if (bool-b (eval-exp-wrong (if-then-else-e1 e)))
;;              (eval-exp-wrong (if-then-else-e2 e))
;;              (eval-exp-wrong (if-then-else-e3 e)))]
;;         [#t (error "eval-exp expected an exp")] ; not strictly necessary but helps debugging
;;         ))
;;
;; (define (eval-exp e)
;;   (cond [(const? e)
;;          e]
;;         [(negate? e)
;;          (let ([v (eval-exp (negate-e1 e))])
;;            (if (const? v)
;;                (const (- (const-int v)))
;;                (error "negate applied to non-number")))]
;;         [(add? e)
;;          (let ([v1 (eval-exp (add-e1 e))]
;;                [v2 (eval-exp (add-e2 e))])
;;            (if (and (const? v1) (const? v2))
;;                (const (+ (const-int v1) (const-int v2)))
;;                (error "add applied to non-number")))]
;;         [(multiply? e)
;;          (let ([v1 (eval-exp (multiply-e1 e))]
;;                [v2 (eval-exp (multiply-e2 e))])
;;            (if (and (const? v1) (const? v2))
;;                (const (* (const-int v1) (const-int v2)))
;;                ((error "multiply applied to non-number"))))]
;;         [(bool? e)
;;          e]
;;         [(eq-num? e)
;;          (let ([v1 (eval-exp (eq-num-e1 e))]
;;                [v2 (eval-exp (eq-num-e2 e))])
;;            (if (and (const? v1) (const? v2))
;;                (bool (= (const-int v1) (const-int v2))) ; creates (bool #t) or (bool #f)
;;                (error "eq-num applied to non-number")))]
;;         [(if-then-else? e)
;;          (let ([v-test (eval-exp (if-then-else-e1 e))])
;;            (if (bool? v-test)
;;                (if (bool-b v-test)
;;                    (eval-exp (if-then-else-e2 e))
;;                    (eval-exp (if-then-else-e3 e)))
;;                (error "if-then-else applied to non-boolean")))]
;;         [#t (error "eval-exp expected an exp")] ; not strictly necessary but helps debugging
;;         ))
