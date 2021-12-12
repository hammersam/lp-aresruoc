;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out))

(struct var  (string) #:transparent)
(struct int  (num)    #:transparent)
(struct add  (e1 e2)  #:transparent)
(struct ifgreater (e1 e2 e3 e4)    #:transparent)
(struct fun  (nameopt formal body) #:transparent)
(struct call (funexp actual)       #:transparent)
(struct mlet (var e body) #:transparent)
(struct apair (e1 e2)     #:transparent)
(struct fst  (e)    #:transparent)
(struct snd  (e)    #:transparent)
(struct aunit ()    #:transparent)
(struct isaunit (e) #:transparent)

(struct closure (env fun) #:transparent)

(define (racketlist->mupllist rlst)
  (if (empty? rlst)
      (aunit)
      (apair (car rlst) (racketlist->mupllist (cdr rlst)))))

(define (mupllist->racketlist mlst)
  (if (equal? mlst (aunit))
      null
      (cons (apair-e1 mlst) (mupllist->racketlist (apair-e2 mlst)))))

(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(int? e) e]
        [(aunit? e) e]
        [(closure? e) e]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and
                (and (int? v1)
                     (int? v2))
                (> (int-num v1)
                   (int-num v2)))
               (eval-under-env (ifgreater-e3 e) env)
               (eval-under-env (ifgreater-e4 e) env)))]
        [(fun? e)
         (closure env e)]
        [(mlet? e)
         (let* ([v1 (eval-under-env (mlet-e e) env)]
                [new-env (cons (cons (mlet-var e) v1) env)])
           (eval-under-env (mlet-body e) new-env))]
        [(call? e)
         (let ([v1 (eval-under-env (call-funexp e) env)]
               [v2 (eval-under-env (call-actual e) env)])
           (if (closure? v1)
               (let* ([cenv (closure-env v1)]
                      [f (closure-fun v1)]
                      [fbody (fun-body f)]
                      [fname (fun-nameopt f)]
                      [fargname (fun-formal f)])
                 (eval-under-env
                  fbody
                  (cons (cons fargname v2)
                        (if fname
                            (cons (cons fname v1) cenv)
                            cenv))))
               (error "MUPL call applied to non-closure")))]
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        [(fst? e)
         (let ([v1 (eval-under-env (fst-e e) env)])
           (if (apair? v1)
               (apair-e1 v1)
               (error "MUPL fst applied to non-apair")))]
        [(snd? e)
         (let ([v1 (eval-under-env (snd-e e) env)])
           (if (apair? v1)
               (apair-e2 v1)
               (error "MUPL snd applied to non-apair")))]
        [(isaunit? e)
         (let ([v1 (eval-under-env (isaunit-e e) env)])
           (if (aunit? v1)
               (int 1)
               (int 0)))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

(define (eval-exp e)
  (eval-under-env e null))

(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (if (empty? lstlst)
      e2
      (let ([name (car (car lstlst))]
            [e (cdr (car lstlst))])
        (mlet name e (mlet* (cdr lstlst) e2)))))

(define (ifeq e1 e2 e3 e4)
  (mlet "_x" e1
    (mlet "_y" e2
      (ifaunit
        (ifgreater (var "_x") (var "_y") (aunit) (int 0))
        e4
        (ifaunit
          (ifgreater (var "_y") (var "_x") (aunit) (int 0))
          e4
          e3)))))

(define mupl-map
  (fun "step1"
       "f"
       (fun "step2"
            "mlst"
            (ifgreater (isaunit (var "mlst"))
                       (int 0)
                       (aunit)
                       (apair (call (var "f") (fst (var "mlst")))
                              (call (var "step2") (snd (var "mlst"))))))))

(define mupl-mapAddN
  (mlet "map" mupl-map
        (fun "step1" "i"
             (fun "step2" "mlst"
                  (mlet "add-N"
                        (fun "f"
                             "elem"
                             (add (var "elem") (var "i")))
                             (call (call (var "map") (var "add-N")) (var "mlst")))))))

;; (define mupl-filter
;;   (fun "get-predicate"
;;        "predicate"
;;        (fun "get-list"
;;             "mlst"
;;             (ifgreater
;;              (isaunit (var "mlst"))
;;              (int 0)
;;              (aunit)
;;              (ifgreater (call (var "predicate") (fst (var "mlst")))
;;                         (int 0)
;;                         (apair (fst (var "mlst"))
;;                                (call
;;                                 (call (var "get-predicate") (var "predicate"))
;;                                 (snd (var "mlst"))))
;;                         (call
;;                          (call (var "get-predicate") (var "predicate"))
;;                          (snd (var "mlst"))))))))
;;
;; (eval-exp (call
;;            (call
;;             mupl-filter
;;             (fun "positive?" ;; more interesting predicates?
;;                  "x"
;;                  (ifgreater (var "x")
;;                             (int 0)
;;                             (int 1)
;;                             (int 0))))
;;            (apair (int -1)
;;                   (apair (int 1)
;;                          (apair (int -2)
;;                                 (apair (int 2)
;;                                        (aunit)))))))
;;
;; (define mupl-reduce
;;   (fun "get-f"
;;        "f"
;;        (fun "get-accu"
;;             "accu"
;;             (fun "get-list"
;;                  "mlst"
;;                  (ifgreater (isaunit (var "mlst"))
;;                             (int 0)
;;                             (var "accu")
;;                             (call
;;                              (call
;;                               (call (var "get-f")
;;                                     (var "f"))
;;                               (call (var "f")
;;                                     (apair (var "accu")
;;                                            (fst (var "mlst")))))
;;                              (snd (var "mlst"))))))))
;;
;; (eval-exp
;;  (call
;;   (call (call mupl-reduce
;;               (fun #f
;;                    "p"
;;                    (add (fst (var "p"))
;;                         (snd (var "p")))))
;;         (int 0))
;;   (apair (int 0)
;;          (apair (int 1)
;;                 (apair (int 2)
;;                        (aunit))))))

;; (struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; (define (compute-free-var e)
;;   (letrec
;;       ([aux
;;         (lambda (e freevarset)
;;           (cond
;;             [(var? e)
;;              (cons e
;;                    (if (set-member? freevarset (var-string e))
;;                        (set-remove freevarset (var-string e))
;;                        (set-add freevarset (var-string e))))]
;;             [(int? e)
;;              (cons e freevarset)]
;;             [(add? e)
;;              (let* ([compute-e1 (aux (add-e1 e) freevarset)]
;;                     [compute-e2 (aux (add-e2 e) freevarset)])
;;                (cons (add (car compute-e1)
;;                           (car compute-e2))
;;                      (set-union (cdr compute-e1)
;;                                 (cdr compute-e2))))]
;;             [(ifgreater? e)
;;              (let* ([compute-e1 (aux (ifgreater-e1 e) freevarset)]
;;                     [compute-e2 (aux (ifgreater-e2 e) freevarset)]
;;                     [compute-e3 (aux (ifgreater-e3 e) freevarset)]
;;                     [compute-e4 (aux (ifgreater-e4 e) freevarset)])
;;                (cons (ifgreater (car compute-e1)
;;                                 (car compute-e2)
;;                                 (car compute-e3)
;;                                 (car compute-e4))
;;                      (set-union (cdr compute-e1)
;;                                 (cdr compute-e2)
;;                                 (cdr compute-e3)
;;                                 (cdr compute-e4))))]
;;             [(fun? e)
;;              (let* ([compute-body (aux (fun-body e) freevarset)]
;;                     [compute-e (if (set-member? freevarset (fun-formal e))
;;                                    (set-remove freevarset (fun-formal e))
;;                                    (set-add freevarset (fun-formal e)))]
;;                     [res (set-union (cdr compute-body)
;;                                      compute-e)])
;;                (cons (fun-challenge (fun-nameopt e)
;;                                     (fun-formal e)
;;                                     (car compute-body)
;;                                     res)
;;                      res))]
;;             [(call? e)
;;              (let* ([compute-funexp (aux (call-funexp e) freevarset)]
;;                     [compute-actual (aux (call-actual e) freevarset)])
;;                (cons (call (car compute-funexp)
;;                            (car compute-actual))
;;                      (set-union (cdr compute-funexp)
;;                                 (cdr compute-actual))))]
;;             [(mlet? e)
;;              (let* ([compute-var
;;                      (if (set-member? freevarset (mlet-var e))
;;                          (set-remove freevarset (mlet-var e))
;;                          (set-add freevarset (mlet-var e)))]
;;                     [compute-body (aux (mlet-body e) freevarset)]
;;                     [compute-e (aux (mlet-e e) freevarset)])
;;                (cons (mlet (mlet-var e)
;;                            (car compute-e)
;;                            (car compute-body))
;;                      (set-union compute-var
;;                                 (cdr compute-body)
;;                                 (cdr compute-e))))]
;;             [(apair? e)
;;              (let* ([compute-e1 (aux (apair-e1 e) freevarset)]
;;                     [compute-e2 (aux (apair-e2 e) freevarset)])
;;                (cons (apair (car compute-e1)
;;                             (car compute-e2))
;;                      (set-union (cdr compute-e1)
;;                                 (cdr compute-e2))))]
;;             [(fst? e)
;;              (let ([compute-e (aux (fst-e e) freevarset)])
;;                (cons (fst (car compute-e))
;;                      (cdr compute-e)))]
;;             [(snd? e)
;;              (let ([compute-e (aux (snd-e e) freevarset)])
;;                (cons (snd (car compute-e))
;;                      (cdr compute-e)))]
;;             [(isaunit? e)
;;              (let ([compute-e (aux (isaunit-e e) freevarset)])
;;                (cons (isaunit (car compute-e))
;;                      (cdr compute-e)))]
;;             [(aunit? e)
;;              (cons e freevarset)]
;;             [else (error "illegal MUPL expression")]))])
;;     (car (aux e (set)))))

;; (define (eval-under-env-c e env) "CHANGE")

;; (define (eval-exp-c e)
;;   (eval-under-env-c (compute-free-var e) null))
