#lang errortrace racket
#|
    ===> PLEASE DO NOT DISTRIBUTE SOLUTIONS NOR TESTS PUBLICLY <===

   We ask that solutions be distributed only locally -- on paper, on a
   password-protected webpage, etc.

   Students are required to adhere to the University Policy on Academic
   Standards and Cheating, to the University Statement on Plagiarism and the
   Documentation of Written Work, and to the Code of Student Conduct as
   delineated in the catalog of Undergraduate Programs. The Code is available
   online at: http://www.umb.edu/life_on_campus/policies/code/

|#
(require rackunit)
(require "ast.rkt")
(provide (all-defined-out))

;; Exercise 1.a
(define p:empty (delay '()))

;; Exercise 1.b
(define (p:empty? p) (cond [(and (equal? '() (force p))(empty? (force p))) #t]
                           [else #f]
                           ))


;; Exercise 1.c
;; It is possible to implement a function (P:cons x 1)
;; where x is not evaluated, we do this by using the delay
;; function in lazy racket
;; every tail of a list is a promise therefore l
;; is promised



;(define (p:cons x l)
;(cond [(equal? '() x) "empty head"]
     ; [(empty? l) "empty tail"]
     ; [else (delay (cons x l))]))


;; returns nth item of the list to evaluate tail and not x 
;;(define (lazy-filter x l)
;;(cond [(equal? l 0) (p:cons ls) (- n 1)]))






;; Exercise 1.d
(define (p:first l) (car(force l)))

;; Exercise 1.e
(define (p:rest l) (cdr(force l)))


;; Exercise 1.f

(define (p:append l1 l2)
    (cond [(p:empty? l1) l2]
          [else (delay (cons (p:first l1) (p:append (p:rest l1) l2)))]))

;; Exercise 2.a
;; Auxiliary functions
(define (tree-left self) (first self))
(define (tree-value self) (second self))
(define (tree-right self) (third self))

(define (bst->list self)
  (cond [(empty? self) self]
        [else
         (append
           (bst->list (tree-left self))
           (cons (tree-value self)
                 (bst->list (tree-right self))))]))

(define (bst->p:list self)
  (cond
    [(null? self) p:empty]
    [else (p:append (bst->p:list (tree-left self))
                    (delay (cons (tree-value self)
                                 (bst->p:list(tree-right self)))))]))


;; Exercise 2.b
;; Lazy evaluation can outperform eager evaluation
;; due to the fact that eager evaluation goes through
;; each line and performing each function, however lazy evaluation
;; only uses that certain function when needed, saving time and memory
;;

;(append (list 1 2) (list 3 4))
;'(1 2 3 4)


;; (p:append x y)
;; (define list1 (list 1 2 3))
;;(define list2 (list 4 5 6))


;; (define eager (p:append list1 list2 ))
;; ----> (list 1 2 3 4 5 6 7 8 9)

;(define lazy (delay(p:append list1 list2)))
;; -----> first has to be called before the evaluation can take
;; place due to the delay, will give us a much shorter answer if
;; you want to see the certain elements appended first which is a more
;; effecient and simpler way to display an append of 2 diffrent lists
;; or elements within a list as compared to the whole entire long list
;; as related to eager evaluation.



;; Exercise 3
;; Auxiliary functions
(define (stream-get stream) (car stream))
(define (stream-next stream) ((cdr stream)))
(define (stream-foldl f a s)
  (define (stream-fold a1 s1)
    (thunk
     (define strg (f (stream-get s1) a1))
     (cons a1 (stream-fold strg (stream-next s1)))))
((stream-fold a s)))






 ;; Exercise 4
(define (stream-skip n s)
  (cond [(equal? n 0) s]
        [(< n 0) s]
        [else (stream-skip (- n 1) (stream-next s))]))






;; Exercise 5

(struct r:bool (value) #:transparent)


(define andf
  (lambda v
    (cond
      [(empty? v) #t]
      [(equal? 1 (length v)) (first v)]
      [(first v)
       (cond [(or (equal? #f (first v)) (equal? #f (rest v))) #f]
             [else (apply andf (rest v))])]
      [else #f])))




(define addf
  (lambda v
    (cond
      [(empty? v) 0]
      [(equal? 1 (length v)) (first v)]
      [(equal? 2 (length v)) (+ (first v)(second v))]
      [(equal? 3 (length v)) (+ (first v)(second v)(third v))]
      [(equal? 4 (length v)) (+ (first v)(second v)(third v)(fourth v))])))
     
      

(define (r:eval-builtin sym)
  (cond [(equal? sym '+) addf]
        [(equal? sym '*) *]
        [(equal? sym '-) -]
        [(equal? sym '/) /]
        [(equal? sym 'and) andf]
        [else #f]))

(define (r:eval-exp exp)
  (cond
    [(r:bool? exp) (r:bool-value exp)]
    ; 1. When evaluating a number, just return that number
    [(r:number? exp) (r:number-value exp)]
    ; 2. When evaluating an arithmetic symbol,
    ;    return the respective arithmetic function
    [(r:variable? exp) (r:eval-builtin (r:variable-name exp))]
    ; 3. When evaluating a function call evaluate each expression and apply
    ;    the first expression to remaining ones
    [(r:apply? exp)
     ((r:eval-exp (r:apply-func exp))
      (r:eval-exp (first (r:apply-args exp)))
      (r:eval-exp (second (r:apply-args exp))))]
    [else (error "Unknown expression:" exp)]))
