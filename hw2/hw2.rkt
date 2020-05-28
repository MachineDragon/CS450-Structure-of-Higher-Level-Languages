#lang racket
#|
            #####################################################
            ###  PLEASE DO NOT DISTRIBUTE SOLUTIONS PUBLICLY  ###
            #####################################################

  Copy your solution of HW1 as file "hw1.rkt". The file should be in the same
  directory as "hw2.rkt" and "ast.rkt".
|#
(require "ast.rkt")
(require "hw1.rkt")
(require rackunit)
(provide (all-defined-out))
;; ^^^^^ DO NOT CHANGE ANY CODE ABOVE THIS LINE ^^^^^


;; Exercise 1.a: Read-write cell
;; Solution has 3 lines.
(define (rw-cell x) (define (place a)
                      (cond [(equal? 1 (length a)) (rw-cell (first a))]
                            [else (empty? a) x] )) place)
  

;; Exercise 1.b: Read-only cell
;; Solution has 4 lines.
(define (ro-cell x) (define (place a)
                      (cond [(equal? 1 (length a)) (ro-cell x)]
                            [else (empty? a) x] )) place)
;; Exercise 2: Interperse
;; Solution has 11 lines.
(define (intersperse l v)
(define (pair accum l)
  (cond
    [(empty? l) (accum l)]
    [(empty? (rest l)) (accum l)]
    [else
     (define top (first l))
     (define bottom (rest l))
     (pair
      (lambda(c)(accum (cons top (cons v c))))
     bottom)]))
  (pair (lambda (c) c )l))


  

;; Exercise 3.a: Generic find
;; Solution has 7 lines.
(define (find pred l)
  (define (generic l len)
	(cond 
          [(null? l) #f]
          [(pred len (first l))(cons len (first l))]
          [else (generic (rest l) (+ 1 len))]))
  (define place 0)
  (define val l)
  (generic val place))






;; Exercise 3.b: Member using find
;; Solution has 3 lines.
(define (member x l)
  (cond [(empty? l) #f]
        [(equal? x (car l)) #t]
        [(equal? x (cdr l)) #t]
        [(equal? x (car(cdr l))) #t]
        [(equal? x (cdr(cdr l))) #t]
        [else #f]))
  
  
  

















;; Exercise 3.c: index-of using find
;; Solution has 4 lines.
(define (index-of l x) 'todo)








;; Exercise 4: uncurry, tail-recursive
;; Solution has 8 lines.
(define (uncurry f)
  (define(c accum)
  (define (fact accum f)
    (cond
      [(equal? accum '()) f]
      [(equal? '() (rest accum)) (f (first accum))]
      [else(fact (rest accum)(f(first accum)))]))
    (fact accum f))c)
      
      











;; Exercise 5: Parse a quoted AST
;; Solution has 26 lines.
(define (parse-ast node)
  
(define (make-define-func node)
  (define rs (map r:variable (rest (second node))))
  (define qs (map parse-ast (cdr(cdr node))))
    (r:define (r:variable (first (second node))) 
              (r:lambda rs qs)))
               
               
              
  
  (define (make-define-basic node)
    (r:define
     (r:variable (second node))
     (parse-ast (third node))))

  
  (define (make-lambda node)
    (define mp1 (map r:variable (second node)))
    (define mp2 (map parse-ast (cdr(cdr node)))) 
    (r:lambda mp1 mp2))
  
  (define (make-apply node)
    (r:apply (parse-ast (first node))(map parse-ast (rest node))))

  
  (define (make-number node)
    (r:number node))

  
  (define (make-variable node)
    (r:variable node))


  (cond
    [(define-basic? node) (make-define-basic node)]
    [(define-func? node) (make-define-func node)]
    [(symbol? node) (make-variable node)]
    [(real? node) (make-number node)]
    [(lambda? node) (make-lambda node)]
    [else (make-apply node)]))
