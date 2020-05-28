#|
    ===> PLEASE DO NOT DISTRIBUTE THE SOLUTIONS PUBLICLY <===

   We ask that solutions be distributed only locally -- on paper, on a
   password-protected webpage, etc.

   Students are required to adhere to the University Policy on Academic
   Standards and Cheating, to the University Statement on Plagiarism and the
   Documentation of Written Work, and to the Code of Student Conduct as
   delineated in the catalog of Undergraduate Programs. The Code is available
   online at: http://www.umb.edu/life_on_campus/policies/code/

|#
;; PLEASE DO NOT CHANGE THE FOLLOWING LINES
#lang errortrace racket
(provide (all-defined-out))
(require "hw4-util.rkt")
;; END OF REQUIRES

;; Utility functions
(define (s:apply-arg1 app)
  (first (s:apply-args app)))
(define (s:lambda-param1 lam)
  (first (s:lambda-params lam)))
(define (s:lambda-body1 lam)
  (first (s:lambda-body lam)))
;; Utility functions
(define (e:apply-arg1 app)
  (first (e:apply-args app)))
(define (e:lambda-param1 lam)
  (first (e:lambda-params lam)))
(define (e:lambda-body1 lam)
  (first (e:lambda-body lam)))


;; Exercise 1
(define (s:subst exp var val)
  (cond
    [(s:number? exp) exp]
    [(and (s:variable? exp) (equal? exp var)) val]
    [(and (s:variable? exp) (not(equal? exp var))) exp]
     [(s:apply? exp)
(define fl (s:subst (s:apply-func exp) var val))
     (s:apply fl(list (s:subst (s:apply-arg1 exp) var val)))]))
     
       



;; Exercise 2
(define (s:eval subst exp)
  (cond  [(s:apply? exp)
          (s:eval subst(subst (s:lambda-body1)
                              (s:lambda-param1
                              (s:eval subst
                                      (s:apply-func exp)
                                      (s:eval subst (s:apply-arg1 exp))))))]
                                                    
         [(s:value? exp) exp]))
         



;; Exercise 3
(define (e:eval env exp)
  (cond
    [(and (hash-has-key? env exp)(e:variable? exp))(hash-ref env exp)]
    [(e:value? exp) exp]
    [(e:lambda? exp)(e:closure env exp)]
    [(e:number? exp)exp]
    [(e:apply? exp)
     (define apf (e:eval env  (e:apply-func exp)))
     (define lp  (e:lambda-param1 (e:closure-decl apf)))
     (e:eval (hash-set (e:closure-env apf) lp (e:eval env (e:apply-arg1 exp)))         
 (e:lambda-body1 (e:closure-decl (e:eval env (e:apply-func exp)))))]))





;; Exercise 4 (Manually graded)
#|


Implementing lambda racket without an environment is sometimes a better
alternative to running lambda racket with an environment for many reasons.

- It will take up less memory, no environmental variables and procedures will be present without an environment

- The evaluation search time will be less as the function evaluation wont be looking throughout the environment for a current substitue depending
on the number of variables and symbols within, however if there is a higher amount then a single variable then looking it up within an environment would
be better.



Conversely, running lambda racket with an environment is a better
alternative to running without an environment for many reasons

- An environment can bind a value for each symbol

- It makes looking up symbols and definitions much easier and faster as it is already in the environment

- It is similar to a table, creating new definitions such as (define y (* 5 5)) will put y in the symbol table as the value 25 within the environment

- The environment will allow easy access to all symbols and a much quicker access to looking up all variables and procedures

- time-wise when evaluating something such as a lambda expression within an environment will take
less time looking for it in the environment in order to run the procedure



|#






;; Exercise 5 (Manually graded)
#|


Formal Specification makes implementation of the software system more efficient for many reasons. The expressions, syntax, vocabulary
are formally defined which makes it easier to understand the software design, and whether the program fits the specifications of the software.
It also allows better understanding of how the code should work and helps the software engineering team understand how the code should be run
and implemented to get the desired outcome.



|#
