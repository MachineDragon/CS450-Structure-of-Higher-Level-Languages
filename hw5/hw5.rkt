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
(require "hw5-util.rkt")
(require rackunit)
(provide d:eval-exp d:eval-term)
(define (d:apply-arg1 app)
  (first (d:apply-args app)))
(define (d:lambda-param1 lam)
  (first (d:lambda-params lam)))
;; END OF REQUIRES





;; Exercise 1
(define/contract (d:eval-exp mem env exp)
  (-> mem? handle? d:expression? eff?)
  (cond
   [(d:variable? exp)(eff mem (environ-get mem env exp))]
   [(d:lambda? exp)(eff mem (d:closure env exp))]
   [(d:value? exp) (eff mem exp)]
   [(d:apply? exp)
     
    (define v1+mem1 (d:eval-exp mem env (d:apply-func exp)))
    (define mem1 (eff-state v1+mem1))
    (define v1 (eff-result v1+mem1))

    (define v2+mem2 (d:eval-exp mem1 env (d:apply-arg1 exp)))  
    (define mem2 (eff-state v2+mem2))
    (define v2 (eff-result v2+mem2))

     (define ec (d:closure-env v1))
     (define dc (d:closure-decl v1))
    
   (define envp (environ-push mem2 ec (d:lambda-param1 dc v2)))
   (define envs (eff-state envp))
   (define envr (eff-result envp))
   (define dcl (d:closure-decl v1))
   (define le (d:lambda-body dcl))

   (d:eval-term envs envr)]))
   
                              









;; Exercise 2
(define/contract (d:eval-term mem env term)
  (-> mem? handle? d:term? eff?)
  'todo)











  












;; Exercise 3 (Manually graded)
#|
Variable bindings differ in lambda Racket as compared to normal Racket in terms of definitions due to the functionality of the exhisting environment
and the semantics of binding variables in them. In lambda Racket, an easier binding will cause a definition to hold the save value throughout the whole
program as long as it isnt changed, and theese definitions can be used in other functions to make a more complex program that will use its
placeholder within the function to use the value given when the variable was defined. For example (define x 10), in lambda racket throughout the whole
program x will keep its value as 10, in (define y (+ x x)), x still has a value 10 when used in another function despite being defined somewhere
else in the program.

However in normal racket, due to no environment and diffrent binding semantics, x would not keep its value and have to be manually defined over
and over again in other functions to get the desired outcome and it wont be updated due to no environment


|#






