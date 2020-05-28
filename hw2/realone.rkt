#lang racket




(define (parse-ast node)
  (define (make-define-func node)
    (define-func? node)
   (r:define (parse-ast (first (define-head node)))(parse-ast (cons 'lambda (cons (rest (define-head node)) (define-body node)))))
  )
  (define (make-define-basic node)
    (define-basic? node)
    (r:define  (parse-ast (define-head node)) (parse-ast (car (define-body node))))
  )
  (define (make-lambda node)
    (lambda? node)
    (r:lambda  (map parse-ast (lambda-params node)) (map parse-ast (lambda-body node)))
  )
  (define (make-apply node)
    (apply? node)
    (r:apply (parse-ast(apply-func node)) (map parse-ast(apply-args node)))
   )
  
  (define (make-number node)
    (real? node)(r:number node))
  
  (define (make-variable node)
    (symbol? node)(r:variable node))
  
  
  
  (cond
    [(define-basic? node) (make-define-basic node)]
    [(define-func? node) (make-define-func node)]
    [(symbol? node) (make-variable node)]
    [(real? node) (make-number node)]
    [(lambda? node) (make-lambda node)]
    [else (make-apply node)]))