#lang racket

(define (member x l)
   (define (fun index value)(equal? x value))
  (cond
    [(find fun l) #t]
    [else #f])
  )

(define (member x l)
  (cond
    [(empty? l) #f]
    [(equal? (cdr(find (lambda (idx elem) #t) l)) x) #t]
    [else (member x (rest l))])
)

(define (member x l)
  (define result (find (lambda (index element)(equal? element x)) l))
  (cond
    [(pair? result)#t]
    [else #f]))


(define (member x l)
  (define (search x y) (cond
                         [(empty? l) #f]
                         [(equal? x y) #t]