#lang racket
;; Please, do not remove this line and do not change the function names,
;; otherwise the grader will not work and your submission will get 0 points.
(provide (all-defined-out))

(define ex1 (* (- (* 6 7) (/ 14 8)) (+ 7 13)))

(define ex2 (list
             (* (- (* 6 7) (/ 14 8)) (+ 7 13))             
             (* (- 42 (/ 14 8)) (+ 7 13))
             (* (- 42 7/4) (+ 7 13))
             (* 161/4 (+ 7 13))
             (* 161/4 20)
             805))
  
(define (ex3 x y)
   (= (- (+ 12 y) y) (- (* x y) (- y x))))




;; Constructs a tree from two trees and a value
(define (tree left value right)
  (list left value right)
  )




;; Constructs a tree with a single node
(define (tree-leaf value)
  (list empty value '())
  )



;; Accessors
(define (tree-left self)
  (car self)
  )

(define (tree-value self)
  (car (cdr self))
  )


(define (tree-right self)
  (car (cdr (cdr self)))
  )


;; Copies the source and updates one of the fields
(define (tree-set-value self value)
  (list (tree-left self) value (tree-right self))
  )

(define (tree-set-left self left)
  (list left(tree-value self)(tree-right self))
  )


(define (tree-set-right self right)
(list (tree-left self) (tree-value self) right)
  ) 

;; Function that inserts a value in a BST
(define (bst-insert self value)
  
  (cond [(equal? empty self) (tree-leaf value)]

        [(equal? (tree-value self) value) (tree-set-value self value)]

        [(< value (tree-value self)) (tree-set-left self (bst-insert (tree-left self) value))]
        
        [else (tree-set-right self (bst-insert (tree-right self) value))]))


;; lambda
(define (lambda? node)
  (and
   (list? node)
   (>= (length node) 3)
   (equal? 'lambda (car node))
   (list? (car (cdr node)))
   (andmap symbol? (car (cdr node)))
   ))

 
(define (lambda-params node)
  (car (cdr node)))

  
(define (lambda-body node)
  (cdr (cdr node)))


;; apply
(define (apply? l)
  (and
   (list? l)
   (> (length l) 0)
   (not (equal? 'lambda (car l)))))


(define (apply-func node)
  (car node))


(define (apply-args node)
  (cdr node))


;; define
(define (define? node) 
  (or
   (and
    (equal? (list? node) #t)
    (= (length node) 3)
    (equal? (car node) 'define)
    (symbol? (car(cdr node))))
   (and
    (list? node)
    (>= (length node) 3)
    (list? (car (cdr node)))
    (equal? (car node) 'define)
    (andmap symbol? (car (cdr node)))
    (not (empty? (cadr node)))
    (list? (cdr (cdr node))))))

    

(define (define-basic? node)
  (and
   (equal? (list? node) #t)
   (= (length node) 3)
   (equal? (car node) 'define)
   (symbol? (car(cdr node)))
          ))

 
(define (define-func? node)
 (and
   (list? node)
   (>= (length node) 3)
   (list? (car (cdr node)))
   (equal? (car node) 'define)
   (andmap symbol? (car (cdr node)))
   (not (empty? (cadr node)))
   (list? (cdr (cdr node)))))