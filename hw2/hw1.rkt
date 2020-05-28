#lang racket
(provide (all-defined-out))

(define (lambda? node)
  (and
    (list? node)
    (>= (length node) 3)
    (equal? 'lambda (first node))
    (list? (lambda-params node))
    (andmap symbol? (lambda-params node))))
(define lambda-params cadr)
(define lambda-body cddr)

(define (apply? l)
  (and (list? l) (>= (length l) 1)))
(define apply-func car)
(define apply-args cdr)

(define (define-basic? node)
  (and
    (list? node)
    (= (length node) 3)
    (equal? 'define (car node))
    (symbol? (define-head node))))

(define (define-func? node)
  (and
    (list? node)
    (>= (length node) 3)
    (equal? 'define (car node))
    (list? (define-head node))
    (andmap symbol? (define-head node))
    (>= (length (define-head node)) 1)))

(define (define? node)
  (or
    (define-basic? node)
    (define-func? node)))

(define define-head cadr)
(define define-body cddr)

