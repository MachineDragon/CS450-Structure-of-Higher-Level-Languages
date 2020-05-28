#lang racket
(require rackunit)
(require "hw6-util.rkt")
(require "hw6.rkt")

;;;;;;;;;;;;;;;
;; Exercise 1

(check-equal? (frame-refs (parse-frame '[(x . 2) (y . 10) (z . 0)]))
  (set))
(check-equal? (frame-refs (parse-frame '[E10 (x . 2) (y . 10) (z . 0)]))
  (set (handle 10)))

(check-equal? (frame-refs (parse-frame '[(x . (closure E0 (lambda (x) x))) (y . 10) (z . (closure E1 (lambda (z) z)))]))
  (set (handle 0) (handle 1)))

(check-equal? (frame-refs (parse-frame '[E9 (x . (closure E0 (lambda (x) x))) (y . 10) (z . (closure E1 (lambda (z) z)))]))
  (set (handle 0) (handle 1) (handle 9)))


