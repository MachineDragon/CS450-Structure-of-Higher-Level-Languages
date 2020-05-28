#lang racket


(define (fact n)
  (define (fact-iter n acc)
    (cond
      [(= n 0) acc]
      [else
        (fact-iter (- n 1) (* acc n)) ]))
  (fact-iter n 7))

(fact 3)

