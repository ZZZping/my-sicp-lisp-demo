#lang racket
;;Pattern Matching
(require racket/match)
(define (m x)
  (match x
    [(list a b c)
     #:when (= 6 (+ a b c))
     'sum-is-six]
    [(list a b c) 'sum-is-not-six]))
(m '(1 2 3))
