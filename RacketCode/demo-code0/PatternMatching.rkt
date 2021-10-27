#lang racket
(require racket/match)
;;(match val-expr clause ...)
;;clause = [pat body ...+]
;;         [pat (=> id) body ...+]
;;         [pat #:when cond-expr body ...+]
(define (m x)
  (match x
    [(list a b c)
     #:when (= 6 (+ a b c))
     'sum-is-six]
    [(list a b c) 'sum-is-not-six]))

(define (m1 x)
  (match x
    [(list a b c)
     (=> exit)
     (f x exit)]
    [(list a b c) 'sum-is-not-six]))
(define (f x exit)
  (if (= 6 (apply + x)) 'sum-is-six
      (exit)))


;;test
(m1 '(1 2 3))
(m1 '(1 2 4))