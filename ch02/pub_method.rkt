#lang sicp
(define (square x)
  (* x x))
(define (gcd a b)
  (if (= b 0) a
      (gcd b (remainder a b))))