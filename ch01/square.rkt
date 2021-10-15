#lang sicp
(define (square x)
  (* x x));

(define (sum-of-squares x y)
  (+ (square x) (square y)));

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)));

(define (sum-square x y)
  (+ (+ (square x) (square y)) (* 2 (* x y))));

