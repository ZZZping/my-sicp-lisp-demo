#lang sicp
;;update sqrt function
(define (sqrt x)
  ;;define good-enough? function
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))
  ;;define square function
  (define (square x)
    (exp (double (log x))))
  ;;define double function
  (define (double x)
    (+ x x))
  ;;define abs fuction
  (define (abs x)
    (if (< x 0) (- x) x))
  ;;define improve
  (define (improve guess x)
    (average guess (/ x guess)))
  ;;define average function
  (define (average x y)
    (/ (+ x y) 2))
  ;;define sqrt-iter function
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x));
