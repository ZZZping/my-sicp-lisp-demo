#lang sicp
;;use newton's method

;;define suqare funxtion
(define (square x)
  (* x x));

;;define cubed function
(define (cubed x)
  (* (square x) x));

;;define average function
(define (average x y)
  (/ (+ x y) 2));

;;define improve function which statement is guess and x
;;(improve guess x)=((x/guess)+guess)/2
(define (improve guess x)
  (average guess (/ x guess)));

;;define the method for guess x^3
(define (improve-for-xxx guess x)
  (average-for-xxx (/ x (square guess)) (* guess 2)));

;;define average for xxx
(define (average-for-xxx x y)
  (/ (+ x y) 3));

;;define abs function
(define (abs x)
  (if (< x 0) (- x) x));

;;define good-enough? function
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.0001));

;; defined good-enough? for cubed
(define (good-enough?-for-cubed guess x)
  (< (abs (- (cubed guess) x)) 0.000000000001));

;;
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)));

;; xxx
(define (sqrt-iter-xxx guess x)
  (if (good-enough?-for-cubed guess x)
      guess
      (sqrt-iter-xxx (improve-for-xxx guess x) x)));

;;
(define (sqrt x)
  (sqrt-iter 1.0 x));

;;
(define (sqrt-xxx x)
  (sqrt-iter-xxx 1.0 x));

;;define new-if
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)));

(define (sqrt-iter1 guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter1 (improve guess x)
                      x)));
(define (sqrt1 x)
  (sqrt-iter1 1.0 x));
