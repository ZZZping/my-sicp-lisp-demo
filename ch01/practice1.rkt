#lang sicp
;1.1
(+ 5 3 4);
(- 9 1);
(/ 6 2);
(+ (* 2 4) (- 4 6));
(define a 3);
(= a 3);
(define b (+ a 1));
(+ a b (* a b));
(= a b);
(if (and (> b a) (< b (* a b)))
         b
         a);
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25));
(+ 2 (if (> b a) b a));
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1));
;;1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)));
;;1.3
;;Define a procedure that takes three numbers
;;as arguments and returns the sum of the squares of the two
;;larger numbers.
(define (max x y)
  (cond ((> x y) x)
        ((= x y) x)
        ((< x y) y)));
(define (thMax k p q)
  (max k (max p q)));
(define (square x)
  (* x x));
(define (min x y)
  (if (> x y)
      y
      x));
(define (thMin x y z)
  (min x (min y z)));
(define (two-max-square x y z)
  (+ (square (thMax x y z)) ( square (- (+ x y z) (thMax x y z) (thMin x y z)))));

;;1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b));
;;if b > 0, return "+", then (+ a b) is "a + b",
;;else return "-", then (- a b) is "a - b"

;;1.5
(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y));
;;1.6
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)));