#lang scheme
#|
pratice1.2
|#

#|(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))|#

(define (max x y)
  (if (> x y)
      x
      y))
(define (min x y)
  (cond ((< x y) x)
        ((< y x) y)))
(define (sum x y z)
  (+ (max (max x y) z) (- (+ x y z) (max (max x y) z) (min (min x y) z) )))
(sum 10 12 15)
