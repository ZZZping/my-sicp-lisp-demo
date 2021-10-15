#lang sicp
;factorial 0.0
(define (factorial x)
  (if (= x 1)
      1
      (* x (factorial (- x 1)))))

;factorial 0.1
(define (factorial1 n)
  (fact-iter 1 1 n))

(define (fact-iter product count max-count)
  (if (> count max-count)
      product
      (fact-iter (* count product) (+ count 1) max-count)))

;factorial 0.2
;;(iter 1 1) == begin from count = 1 and product = 1
(define (factorial2 n)
  (define (iter product count)
    (if (> count n)
        product
        (iter (* product count) (+ count 1))))
  (iter 1 1))

;factorial 0.3
(define (factorial3 n)
  (define (fa product counter max-count)
    (if (> counter max-count)
        product
        (fa (* counter product) (+ counter 1) max-count)))
  (fa 1 1 n))
