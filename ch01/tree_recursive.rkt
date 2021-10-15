#lang sicp
;;fib 0.0
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (dec n)) (fib (- n 2)))));

;;fib 0.1
(define (fib1 n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib1 (- n 1)) (fib1 (- n 2))))));

;;fib 0.3
;;
(define (fib3 n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n));

;;Counting change
(define (count-change amount) (count-change-iter amount 5))
(define (count-change-iter amount kinds-of-coins)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (= kinds-of-coins 0)) 0)
          (else (+ (count-change-iter amount
                                      (- kinds-of-coins 1))
                   (count-change-iter (- amount
                                       (first-denomination
                                        kinds-of-coins))
                                    kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
    (cond ((= kinds-of-coins 1) 1)
          ((= kinds-of-coins 2) 5)
          ((= kinds-of-coins 3) 10)
          ((= kinds-of-coins 4) 25)
          ((= kinds-of-coins 5) 50)))