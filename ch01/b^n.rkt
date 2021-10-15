#lang sicp
(define (cube x)
   (* x x x));
(define (p x)
  (- (* 3 x) (* 4 (cube x))));
(define (sine x)
  (if (not (> (abs x) 0.1)) x
      (p (sin (/ x 3.0)))));
;;(sin 12.15)
;;12.15 > 0.1
;; (p (sin (/ 12.15 3)))
;;

;;b^n
(define (b_n b n)
  (if (= n 0) 1
      (* b (b_n b (- n 1)))));

(define (b_n_1 b n)
  (exp-iter-b b n 1));
(define (exp-iter-b b counter product)
  (if (= counter 0) product
      (exp-iter-b b (- counter 1) (* b product))));

(define (fast_b_n b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast_b_n b (/ n 2))))
        (else (* b (fast_b_n b (- n 1))))));
;;判断是否是偶数
(define (even? n)
  (= (remainder n 2) 0));
(define (square n)
  (* n n));

;;1.16
(define (b_n_2 b n)
  (fast-exp-iter-b b n 1))
(define (fast-exp-iter-b b counter product)
  (cond ((= counter 0) product)
        ((even? counter) (fast-exp-iter-b (square b) (/ counter 2) product))
        ( else (fast-exp-iter-b b (- counter 1) (* product b)))));

;;1.17
(define (double x)
  (* 2 x))

(define (havle x)
  (/ x 2))

(define (odd? x)
  (= (remainder x 2) 1))

(define (multi a b)
  (cond ((= b 0) 0)
        ((even? b) (double (multi a (havle b))))
        ((odd? b) (+ a (multi a (- b 1))))))

;;1.18
(define (multi-iter a b product)
  (cond ((= b 0) product)
        ((even? b) (multi-iter (double a) (havle b) product))
        (else (multi-iter a (- b 1) (+ a product)))))
(define (multi1 a b)
  (multi-iter a b 0))

;;1.19
(define (fib-iter a b p q count)
  (cond ((= count 0) 0)
        ((even? count) (fib-iter a b (+ (square p) (square q)) (* 2 (* p q)) (/ count 2)))
        (else (fib-iter (+ (* b q) (* a (+ p q))) (+ (* b p) (* a q)) p q (- count 1)))))
(define (fib n)
  (fib-iter 1 0 0 1 n))