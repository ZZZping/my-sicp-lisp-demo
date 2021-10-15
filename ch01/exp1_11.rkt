#lang sicp
;;1.11
;;recursive
(define (f1 n)
  (if (< n 3) n
      (+ (f1 (- n 1))
         (* (f1 (- n 2)) 2)
         (* (f1 (- n 3)) 3))));

;;iterative
(define (f2 n)
  (define (f2-iter a b c count)
    (if (< count 3) count
        (f2-iter (+ a
                    (* b 2)
                    (* c 3)) a b (- count 1))))
  (f2-iter 2 1 0 n))
;;(f2-iter 0 1 2 3)
;;
(define (f3 n)
  (define (f3-iter a b c count)
    (cond ((= count 0) c)
          ((= count 1) b)
          ((= count 2) a)
          (else (f3-iter (+ a (* b 2) (* c 3)) a b (- count 1)))))
  (f3-iter 2 1 0 n))

;;1.12
;;recursive
(define (pascal m n)
  (cond ((or (= n 0) (= n m)) 1)
        ((and (= n 1) (= m 2)) 2)
        (else (+ (pascal (- m 1) (- n 1)) (pascal (- m 1) n)))))

;;iterative
(define (pascal1 row col)
  (/ (factorial row) (* (factorial col) (factorial (- row col)))))
(define (factorial n) (factorial-iter 1 1 n))
(define (factorial-iter product counter max-count)
      (if (> counter max-count) product
            (factorial-iter (* counter product) (+ counter 1) max-count)))