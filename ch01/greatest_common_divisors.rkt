#lang sicp
;;GCD(a,b)
(define (gcd0 a b)
  (if (= b 0) a
      (gcd0 b (remainder a b))))

;;search for divisors
(define (divides? a b)
  (= (remainder b a) 0))

(define (square x)
  (* x x))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (small-divisor n)
  (find-divisor n 2))

(define (prime? n)
  (= n (small-divisor n)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (cube x) (* x x x))

(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(define (sum-cube a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cube (+ a 1) b))))

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))


;;term and next are two procedures
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

(define (inc n) (+ n 1))
(define (sum-cube-inc a b)
  (sum cube a inc b))

(define (identity x) x)
(define (sum0integers a b)
  (sum identity a inc b))

(define (pi-sums a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

;;1.3.2 lambda
(lambda (x) (+ x 4))
;;||
(define (plus4 x) (+ x 4))
;;||
;;(define plus4 (lambda (x) (+ x 4)))
(lambda (x) (/ 1.0 (* x (+ x 2))))
(define (pi-sum-lambda a b)
  (sum
   (lambda (x) (/ 1.0 (* x (+ x 2))))
   a
   (lambda (x) (+ x 4))
   b))
(define (integral-lambda f a b dx)
  (* (sum f (+ a (/ dx 2.0)) (lambda (x) (+ x dx)) b)))

((lambda (x y z) (+ x y (square z))) 1 2 3)
((lambda (x y z) (+ (square x) (cube y) z)) 1 2 3)

;;without lambda
(define (f x y)
  (define (f-helper a b)
    (+ (* x (square a b))
       (* y b)
       (* a b)))
  (f-helper (+ 1 (* x y))
            (- 1 y)))
;;with lambda
(define (f-lambda x y)
  ((lambda (a b)
     (+ (* x (square a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))

;;with let
(define (f-let x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

;;maybe binary search
;;control precision
(define (close-enough? x y) (< (abs (- x y)) 0.001))
(define (average x y) (/ (+ x y) 2))
((lambda (x y) (/ (+ x y) 2)) 3 4)

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

;;change in lambda
;;(let ((average1 (/ (+ x y) 2))))
(define (search-change f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if ((lambda (x y) (< (abs (- x y)) 0.001)) neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Vlues are not of opposite sign" a b)))))

(half-interval-method sin 2.0 4.0)
(half-interval-method (lambda (x) (- (cube x) (* 2 x) 3)) 1.0 2.0)

;;find fixed points of functions
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough1? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough1? guess next)
          next
          (try next))))
  (try first-guess))
(fixed-point sin 1.0)

(define (sqrt1 x)
  (fixed-point (lambda (y) (/ x y)) 1.0))

(define (sqrt2 x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

;;return a lambda procedure
;;f is also a procedure, also a argument
;;
(define (average-damp f)
  (lambda (x) (average x (f x))))
;;
((average-damp cube) 10)
(define (sqrt3 x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

(define dx 0.00001)
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))
(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))
(define (sqrt4 x)
  (newtons-method
   (lambda (y) (- (square y) x)) 1.0))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))
(define (sqrt5 x)
  (fixed-point-of-transform
   (lambda (y) (/ x y)) average-damp 1.0))
(define (sqrt6 x)
  (fixed-point-of-transform
   (lambda (y) (- (square y) x)) newton-transform 1.0))
(sqrt4 3)
(sqrt5 3)
(sqrt6 3)
