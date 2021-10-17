#lang sicp
;;Symnolic Data
;;In this section we extend the representational capability
;;of our language by introducing the ability to work with arbitrary symbols as data.
;;Quotation
;;(define a 1)
;;(define b 2)
;;(list a b)
;;We only write a quotation mark at the begining of the object to be quoted
;;The single quote is different from the double quote we have been using to
;;enclose character strings to be printed. Whereas the single quote canbe
;;used to donate lists or symbols, the double quote is used only with characters strings.
;;(list 'a 'b)
;;(list 'a b)
;;(list a 'b)
;;Quotation also allows us to type in compound objects, using the
;;conventional printed representation for lists
;;(car '(a b c))
;;(cdr '(a b c))

;;memq
;;Memq tasks two arguments, a symbol and a list.
;;If the symbol is not contained in the list, the
;;memq returns false. Other wise, it returns the
;;sublist of the list beginning with the first
;;occurrence of the symble
;;item is a symbol and x is a list
;;we use a recursive for the list x,
;;if x = '(a b c), (car x) = a and let x = (cdr x) => (b c)
(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))
;;ex2.53
;;(list 'a 'b 'c);;(a b c)
;;(list (list 'george));;((george))
;;(cdr '((x1 x2) (y1 y2)));;((y1 y2))
;;(cadr '((x1 x2) (y1 y2)));;(y1 y2)
;;(pair? (car '(a short list)));;false
;;(memq 'red '((red shoes) (blue socks)));;false
;;(memq 'red '(red shoes blue socks));;(red shose blue socks)
;;(car '(y1 y2));;y1
;;(cdr '(y1 y2));;(y2)
;;(cadr '((x1 x2) (y1 y2)));;(y1 y2)
;;(cdr (cdr '((x1 x2) (y1 y2))));;()
;;(car (cdr '((x1 x2) (y1 y2))));;(y1 y2)

;;ex2.54
(define (equal? list1 list2)
  (cond ((and (not (pair? list1)) (not (pair? list2)))
         (eq? list1 list2))
        ((and (pair? list1) (pair? list2))
         (and (equal? (car list1) (car list2)) (equal? (cdr list1) (cdr list2))))
        (else false)))
;;
(define (equal1? a b)
  (if (or (eq? a b)
      (and
           (or
               (and
                    (pair? a)
                    (pair? b))
               (and
                    (null? a)
                    (null? b)))
           (and
                (equal1? (car a) (car b))
                (equal1? (cdr a) (cdr b)))))
      #t #f))

(define (equal2? a b)
  (or (eq? a b)
      (and (or (and (pair? a) (pair? b))
               (and (null? a) (null? b)))
           (and (equal2? (car a) (car b))
                (equal2? (cdr a) (cdr b))))))
(define (equal3? a b)
  (if (and (pair? a) (pair? b))
      (cond ((null? a) (null? b))
            ((null? b) false)
            ((equal3? (car a) (car b)) (equal3? (cdr a) (cdr b)))
            (else false))
      (eq? a b)))

;;ex2.55
;;(car ''abracadabra) ;;(car '(quote abracadabra))
;;(car '(cdr abracadabra))
;;(car '(list 1 2 3))
;;(car '('1111))
;;(cdr '('1111))

;;symbolic differentiation

;;Is x is a variable.
;;The variable are symbols. Idenfined by the primitive predicate symbol?
(define (variable? x) (symbol? x))
;;Are v1 and v2 the same variable.
;;Two variables are the same if the symbols representing them are eq?
(define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))
;;Construct the sum of a1 and a2
;;Sums is constructed as list
;;(define (make-sum a1 a2) (list '+ a1 a2))
;;We change make-sum so that if both summands are numbers,
;;make-sum will add them and return their sum. Also if one
;;of the summands is 0, then make-sum will return the other
;;summand.
;;(define (make-sum a1 a2)
;;  (cond ((=number? a1 0) a2)
;;        ((=number? a2 0) a1)
;;        ((and (number? a1) (number? a2))
;;         (+ a1 a2))
;;        (else (list '+ a1 a2))))
;;we can redefine make-sum again
(define (make-sum-list l)
  (if (= (length l) 2)
      (list '+ (car l) (cadr l))
      (make-sum (car l) (make-sum-list (cdr l)))))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (make-sum-list (list a1 a2)))))
;;define =number? function
;;This checks whether an expression is equal to a given number.
(define (=number? exp num)
  (and (number? exp) (= exp num)))
;;Construct the product of m1 and m2
;;Product is constructed as list
;;(define (make-product m1 m2) (list '* m1 m2))
;;Similarly, we will redefine make-product in the rules that 0
;;times anything is 0 and 1 times anything is the thing itself
;;(define (make-product m1 m2)
;;  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
;;        ((=number? m1 1) m2)
;;        ((=number? m2 1) m1)
;;        ((and (number? m1) (number? m2)) (* m1 m2))
;;        (else (list '* m1 m2))))
;;redefine make-product
(define (make-product-list l)
  (if (= (length l) 2)
      (list '* (car l) (cadr l))
      (make-product (car l) (make-product-list (cdr l)))))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (make-product-list (list m1 m2)))))
;;Is x a sum?
;;A sum is a list whose first element is the symbol +
;;(+ addend augend) <=> addend + augend
(define (sum? x) (and (pair? x) (eq? (car x) '+)))
;;Addend of the sum s
;;The addend is the second item of sum list
(define (addend s) (cadr s))
;;Augend of the sum s
;;The augend is the third item of sum list
;;(define (augend s) (caddr s))
;;redefine augend
(define (augend s)
  (let ((a (cddr s)))
    (if (= (length a) 1)
        (car a)
        (make-sum-list a))))
;;Is x a product
;;A product is a list whose first element is the symbol *
(define (product? x) (and (pair? x) (eq? (car x) '*)))
;;Multiplier of the product p
;;The multiplier is the second item of product list
(define (multiplier p) (cadr p))
;;Multiplicand of the product p
;;The multiplicand is the third item of product list
;;(define (multiplicand p) (caddr p))
;;redefine multiplicand
(define (multiplicand p)
  (let ((m (cddr p)))
    (if (= (length m) 1)
        (car m)
        (make-product-list m))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum
                     (deriv (addend exp) var)
                     (deriv (augend exp) var)))
        ((product? exp) (make-sum
                         (make-product
                          (multiplier exp)
                          (deriv (multiplicand exp) var))
                         (make-product
                          (deriv (multiplier exp) var)
                          (multiplicand exp))))
        (else
         (error "unknown expression type: DERIV" exp))))

;;ex2.56
(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '**)))
(define (base exp)
  (cadr exp))
(define (exponent exp)
  (caddr exp))
;;(define (make-exponentiation base exp)
;;  (cond ((=number? base 1) 1)
;;         ((=number? exp 1) base)
;;         ((=number? exp 0) 1)
;;         (else (list '** exp base))))
;;simplify make-exponetitation
(define (make-exponentiation base exp)
  (cond ((=number? base 1) 1)
        ((=number? exp 0) 1)
        ((=number? exp 1) base)
        ((and (number? base) (number? exp)) (expt base exp))
        (else (list '** base exp))))
(define (advanced-deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (advanced-deriv (addend exp) var) (advanced-deriv (augend exp) var)))
        ((product? exp) (let ((m1 (multiplier exp)) (m2 (multiplicand exp)))
                          (make-sum (make-product (advanced-deriv m1 var) m2) (make-product m1 (advanced-deriv m2 var)))))
        ((and (exponentiation? exp) (=number? (advanced-deriv (exponent exp) var) 0))
         (let ((b (base exp)) (e (exponent exp)))
           (make-product (advanced-deriv b var) (make-product e (make-exponentiation b (make-sum e -1))))))
        (else (list 'advanced-deriv exp var))))
(define (exponent-deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (exponent-deriv (addend exp) var) (exponent-deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp) (exponent-deriv (multiplicand exp) var))
          (make-product (exponent-deriv (multiplier exp) var) (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (make-product
           (exponent exp)
           (make-exponentiation
            (base exp)
            (if
              (number?
               (exponent exp))
              (- (exponent exp) 1)
              ('(- (exponent exp) 1)))))
          (exponent-deriv (base exp) var)))
        (else (error "unknown expression type -- DERIV" exp))))

;;ex2.57
;;ex2.57 is make-sum-list and make-product-list
;;ex2.58

;;test
;;(memq 'apple '(orange banana prune pear));;false
;;(memq 'apple '(x (apple banana) y apple pear));;(apple pear)
;;(equal1? '(1 2 3 (4 5) 6) '(1 2 3 (4 5) 6))
;;(equal1? '(this is a list) '(this is a list))
;;(equal1? '(this is a list) '(this (is a) list))
;;(deriv '(+ x 3) 'x)
;;(deriv '(* x y) 'x)
;;(deriv '(* x y) 'y)
;;(deriv '(* (* x y) (+ x 3)) 'x)
;;(deriv '(* x y (+ x 3)) 'x)
