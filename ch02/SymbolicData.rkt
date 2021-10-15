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

;;test
;;(memq 'apple '(orange banana prune pear));;false
;;(memq 'apple '(x (apple banana) y apple pear));;(apple pear)
;;(equal1? '(1 2 3 (4 5) 6) '(1 2 3 (4 5) 6))
;;(equal1? '(this is a list) '(this is a list))
;;(equal1? '(this is a list) '(this (is a) list))
