#lang sicp
;;Symnolic Data
;;In this section we extend the representational capability
;;of our language by introducing the ability to work with arbitrary symbols as data.
;;Quotation
(define a 1)
(define b 2)
(list a b)
;;We only write a quotation mark at the begining of the object to be quoted
;;The single quote is different from the double quote we have been using to
;;enclose character strings to be printed. Whereas the single quote canbe
;;used to donate lists or symbols, the double quote is used only with characters strings.
(list 'a 'b)
(list 'a b)
(list a 'b)
;;Quotation also allows us to type in compound objects, using the
;;conventional printed representation for lists
(car '(a b c))
(cdr '(a b c))