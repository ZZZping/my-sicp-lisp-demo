#lang racket
;;Pattern Matching
(require racket/match)
(define (m x)
  (match x
    [(list a b c)
     #:when (= 6 (+ a b c))
     'sum-is-six]
    [(list a b c) 'sum-is-not-six]))

;;tree-sum
(define tree-sum
  (lambda (exp)
    (match exp
      [(? number? x) x]
      [`(,e1 ,e2)
        (let ([v1 (tree-sum e1)]
              [v2 (tree-sum e2)])
          (+ v1 v2))])))

;;calc
(define calc
  (lambda (exp)
    (match exp
      [(? number? x) x]
      [`(,op ,e1 ,e2) (let ([v1 (calc e1)]
                            [v2 (calc e2)])
                        (match op
                          ['+ (+ v1 v2)]
                          ['- (- v1 v2)]
                          ['* (* v1 v2)]
                          ['/ (/ v1 v2)]))])))

;;A small language R2
(define env0 '())
(define ext-env
  (lambda (x v env) (cons `(,x . ,v) env)))
(define lookup
  (lambda (x env)
    (let ([p (assq x env)])
      (cond [(not p) #f]
            [else (cdr p)]))))
(struct Closure (f env))
(define interp
  (lambda (exp env)
    (match exp
      [(? symbol? x) (let ([v (lookup x env)])
                       (cond [(not v) (error "undefined variable" x)]
                             [else v]))]
      [(? number? x) x]
      [`(lambda (,x) ,e) (Closure exp env)]
      [`(let ([,x ,e1]) ,e2) (let ([v1 (interp e1 env)])
                              (interp e2 (ext-env x v1 env)))]
      [`(,e1 ,e2) (let ([v1 (interp e1 env)]
                        [v2 (interp e2 env)])
                    (match v1
                      [(Closure `(lambda (,x) ,e) env-save) (interp e (ext-env x v2 env-save))]))]
      [`(,op ,e1 ,e2) (let ([v1 (interp e1 env)]
                            [v2 (interp e2 env)])
                        (match op
                          ['+ (+ v1 v2)]
                          ['- (- v1 v2)]
                          ['* (* v1 v2)]
                          ['/ (/ v1 v2)]))])))
(define r2
  (lambda (exp) (interp exp env0)))


;;test
(tree-sum '(1 2))
(tree-sum '(1 (2 3)))
(calc '(+ 2 3))
(calc '(* (+ 1 2) (+ 3 4)))
(calc '(/ (+ 1 2) (+ 3 4)))
(calc '(* (+ (- 5 2) (* 2 (/ 6 3))) (+ 2 2)))
(r2 '(+ 1 2))