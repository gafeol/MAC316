#lang plai

(define-type  ExprS
  [numS     (n number?)]
  [plusS    (l ExprS?) (r ExprS?)]
  [minusS (l ExprS?) (r S?)]
  [uminusS (l ExprS?)]
  [multS    (l ExprS?) (r ExprS?)])

(define-type  FunDefC
  [fdC (name symbol?) (arg symbol?) (body ExprC?)])

(define-type  ExprC
  [numC   (n number?)]
  [plusC (l ExprC?) (r ExprC?)]
  [multC (l ExprC?) (r ExprC?)])

(define (parse s)
  (cond
    [(number? s) (numS s)]
    [(list? s)
     (let ((sl s))
       (case (first  sl)
         [(+) (plusS (parse (second  sl)) (parse (third  sl)))]
         [(*) (multS (parse (second  sl)) (parse (third  sl)))]
         [(-) (cond
                [(equal? (rest (rest s)) '()) (uminusS (parse (second sl)))]
                [else (minusS (parse (second sl)) (parse (third sl)))]
                )]
         [else (error  'parse "invalid list input")]))]
    [else (error  'parse "invalid input")]))

(define (desugar (s ExprS?))
  (type-case ExprS s
    [numS (a) (numC a)]
    [plusS (l r) (plusC (desugar l) (desugar r))]
    [multS (l r) (multC (desugar l) (desugar r))]
    [uminusS (l) (multC (numC -1) (desugar l))]
    [minusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]))

(define (interp (a ExprC?))
  (type-case  ExprC a
    [numC (n) n]
    [plusC (l r) (+ (interp l) (interp r))]
    [multC (l r) (* (interp l) (interp r))]))


(define (interpS s) (interp (desugar (parse s))))