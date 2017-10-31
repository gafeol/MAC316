#lang plai-typed

(define-type  ArithC
  [numC   (n : number)]
  [plusC (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)])

(define-type  ArithS
  [numS     (n : number)]
  [plusS    (l : ArithS) (r : ArithS)]
  [bminusS (l : ArithS) (r : ArithS)]
  [multS    (l : ArithS) (r : ArithS)])

(define (interp [a : ArithC ]) : number
  (type-case  ArithC a
    [numC (n) n]
    [plusC (l r) (+ (interp l) (interp r))]
    [multC (l r) (* (interp l) (interp r))]))

(define (desugar [as : ArithS ]) : ArithC   ; recebe  ArithS e devolve
  (type-case  ArithS  as
    [numS     (n)    (numC n)]                 ; convers~ao direta
    [plusS    (l r) (plusC (desugar l)      ; todas  as sub ́arvores  devem
                           (desugar r))]     ; o a acucar  retirado
    [multS    (l r) (multC (desugar l)
                           (desugar r))]
    [bminusS (l r) (plusC (desugar l)      ; aqui feita a transformacao
                          (multC (numC  -1) (desugar r)))]
    ))

(define (parse [s : s-expression ]) : ArithS
  (cond
    [( s-exp-number? s) (numS (s-exp->number s))]
    [( s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first  sl))
         [(+) (plusS (parse (second  sl)) (parse (third  sl)))]
         [(*) (multS (parse (second  sl)) (parse (third  sl)))]
         ; agora  temos o  ’-’
         [(-) (bminusS (parse (second  sl)) (parse (third  sl)))]
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))