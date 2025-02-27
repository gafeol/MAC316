#lang plai

#|
 | Funções não têm mais nome, serão chamadas de lamC (em homenagem ao λ)
 |#

; Expressões básicas
(define-type ExprC
  [numC    (n  number?)]
  [idC     (s  symbol?)]
  [plusC   (l  ExprC?) (r  ExprC?)]
  [multC   (l  ExprC?) (r  ExprC?)]
  [lamC    (arg  symbol?) (body  ExprC?)] ; nomes não são mais necessários
  [appC    (fun  ExprC?) (arg  ExprC?)]
  [ifC     (condição  ExprC?) (sim  ExprC?) (não  ExprC?)]
  [quoteC  (s symbol?)]
  [letrecC (s symbol?) (fun ExprC?) (arg ExprC?)] ; fat lamba (fat 5)
  )

 
; Expressões açucaradas
(define-type ExprS
  [numS    (n  number?)]
  [idS     (s  symbol?)] 
  [lamS    (arg  symbol?) (body  ExprS?)] ; muda de acordo
  [appS    (fun  ExprS?) (arg  ExprS?)] 
  [plusS   (l  ExprS?) (r  ExprS?)]
  [bminusS (l  ExprS?) (r  ExprS?)]
  [uminusS (e  ExprS?)]
  [multS   (l  ExprS?) (r  ExprS?)]
  [ifS     (c  ExprS?) (s  ExprS?) (n  ExprS?)]
  [quoteS  (s symbol?)]
  [letS    (arg list?) (body ExprS?)]
  [let*S   (arg list?) (body ExprS?)]
  [letrecS (fun list?) (body ExprS?)] 
  )


; Retirando o açúcar
(define (desugar as); ExprS => ExprC  
  (type-case ExprS as
    [numS    (n) (numC n)]
    [idS     (s) (idC s)]
    [lamS    (a b)  (lamC a (desugar b))]
    [appS    (fun arg) (appC (desugar fun) (desugar arg))] 
    [plusS   (l r) (plusC (desugar l) (desugar r))] 
    [multS   (l r) (multC (desugar l) (desugar r))]
    [bminusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]
    [uminusS (e)   (multC (numC -1) (desugar e))]
    [ifS     (c s n) (ifC (desugar c) (desugar s) (desugar n))]
    [quoteS  (s) (quoteC s)]
    [letS    (arg body) (appC (lamC (first arg) (desugar body)) (desugar (second arg)))]
    [let*S   (arg body) (appC (lamC (first (first arg))
                                    (appC (lamC (first (second arg)) (desugar body)) (desugar (second (second arg)))))
                              (desugar (second (first arg))))]
    [letrecS (fun arg) (letrecC (first fun) (desugar (second fun)) (desugar arg))]
                                    
    ))


#|
 | Closures não têm mais nome, mas precisam de Environment
 |#

; Símbolos devem se associar a um Value
(define-type Binding
      [bind (name  symbol?) (val  Value?)])

; A lista de associações é o Environment
; (define-type-alias Env (listof Binding))
(define mt-env empty)    ; Tente pronunciar "mt" em inglês e compare com "empty"
(define extend-env cons) ; Por sorte, cons faz exatamente o que queremos para estender o env
(define-type Value
  [numV  (n  number?)]
  [closV (arg  symbol?) (body  ExprC?) (env  list?)]
  [symbolV (s symbol?)])

; Novos operadores
(define (num+ l r); Value x Value => Value
    (cond
        [(and (numV? l) (numV? r))
             (numV (+ (numV-n l) (numV-n r)))]
        [else
             (error 'num+ "Um dos argumentos não é número")]))

(define (num* l r); Value x Value => Value
    (cond
        [(and (numV? l) (numV? r))
             (numV (* (numV-n l) (numV-n r)))]
        [else
             (error 'num* "Um dos argumentos não é número")]))

; Interpretador
(define (interp a env); ExprC x Env => Value
  (type-case ExprC a
    [numC (n) (numV n)] 
    [idC (n) (lookup n env)]
    [lamC (a b) (closV a b env)] ; definição de função captura o environment

    [appC (f a)
          (local ([define f-value (interp f env)]) ; f-value descreve melhor a ideia
            (interp (closV-body f-value)
                    (extend-env 
                        (bind (closV-arg f-value) (interp a env))
                        (closV-env f-value) ; não mais mt-env
                    )))]
    
    [plusC (l r) (num+ (interp l env) (interp r env))]
    [multC (l r) (num* (interp l env) (interp r env))]
    [ifC (c s n) (if (zero? (numV-n (interp c env))) (interp n env) (interp s env))]
    [quoteC (s) (symbolV s)]
    [letrecC (s f a)
             (let ((func (interp f env)))
               (begin
                 (set-closV-env! func (extend-env (bind s func) env))
                 (interp a (closV-env func))
                 ))]
    ))

; Lookup para procurar símbolos no Environment
(define (lookup for env); [for : symbol] [env : Env]) => Value
       (cond
            [(empty? env) (error 'lookup (string-append (symbol->string for) " não foi encontrado"))] ; livre (não definida)
            [else (cond
                  [(symbol=? for (bind-name (first env)))   ; achou!
                                 (bind-val (first env))]
                  [else (lookup for (rest env))])]))        ; vê no resto

(define (readFile s)
  (when (not (empty? s))
    (begin (println (interpS (first s)))
           (readFile (rest s)))))

; Parser
(define (parse s); [s : s-expression] => ExprS
  (cond
    [(number? s) (numS s)]
    [(symbol? s) (idS  s)] ; Pode ser um símbolo livre nas definições de função
    [(list? s)
     (let ([sl s])
       (case  (first sl)
         [(+) (plusS (parse (second sl)) (parse (third sl)))]
         [(*) (multS (parse (second sl)) (parse (third sl)))]
         [(-) (bminusS (parse (second sl)) (parse (third sl)))]
         [(~) (uminusS (parse (second sl)))]
         [(func) (lamS (second sl) (parse (third sl)))]
         [(lambda) (lamS (second sl) (parse (third sl)))]
         [(call) (appS (parse (second sl)) (parse (third sl)))]
         [(if) (ifS (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
         [(quote) (quoteS (second sl))]
         [(let) (letS (list (first (first (second sl))) (parse (second (first (second sl))))) (parse (third sl)))]
         [(let*) (let*S (list (list (first (first (second sl))) (parse (second (first (second sl)))))
                              (list (first (second (second sl))) (parse (second (second (second sl))))))
                        (parse (third sl)))]
         [(letrec) (letrecS (list (first (first (second sl))) (parse (second (first (second sl))))) (parse (third sl)))]
         [(load) (readFile (let ((file (open-input-file (symbol->string (symbolV-s (interpS (second sl)))))))
                   (let f ((x (read file)))
                     (if (eof-object? x)
                         (begin
                           (close-input-port file)
                           '())
                         (cons x (f (read file)))))))]
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))


; Facilitador
(define (interpS s) (interp (desugar (parse s)) mt-env))

; Testes
(test (interp (plusC (numC 10) (appC (lamC '_ (numC 5)) (numC 10)))
              mt-env)
      (numV 15))

(test (interpS '(let ((x (* 2 3))) (+ x (+ x 7))))
      (numV 19))


(test (interpS '(let* ([x (* 2 3)] [y (+ 2 x)]) (- y x)))
      (numV 2))

(test (interpS '(+ 10 (call (func x (+ x x)) 16)))
      (numV 42))

(test (interpS '(letrec ([fac (lambda n (if n (* n (call fac (- n 1))) 1))]) (call fac 5)))
      (numV 120))

(test (interpS '(let ([x 2])
            (let ([y 3])
              (let ([foo (lambda z (+ x (+ y z)))])
                (let ([x 7])
                  (letrec ([bar (lambda k (if k (* 2 (call bar (- k 1))) 1))])
                    (call bar (call foo 4))))))))
      (numV 512))