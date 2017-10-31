#lang plai-typed

(let ([a (list 1 2 3)]
      [b (list 4 5 6)])
     (cons a b))

((lambda (list-a list-b) (cons list-a list-b)) (list 1 2 3) (list 4 5 6))
