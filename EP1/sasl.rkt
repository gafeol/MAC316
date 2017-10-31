#lang scheme

(define interval (lambda (st end)
                   (if (< end st) '()
                       (cons st (interval  (+ 1 st) end)))))
(define fun-search (lambda (n)
                     (find-val pred (interval 1 n) (+ 1 n))))
(define find-val (lambda (pred lista valor-falha)
                   (if (null? lista)
                       valor-falha
                       (if (pred (car lista))
                           (car lista)
                           (find-val pred (cdr lista) valor-falha)))))