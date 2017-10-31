#lang scheme 

(define f (lambda (x) (x 5) (x 8)))

(f (lambda (k)(* k k)))

(+ (call/cc f) 3)

(define (mul a b)
  (begin
    (displayln (list a b))
(* a b)))

(define (lazy*-aux l cont)
  (if (= (first l) 0) 0
      (if (null? (rest l))
          (cont (first l))
          (lazy*-aux (cdr l) (lambda (n) (mul (first l) (cont n)))))))

(define (lazy*-aux2 l cont)
  (call/cc (lambda (exit)
             (if (null? l) (cont 1)
                 (if (list? (first l))
                     (lazy*-aux2 (first l) (lambda (p) (lazy*-aux2 (rest l) (lambda (n) (cont (mul n p))))))
                     (if (= (first l) 0) (exit 0)
                         (lazy*-aux2 (rest l) (lambda (n) (mul (first l) (cont n))))))))))

;(define (lazy*-aux2 l continuacao)
;  (if (number? l)
 ;     (if (= l 0)
 ;         0
 ;         (continuacao l))
 ;     (if (null? (cdr l))
 ;         (lazy*-aux2 (car l) continuacao)
 ;         (lazy*-aux2 (car l) (lambda(p) (lazy*-aux2 (cdr l) (lambda (n) (continuacao (mul p n)))))))))

(define (lazy* l)
  (lazy*-aux2 l (lambda (x) x)))