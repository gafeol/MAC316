#lang scheme

;EXC 1

(define (sigma m n)
  (if (> m n)
      0
      (+ m (sigma (+ 1 m) n))))

;(define sigma
;  (lambda (m n)
;    (if (> m n)
;        0
;        (+ m (sigma (+ 1 m) n)))))

;(letrec ((sigma (lambda (m n) (if (> m n) 0 (+ m (sigma (+ 1 m) n)))))) (sigma 2 4))

(define fac
  (lambda (n)
    (if (= n 0) 1 (* n (fac (- n 1))))))

;EXC 2

(define (exp m n)
  (if (= n 0)
      1
      (* m (exp m (- n 1)))))

(define (log-aux m n p)
  (if (> (exp m (+ 1 p)) n)
      p
      (log-aux m n (+ 1 p))))

(define (log m n)
  (log-aux m n 0))

;EXC 3

(define (choose n k)
  (if (= n k)
      1
      (if (= k 0)
          1
          (+ (choose (- n 1) (- k 1)) (choose (- n 1) k)))))

;EXC 4

(define (fib n)
  (if (or (= n 0) (= n 1)) 1
      (+ (fib (- n 2)) (fib (- n 1)))))

;EXC 2.1.a

(define (count x l)
  (if (null? l)
      0
      (if (equal? (first l) x)
          (+ 1 (count x (rest l)))
          (count x (rest l)))))

(define (is x val)
  (if (equal? x val)
      1
      0))

; ESTUDAR ESSE BAGS
(define (countall x l)
  (if (null? l)
      0
      (if (list? l)
          (+ (countall x (first l)) (countall x (rest l)))
          (is x l)
          )))

;EXC 2.1.b

(define (reverse l)
  (if (null? l) '()
      (append (reverse (rest l)) (list (first l)))))

;EXC 2.1.c

(define (twist l)
  (if (null? l)
      '()
      (append (twist (rest l))
            (if (list? (first l))
                (list (twist (first l)))
                (list (first l))))))

;EXC 2.1.d

(define (flatten l)
  (if (null? l) '()
      (if (list? l)
          (append (flatten (first l)) (flatten (rest l)))
          (list l))
      ))

;(define (flatten l)
;  (if (null? l) '()
;      (if (list? (car l))
;          (append (flatten (first l)) (flatten (rest l)))
;          (append (list (first l)) (flatten (rest l))))
;      ))

;EXC 2.1.e

(define (sublist l1 l2)
  (if (null? l1)
      #t
      (if (null? l2)
          #f
          (if (equal? (first l1) (first l2))
              (sublist (rest l1) (rest l2))
              (sublist l1 (rest l2))))))

(define (is-sub l1 l2)
  (if (null? l1)
      #t
      (if (null? l2)
          #f
          (if (equal? (first l1) (first l2))
              (is-sub (rest l1) (rest l2))
              #f))))

(define (contig-sublist l1 l2)
  (if (null? l1)
      #t
      (if (null? l2)
          #f
          (if (is-sub l1 l2)
              #t
              (contig-sublist l1 (rest l2))))))

;EXC 4.1.a

(define curry
(lambda (f) (lambda (a) (lambda (b) (f a b)))))

(define mapcar (lambda (fun l)
  (if (null? l) null
  (cons (fun (car l))
  (mapcar fun (cdr l)))))
)

(define mapc (curry mapcar))

(define quad (lambda (x) (* x x)))


(define quad*
  (mapc quad)
)

(define id (lambda (x) x))

(define (combine f fu zero)
  (lambda (l)
    (if (null? l) zero
        (f (fu (first l)) ((combine f fu zero) (rest l))))))

(define sumsq (combine + quad 0))