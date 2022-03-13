#lang racket

(require racket/contract)

(provide
 (contract-out
  [with-labels with-labels/c]
  [foldr-map foldr-map/c]
  [pair-from pair-from/c]))

(provide with-labels/c foldr-map/c pair-from/c)


; WITH-LABELS

(define with-labels/c
  (parametric->/c [a b] (-> (-> a b) (listof a) (listof (cons/c b (listof a)))))
  )
  
(define (with-labels f xs)
  (if (null? xs)
      null
      (let ((a (car xs)))
        (cons (list (f a) a) (with-labels f (cdr xs))))))

; FOLDR-MAP

(define foldr-map/c
  (parametric->/c [a b c] (-> (-> a c (cons/c b c)) c (listof a) (cons/c (listof b) c)))
  )

(define (foldr-map f a xs)
  (define (it a xs ys)
    (if (null? xs)
        (cons ys a)
        (let [(p (f (car xs) a))]
          (it (cdr p) (cdr xs) (cons (car p) ys)))))
  (it a (reverse xs) null))


; PAIR-FROM

(define pair-from/c
  (parametric->/c [a b c] (-> (-> a c) (-> a b) (-> a (cons/c c b)))) 
  )
(define (pair-from f g)
  (lambda (x)
    (cons (f x) (g x))))


; TESTY

(with-labels number->string '(1 2 3 4))
(foldr-map (lambda(x a) (cons a (+ a x))) 0'(1 2 3))
(( pair-from (lambda(x) (+ x 1)) (lambda(x) (* x 2))) 2)








