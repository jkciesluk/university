#lang racket
(require racket/contract)

(define first-contract
  (parametric->/c [a b] (-> (-> a b) a b)))

(define second-contract
  (parametric->/c [a b c] (-> (-> a b c)
                              (-> (cons/c a b) c))))

(define third-contract 
  (parametric->/c [a b] (-> (listof (-> a b))
                            (listof a)
                            (listof b))))

(define fourth-contract
  (parametric->/c [a b] (-> (-> b (or/c false/c (cons/c a b)))
                            b
                            (listof a))))

(define fifth-contract
  (parametric->/c [a] (-> (-> a boolean?)
                          (listof a)
                          (cons/c (listof a) (listof a)))))

;; Napisz poniżej implementacje procedur spełniające powyższe kontrakty

; 1
(define/contract (foo1 a->b a)
  (parametric->/c [a b] (-> (-> a b) a b))
  (a->b a))

; 2
(define/contract (foo2 ab->c) 
  (parametric->/c [a b c] (-> (-> a b c) (-> (cons/c a b) c)))
  (lambda (p) (ab->c (car p) (cdr p))))


; 3
(define/contract (foo3 abs as)
 (parametric->/c [a b] (-> (listof (-> a b)) (listof a) (listof b)))
 (map (lambda (f arg) (f arg)) abs as))

; test : (foo3 (list (lambda (a) (+ a 3)) (lambda (a) (+ a 2)) (lambda (a) (+ a 1))) (list 1 2 3))


; 4
(define (foo4 b->cons/f b)
  (parametric->/c [a b] (-> (-> b (or/c  false/c (cons/c a b))) b (listof a)))
  (let ((val (b->cons/f b)))
    (if (false? val)
        null
        (list (car val)))))
        


; 5
(define/contract (foo5 a->b as)
 (parametric->/c [a] (-> (-> a boolean?) (listof a) (cons/c (listof a) (listof a))))
 (define (true-list xs)
   (if (null? xs)
       null
       (if (a->b (car xs))
           (cons (car xs) (true-list (cdr xs)))
           (true-list (cdr xs)))))
 (define (false-list xs)
   (if (null? xs)
       null
       (if (a->b (car xs))
           (false-list (cdr xs))
           (cons (car xs) (false-list (cdr xs))))
       ))
  (cons (true-list as) (false-list as)))
  
; test : (foo5 odd? (list 1 2 3 4))

