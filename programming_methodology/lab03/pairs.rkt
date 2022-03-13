#lang racket

(provide null null? cons pair? car cdr)

(define null '() )

(define (null? x)
  (if (eq? x null)
      #t
      #f))
     
  

(define (cons x y)
  (lambda (a)
    (if (= a 1)
        x
        y)))

(define (pair? x)
  (if (and (procedure? x) (not (null? x)))
      #t
      #f))

(define (car x) (x 1) )

(define (cdr x) (x 2) )

(define (list-sum xs)
  (if (null? xs)
      0
      (+ (car xs) (list-sum (cdr xs)))))

(define (list-max xs)
  (define (max a b)
    (if (> a b)
        a
        b))
  (if (null? xs)
      0
      (max (car xs) (list-max (cdr xs)))))
  
(pair? (cons 3 4))
; expected: #t
(pair? null)
; expected: #f
(null? null)
; expected: #t
(null? (cons 5 6))
; expected: #f
(car (cons 3 4))
;expected: 3
(cdr (cons 3 4))
;expected: 4

(list-sum (cons 1 (cons 2 (cons 3 null))))
; expected: 6

(list-max (cons 5 (cons 2 (cons 1 (cons 3 null)))))
; expected: 5