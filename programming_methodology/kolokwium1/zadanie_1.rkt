#lang racket

;; ZAD 1.

(define (kwadraty-liczb-parzystych xs)
  (map (lambda (x) (* x x))
       (filter (λ (x) (= (modulo x 2) 0)) xs)))

(define (aplikuj-procedury fs x)
  (define df (reverse fs))
  (define (iter sf x)
     (if (null? sf)
       x
       (aplikuj-procedury (cdr sf) ((car sf) x))))
  (iter df x))


(define (wstaw-pomiedzy x xs)
  (foldr (λ (h t) (cons x (cons h t))) (cons x null) xs))

(define (grupuj xs)
  5)


