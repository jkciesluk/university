#lang racket

(provide integral)

(define (sum val next a b)
  (if (> a b)
      0
      (+ (val a) (sum val next (next a) b))))

(define (integral f prec)
  (lambda (a b)
    (define dx (/ (- b a) prec))
    (define (value a) (* (f a) dx))
    (define (inc a) (+ a dx))
    (sum value inc a (- b dx))))

(define foo (integral (lambda (x) 10)  1000))
(foo 0.0 10)
(define foo2 (integral (lambda (x) x) 1000))
(foo2 9.0 10)
(foo2 0.0 10)
(define foo3 (integral sin 1000))
(foo3 0.0 (* pi 2))
(foo3 0.0 (/ pi 2))
((integral tan 1000) (/ pi -2) (/ pi 2)) 

