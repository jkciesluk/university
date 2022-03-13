#lang racket

(provide cube-root)

(define (square x) (* x x))
(define (cube x) (* x x x))

(define (abs x)
  (cond [(< x 0) (- x)]
        [else x]))

(define (cube-root x)
  (define (improve guess)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))

  (define (cube-root-iter guess)
    (if (good-enough? guess)
        guess
        (cube-root-iter (improve guess))))
  (define (good-enough? guess)
    (< (abs (- (cube guess) x))
       0.0001))
  (cube-root-iter 1.0))

(cube-root 27)
(cube-root -64)
(cube-root 1024.045)
  