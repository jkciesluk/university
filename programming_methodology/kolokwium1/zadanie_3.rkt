#lang racket

;; ZAD 3.

(define (map f xs)
  (if (null? xs)
      null
      (cons (f (car xs))
            (map f (cdr xs)))))

(define (sum xs)
  (if (null? xs)
      0
      (+ (car xs)
         (sum (cdr xs)))))

(define (map-sum f xs)
  (define (iter xs acc)
    (if (null? xs)
        acc
        (iter (cdr xs)
              (+ (f (car xs)) acc))))
  (iter xs 0))


; TWIERDZENIE
; Dla kazdej procedury f i listy xs zachodzi:
; (map-sum f xs) ≡ (sum (map f xs))

; LEMAT
; Dla kazdej procedury f, liczby acc i listy xs zachodzi:
; (+ (sum (map f xs)) acc) ≡ (iter xs acc)

; Dowód lematu przez indukcje po xs

; 1. xs ≡ null

; (+ (sum (map f null)) acc) ≡ (z definicji map)
; (+ (sum null) acc) ≡ (z definicji sum)
; (+ 0 acc) ≡ (z arytmetyki)
; acc ≡ (z definicji iter dla null)
; (iter null acc)


; 2. Jesli (+ (sum (map f xs)) acc) ≡ (iter xs acc)
;    to (+ (sum (map f (cons x xs))) acc) ≡ (iter (cons x xs) acc)
;    dla dowolnej liczby x

; (+ (sum (map f (cons x xs))) acc) ≡ (z def map)
; (+ (sum (cons (f x) (map f xs))) acc) ≡ (z definicji sum)
; (+ (+ (f x) (sum (map f xs))) acc) ≡ (z łączności dodawania)
; (+ (f x) (+ (sum (map f xs)) acc)) ≡ (z założenia indukcyjnego)
; (+ (f x) (iter xs acc) ≡ (z definicji iter)
; (iter (cons x xs) acc)

; Lemat udowodniony dla dowolnej procedury f, liczby acc i listy xs

; DOWÓD TWIERDZENIA

; (map-sum f xs) ≡ (z definicji map-sum)
; (iter xs 0) ≡ (z lematu)
; (+ (sum (map f xs)) 0) ≡ (z własności dodawania)
; (sum (map f xs))


; Co kończy dowód

; Więc (map-sum f xs) ≡ (sum (map f xs)) dla dowolnej procedury f i listy xs.
























