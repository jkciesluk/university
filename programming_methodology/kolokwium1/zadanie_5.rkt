#lang racket


;; ZAD 5.

(define (height t)
  (if (leaf? t)
      0
      (+ 1 (max (height (node-left t))
                (height (node-right t))))))

(define (full? t)
  (if (leaf? t)
      true
      (and (full? (node-left t))
           (full? (node-right t))
           (= (height (node-left t))
              (height (node-right t))))))

(define (full?-2.0 t)
  (define (check t n)
    (if (leaf? t)
        (= n 0)
        (and (check (node-left t)  (- n 1))
             (check (node-right t) (- n 1)))))
  (check t (height t)))

; TWIERDZENIE

; Dla dowolnego drzewa t zachodzi:
; (full? t) ≡ (full?-2.0 t) 


; Lemat 1:
; Jeśli (check t n) ≡ true, to (height t) ≡ n

; Lemat ten przyjmujemy bez dowodu.


; Dowód twierdzenia przeprowadzimy indukcyjnie po drzewie t :
; Z definicji (full?-2.0 t) ≡ (check t (height t)) ≡ (full? t)



; 1. t ≡ leaf

; (full? leaf) ≡ (z definicji full dla leaf)
; true ≡ (z definicji check dla 0 i leaf)
; (check leaf 0) ≡ (z definicji height)
; (check leaf (height leaf))

; 2. Dla dowolnego v i drzew t1, t2 
; Jesli zachodzi (full? t1) ≡ (full?-2.0 t1) i (full? t2) ≡ (full?-2.0 t2)
; to zachodzi (full? (node v t1 t2)) ≡ (full?-2.0 (node v t1 t2))


; (full?-2.0 (node v t1 t2)) ≡ (z definicji full?-2.0)
; (check (node v t1 t2) (height (node v t1 t2))) ≡ (z definicji height)
; (check (node v t1 t2) (+ 1 (max (height t1) (height t2)))) ≡ (podstawmy n jako (max (height t1) (height t2))
; (check (node v t1 t2) (+ 1 n)) ≡ (z definicji check, wiemy że n > 0)
; (and (check t1 (-1 (+ 1 n))) (check t2 (-1 (+ 1 n)))) ≡ (z arytmetyki)
; (and (check t1 n) (check t2 n)) ≡ (z lematu 1 i założenia indukcyjnego)
; (and (and (full? t1) (= (height t1) (height t2))) (and (full? t2) (= (height t1) (height t2)))) ≡ (z własności and)
; (and (full? t1) (full? t2) (= (height t1) (height t2))) ≡ (z definicji full)
; (full? (node v t1 t2))

; Co kończy dowód

; Więc dla dowolnego drzewa t zachodzi (full? t)≡(full?-2.0 t)
















