#lang racket
;; ZAD 4.

;; Definicja drzewa

(define leaf 'leaf)                ; konstruktor liścia

(define (leaf? t)                  ; predykat
  (eq? t 'leaf))

(define (node v l r)               ; konstruktor węzła
  (list 'node v l r))

(define (node? t)                  ; predykat
  (and (list? t)
       (= (length t) 4)
       (eq? (first t) 'node)))

(define (node-val t)               ; selektory
  (second t))

(define (node-left t)
  (third t))

(define (node-right t)
  (fourth t))

(define (tree? t)                  ; predykat definiujący
  (or (leaf? t)                    ; nasz główny typ danych!
      (and (node? t)
           (tree? (node-left  t))
           (tree? (node-right t)))))


(define big-tree
  (node 1 (node 2 (node 4 leaf leaf)
                (node 5 leaf leaf))
        (node 3 (node 6 leaf leaf)
              (node 7 leaf leaf))))

(define (depth xs)
  (define (iter x acc)
    (if (= x 1)
        acc
        (iter (/ x 2) (+ acc 1))))
  (iter (+ (length xs) 1) 0))



(define (full-tree xs)
  (define d (depth xs))
  (define (split s xs i acc)
    (if (<= s i)
        (split s (cdr xs) (+ i 1) (cons (cons (car xs) (car acc))
                                        (cdr acc)))
        acc))
  (define (iter xs s)
    (if (null? xs)
        leaf
        (let* ((l (take xs (- (expt 2 (- d s)) 1)))
              (r (drop xs (- (expt 2 (- d s)) 1)))
              (mid (car r)))
          (node mid
                (iter l (+ s 1)) 
                (iter (cdr r) (+ s 1))))))
    
  (iter xs 1))








