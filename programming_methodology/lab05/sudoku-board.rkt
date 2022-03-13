#lang racket




(require "sudoku-spec.rkt")

(provide
 rows->board board-rows board-columns board-boxes ; Zad A
 )                 ; Zad B

;; =========
;; Zadanie A
;; =========

;; Tworzy planszę z wierszy (listy list znaków z alfabetu)
(define (rows->board rows)
  (define (rows->board-iter board)
    (if (null? board)
      null
      (append (car board) (rows->board-iter (cdr board)))))
  (cons (length rows) (rows->board-iter rows)))

;; Ujawnia listę wierszy (każdy wiersz to lista znaków z alfabetu)
(define (board-rows board)
  (define n2 (car board))
  (define (last-iter board)
    (if (null? board)
        null
        (cons (take board n2) (last-iter (drop board n2)))))
  (last-iter (cdr board)))

;; Ujawnia listę kolumn (każda kolumna to lista znaków z alfabetu)
(define (board-columns board)
  (define n2 (car board))
  (define (iter i board acc)
    (if (= i n2)
        (reverse (cons (car board) acc))
        (iter (+ i 1) (drop board n2) (cons (car board) acc))))
  (define (last-iter x board)
    (if (= x n2)
        null
        (cons (iter 1 board null) (last-iter (+ x 1) (cdr board)))))
  (last-iter 0  (cdr board)))

;; Ujawnia listę pudełek (każde pudełko to lista znaków z alfabetu)
(define (board-boxes board)
  (define n (car board))
  (define n2 (sqrt n))
  (define (single-iter x board)
    (if (= x n2)
        (take board n2)
        (let ((b (drop board n)))
          (append (take board n2) (single-iter (+ 1 x) b)))))
  (define (multi-iter x board)
    (if (= x n)
        null
        (if (= (modulo x n2) (- n2 1))
            (cons (single-iter 1 board) (multi-iter (+ x 1) (drop board (+ (* n (- n2 1)) n2))))    
            (cons (single-iter 1 board) (multi-iter (+ x 1) (drop board n2))))))
  (multi-iter 0 (cdr board)))
 


; TESTY

   

(define small '((_ _ 4 _)
                (_ 3 _ _)
                (1 2 _ 4)
                (_ 4 _ _)))

(define big '((5 3 _ _ 7 _ _ _ _)
              (6 _ _ 1 9 5 _ _ _)
              (_ 9 8 _ _ _ _ 6 _)
              (8 _ _ _ 6 _ _ _ 3)
              (4 _ _ 8 _ 3 _ _ 1)
              (7 _ _ _ 2 _ _ _ 6)
              (_ 6 _ _ _ _ 2 8 _)
              (_ _ _ 4 1 9 _ _ 5)
              (_ _ _ _ 8 _ _ 7 9)))

(define b (rows->board big))
(define s (rows->board small))

(board-boxes b)
;expected :
;'((5 3 _ 6 _ _ _ 9 8) (_ 7 _ 1 9 5 _ _ _) (_ _ _ _ _ _ _ 6 _) (8 _ _ 4 _ _ 7 _ _) (_ 6 _ 8 _ 3 _ 2 _) (_ _ 3 _ _ 1 _ _ 6) (_ 6 _ _ _ _ _ _ _) (_ _ _ 4 1 9 _ 8 _) (2 8 _ _ _ 5 _ 7 9))

(board-boxes s)
;expected :
;'((_ _ _ 3) (4 _ _ _) (1 2 _ 4) (_ 4 _ _))

(board-rows b)
;expected :
;'((5 3 _ _ 7 _ _ _ _) (6 _ _ 1 9 5 _ _ _) (_ 9 8 _ _ _ _ 6 _) (8 _ _ _ 6 _ _ _ 3) (4 _ _ 8 _ 3 _ _ 1) (7 _ _ _ 2 _ _ _ 6) (_ 6 _ _ _ _ 2 8 _) (_ _ _ 4 1 9 _ _ 5) (_ _ _ _ 8 _ _ 7 9))

(board-columns b)
;expected :
;'((5 6 _ 8 4 7 _ _ _) (3 _ 9 _ _ _ 6 _ _) (_ _ 8 _ _ _ _ _ _) (_ 1 _ _ 8 _ _ 4 _) (7 9 _ 6 _ 2 _ 1 8) (_ 5 _ _ 3 _ _ 9 _) (_ _ _ _ _ _ 2 _ _) (_ _ 6 _ _ _ 8 _ 7) (_ _ _ 3 1 6 _ 5 9))

(board-rows s)
;expected : 
;'((_ _ 4 _) (_ 3 _ _) (1 2 _ 4) (_ 4 _ _))

(board-columns s)
;expected :
;'((_ _ 1 _) (_ 3 2 4) (4 _ _ _) (_ _ 4 _))
