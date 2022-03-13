#lang racket


;; ZAD 2.

(define leaf 'leaf)

(define (leaf? l)
  (eq? l 'leaf))

(define (node-2 v l r)
  (list 'node-2 v l r))

(define (node-2-val t)
  (second t))

(define (node-2-left t)
  (third t))

(define (node-2-right t)
  (fourth t))

(define (node-2? t)
  (and (list? t)
       (= (length t) 4)
       (eq? (first t) 'node-2)))

(define (node-3 v p d t)
  (list 'node-3 v p d t))

(define (node-3-val t)
  (second t))

(define (node-3-first t)
  (third t))

(define (node-3-second t)
  (fourth t))

(define (node-3-third t)
  (fifth t))

(define (node-3? t)
  (and (list? t)
       (= (length t) 5)
       (eq? (first t) 'node-3)))



(define (tree? t)
  (or (leaf? t)
      (and (node-2? t)
           (tree? (node-2-left t))
           (tree? (node-2-right t)))
      (and (node-3? t)
           (tree? (node-3-first t))
           (tree? (node-3-second t))
           (tree? (node-3-third t)))))

(define (sciezka t)
  (define (iter t acc)
    (if (leaf? t)
        (list acc)
        (if (node-2? t)
            (append (iter (node-2-left t) (cons (node-2-val t) acc))
                  (iter (node-2-right t) (cons (node-2-val t) acc)))
            (append (iter (node-3-first t) (cons (node-3-val t) acc))
                  (iter (node-3-second t) (cons (node-3-val t) acc))
                  (iter (node-3-third t) (cons (node-3-val t) acc))))))
  (iter t null))
            


(define big-tree                              ;przykladowe drzewo
  (node-2 1 (node-2 2 (node-2 4 leaf leaf)
                  (node-2 5 leaf leaf))
          (node-3 3 (node-2 6 leaf leaf)
                  (node-2 7 leaf leaf)
                  leaf) ))




      

             