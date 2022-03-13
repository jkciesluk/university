#lang racket

(provide (struct-out const) (struct-out binop) rpn->arith)

;; -------------------------------
;; Wyrazenia w odwr. not. polskiej
;; -------------------------------

(define (rpn-expr? e)
  (and (list? e)
       (pair? e)
       (andmap (lambda (x) (or (number? x) (member x '(+ - * /))))
               e)))

;; ----------------------
;; Wyrazenia arytmetyczne
;; ----------------------

(struct const (val)    #:transparent)
(struct binop (op l r) #:transparent)

(define (arith-expr? e)
  (match e
    [(const n) (number? n)]
    [(binop op l r)
     (and (symbol? op) (arith-expr? l) (arith-expr? r))]
    [_ false]))

(define (operator? op)
  (member op '(+ - * /)))



;; ----------
;; Kompilacja
;; ----------

(define (to-take xs i acc)
  (if (= i 0)
      acc
      (if (number? (car xs))
          (to-take (cdr xs) (- i 1) acc)
          (to-take (cdr xs) (+ i 1) (+ acc 2)))))


(define (rpn->arith e)
  (define (iter stack )
    (cond
      [(number? (car stack)) (const (car stack))]
      [(operator? (car stack))
       (let  ((s (to-take (cdr stack) 1 1)))
        (binop (car stack) (iter (drop (cdr stack) s)) (iter (cdr stack))))]
      [else null]))
  (iter (reverse e)))


      
; Mozesz tez dodac jakies procedury pomocnicze i testy

; TESTY

(define (arith->rpn e)
  (match e
    [(const n)
     (list n)]
    [(binop op el er)
     (let ((pr (arith->rpn el))
           (pl (arith->rpn er)))
       (append pr pl (list op)))]))


(rpn->arith '(1 2 + 7 / 1 2 - 5 * + 3 /))
; expected :
;(binop
; '/
; (binop
;  '+
;  (binop '/ (binop '+ (const 1) (const 2)) (const 7))
;  (binop '* (binop '- (const 1) (const 2)) (const 5)))
; (const 3))

(rpn->arith '(4 2 - 5 7 / + 12 +))
; expected :
;(binop
; '+
; (binop '+ (binop '- (const 4) (const 2)) (binop '/ (const 5) (const 7)))
; (const 12))

(rpn->arith '(1 2 3 4 5 + + + +))
; expected :
;(binop
; '+
; (const 1)
; (binop '+ (const 2) (binop '+ (const 3) (binop '+ (const 4) (const 5)))))


(arith->rpn (rpn->arith '(2 1 - 3 + 4 - 1 2 3 * + -)))
; expected :
; '(2 1 - 3 + 4 - 1 2 3 * + -)
