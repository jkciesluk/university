#lang racket

; --------- ;
; Wyrazenia ;
; --------- ;

(provide (struct-out const) (struct-out binop) (struct-out var-expr) (struct-out let-expr) (struct-out var-dead) find-dead-vars)

(struct const    (val)      #:transparent)
(struct binop    (op l r)   #:transparent)
(struct var-expr (id)       #:transparent)
(struct var-dead (id)       #:transparent)
(struct let-expr (id e1 e2) #:transparent)

(define (expr? e)
  (match e
    [(const n) (number? n)]
    [(binop op l r) (and (symbol? op) (expr? l) (expr? r))]
    [(var-expr x) (symbol? x)]
    [(var-dead x) (symbol? x)]
    [(let-expr x e1 e2) (and (symbol? x) (expr? e1) (expr? e2))]
    [_ false]))

(define (parse q)
  (cond
    [(number? q) (const q)]
    [(symbol? q) (var-expr q)]
    [(and (list? q) (eq? (length q) 3) (eq? (first q) 'let))
     (let-expr (first (second q))
               (parse (second (second q)))
               (parse (third q)))]
    [(and (list? q) (eq? (length q) 3) (symbol? (first q)))
     (binop (first q)
            (parse (second q))
            (parse (third q)))]))

; ---------------------------------- ;
; Wyszukaj ostatnie uzycie zmiennych ;
; ---------------------------------- ;

(define (find-dead-vars e)
  (define (parse x env)
   (match x
    [(const q) (cons (const q) env)]
    [(var-expr q) (if (set-member? env q)
                     (cons (var-expr q) env)
                     (cons (var-dead q) (set-add env q)))]
  
    [(let-expr sym e1 e2)
       (let ((ev2 (parse e2 (set-remove env sym))))
         (let ((ev1 (if (set-member? env sym)
                        (parse e1 (set-add (cdr ev2) sym))
                        (parse e1 (set-remove (cdr ev2) sym)))))
           (cons (let-expr sym (car ev1) (car ev2)) (cdr ev1))))]                     
    [(binop op l r)
     (let ((er (parse r env)))
       (let ((el (parse l (cdr er))))
         (cons  (binop op
                       (car el)
                       (car er))
                       (cdr el))))]))
    (car (parse e (set))))


; TESTY

(find-dead-vars (let-expr 'x (const  3)(binop '+ (var-expr 'x) (var-expr 'x))))
#| expected :
(let-expr 'x (const 3) (binop '+ (var-expr 'x) (var-dead 'x)))
|#

(find-dead-vars (let-expr 'x (const  3) (binop '+ (var-expr 'x) (let-expr 'x (const  5) (binop '+ (var-expr 'x) (var-expr 'x))))))
#| expected :
(let-expr
 'x
 (const 3)
 (binop
  '+
  (var-dead 'x)
  (let-expr 'x (const 5) (binop '+ (var-expr 'x) (var-dead 'x)))))
|#

(find-dead-vars (let-expr 'y (const 45)
                          (binop '+ (binop '- (const 8)
                                           (var-expr 'y))
                                 (let-expr 'x (const 3)
                                           (let-expr 'z (binop '+ (var-expr 'x)
                                                               (var-expr 'y))
                                                     (binop '+ (var-expr 'z)
                                                            (var-expr 'x)))))))
#| expected :
(let-expr
 'y
 (const 45)
 (binop
  '+
  (binop '- (const 8) (var-expr 'y))
  (let-expr
   'x
   (const 3)
   (let-expr
    'z
    (binop '+ (var-expr 'x) (var-dead 'y))
    (binop '+ (var-dead 'z) (var-dead 'x))))))
|#
