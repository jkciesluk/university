#lang racket

;; ---------------
;; Jezyk wejsciowy
;; ---------------
(provide (struct-out const) (struct-out binop) (struct-out var-expr) (struct-out let-expr) (struct-out pos) (struct-out var-free) (struct-out var-bound) annotate-expression)
(struct pos (file line col)     #:transparent)
  
(struct const    (val)          #:transparent)
(struct binop    (op l r)       #:transparent)
(struct var-expr (id)           #:transparent)
(struct let-expr (loc id e1 e2) #:transparent)

(define (expr? e)
  (match e
    [(const n)      (number? n)]
    [(binop op l r) (and (symbol? op) (expr? l) (expr? r))]
    [(var-expr x)   (symbol? x)]
    [(let-expr loc x e1 e2)
     (and (pos? loc) (symbol? x) (expr? e1) (expr? e2))]
    [_ false]))

(define (make-pos s)
  (pos (syntax-source s)
       (syntax-line   s)
       (syntax-column s)))

(define (parse e)
  (let ([r (syntax-e e)])
    (cond
      [(number? r) (const r)]
      [(symbol? r) (var-expr r)]
      [(and (list? r) (= 3 (length r)))
       (match (syntax-e (car r))
         ['let (let* ([e-def (syntax-e (second r))]
                      [x     (syntax-e (first e-def))])
                 (let-expr (make-pos (first e-def))
                           (if (symbol? x) x (error "parse error!"))
                           (parse (second e-def))
                           (parse (third r))))]
         [op   (binop op (parse (second r)) (parse (third r)))])]
      [else (error "parse error!")])))

;; ---------------
;; Jezyk wyjsciowy
;; ---------------

(struct var-free  (id)     #:transparent)
(struct var-bound (pos id) #:transparent)

(define (expr-annot? e)
  (match e
    [(const n)         (number? n)]
    [(binop op l r)    (and (symbol? op) (expr-annot? l) (expr-annot? r))]
    [(var-free x)      (symbol? x)]
    [(var-bound loc x) (and (pos? loc) (symbol? x))]
    [(let-expr loc x e1 e2)
     (and (pos? loc) (symbol? x) (expr-annot? e1) (expr-annot? e2))]
    [_ false]))


(define env-empty null)
(define (env-add loc x env)
  (cons (cons loc x) env))
(define (env-lookup x env)
  (if (null? env)
      #f
      (if (eq? (cdr (first env)) x)
          (car (first env))
          (env-lookup x (cdr env)))))

(define (bound-vars-env e env)
  (match e
    [(const n) (const n)]
    [(binop op l r)
     (binop  op (bound-vars-env l env)
                (bound-vars-env r env))]
    [(let-expr loc x e1 e2)
     (let-expr loc x (bound-vars-env e1 env)
             (bound-vars-env e2 (env-add loc x env)))]
    [(var-expr x)
     (let ((n (env-lookup x env)))
       (if (eq? n false)
           (var-free x)
           (var-bound n x)))]))
     



(define (annotate-expression e)
  (bound-vars-env e env-empty))


; TESTY 

(annotate-expression (parse #'(let [x 3] (+ (let [x (- 12 x)] (+ x x)) (let [x 5] (+ x (+ x y)))))))
#| expected : 
 (let-expr
 (pos #<path:/solution.rkt> 93 36)
 'x
 (const 3)
 (binop
  '+
  (let-expr
   (pos #<path:/solution.rkt> 93 50)
   'x
   (binop
    '-
    (const 12)
    (var-bound (pos #<path:/solution.rkt> 93 36) 'x))
   (binop
    '+
    (var-bound (pos #<path:/solution.rkt> 93 50) 'x)
    (var-bound
     (pos #<path:/solution.rkt> 93 50)
     'x)))
  (let-expr
   (pos #<path:/solution.rkt> 93 77)
   'x
   (const 5)
   (binop
    '+
    (var-bound (pos #<path:/solution.rkt> 93 77) 'x)
    (binop
     '+
     (var-bound (pos #<path:/solution.rkt> 93 77) 'x)
     (var-free 'y))))))
|#

(annotate-expression (parse #'(let [x 5] (* y x))))
#| expected : 
 (let-expr
 (pos #<path:/solution.rkt> 130 36)
 'x
 (const 5)
 (binop
  '*
  (var-free 'y)
  (var-bound (pos #<path:/solution.rkt> 130 36) 'x)))
|#

