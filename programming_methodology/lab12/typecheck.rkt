#lang plait

; wyrażenia

(define-type ArithExpr
  (const [val : Number])
  (binop [op : Symbol] [l : ArithExpr] [r : ArithExpr])
  (var-expr [id : Symbol])
  (let-expr [id : Symbol] [e1 : ArithExpr] [e2 : ArithExpr])
  (if-expr [eb : ArithExpr] [et : ArithExpr] [ef : ArithExpr]))

(define (binary-operator? op)
  (member op '(+ - * / = < and or)))

(define (parse q)
  (cond
    [(s-exp-number? q) (const (s-exp->number q))]
    [(s-exp-symbol? q) (var-expr (s-exp->symbol q))]
    [(s-exp-list? q)
     (let ([ql (s-exp->list q)])
       (cond
         [(and (= (length ql) 3)
               (equal? (first ql) `let))
          (let ([ll (s-exp->list (second ql))])
            (let-expr (s-exp->symbol (first ll))
                      (parse (second ll))
                      (parse (third ql))))]
         [(and (= (length ql) 4)
               (equal? (first ql) `if))
          (if-expr (parse (second ql))
                   (parse (third ql))
                   (parse (fourth ql)))]
         [(and (= (length ql) 3)
               (binary-operator? (s-exp->symbol (first ql))))
          (binop (s-exp->symbol (first ql))
                 (parse (second ql))
                 (parse (third ql)))]))]))

; środowiska

(define-type-alias (Env 'a) (Listof (Symbol * 'a)))

(env-empty : (Env 'a))
(define env-empty empty)

(define (env-add x v env) (cons (pair x v) env))

(define (env-lookup x env)
  (type-case (Env 'a) env
    [empty (error 'env-lookup (string-append "Unknown identifier "
                                             (to-string x)))]
    [(cons p ps)
     (if (eq? (fst p) x)
         (snd p)
         (env-lookup x (rest env)))]))

; ewaluacja

(define-type Value
  (number-val [v : Number])
  (boolean-val [v : Boolean]))

(define (arith-op f)
  (lambda (x y)
    (number-val (f (number-val-v x) (number-val-v y)))))

(define (comp-op f)
  (lambda (x y)
    (boolean-val (f (number-val-v x) (number-val-v y)))))

(define (bool-op f)
  (lambda (x y)
    (boolean-val (f (boolean-val-v x) (boolean-val-v y)))))

(define (op->proc op)
  (case op
    ['+ (arith-op +)]
    ['- (arith-op -)]
    ['* (arith-op *)]
    ['/ (arith-op /)]
    ['= (comp-op =)]
    ['< (comp-op <)]
    ['and (bool-op (lambda (x y) (and x y)))]
    ['or (bool-op (lambda (x y) (or x y)))]))

(define (eval-env e env)
  (type-case ArithExpr e
    [(const n) (number-val n)]
    [(binop op l r) ((op->proc op) (eval-env l env) (eval-env r env))]
    [(var-expr x) (env-lookup x env)]
    [(let-expr x e1 e2) (eval-env e2 (env-add x (eval-env e1 env) env))]
    [(if-expr eb et ef)
     (if (boolean-val-v (eval-env eb env))
         (eval-env et env)
         (eval-env ef env))]))

(define (eval e) (eval-env e env-empty))

; (eval (parse `(let [x 5] (if (= x 5) 1 0))))






; typechecker
; -----------

; typ typów

(define-type Type
  (number-type)
  (boolean-type)
  )


(define (typecheck-env e env)
  (type-case ArithExpr e
    [(const n) (some (number-type))]
    [(var-expr id) (env-lookup id env)]
    [(binop op el er)
     (let* ([t1 (typecheck-env el env)]
            [t2 (typecheck-env er env)])
       (if (or (none? t1) (none? t2))
           (none)
           (let* ([r1 (some-v t1)]
                  [r2 (some-v t2)])
             (case op
               [(+ - * /) (if (and (number-type? r1)
                                   (number-type? r2))
                              (some (number-type))
                              (none))]
               [(= <) (if (and (number-type? r1)
                               (number-type? r2))
                          (some (boolean-type))
                          (none))]
               [(and or) (if (and (boolean-type? r1)
                                  (boolean-type? r2))
                             (some (boolean-type))
                             (none))]
                ))))]
    [(let-expr id e1 e2)
     (let ([t1 (typecheck-env e1 env)])
       (if (none? t1)
           (none)
           (typecheck-env e2 (env-add id t1 env))))]
    [(if-expr eb et ef) (let ([tb (typecheck-env eb env)])
                          (if (none? tb)
                              (none)
                              (if (boolean-type? (some-v tb))
                                  (let* ([tt (typecheck-env et env)]
                                         [tf (typecheck-env ef env)])
                                    (if (or (none? tt) (none? tf))
                                        (none)
                                        (if (equal? (some-v tt) (some-v tf))
                                            tt
                                            (none))))
                                  (none))))]))
            
 
(define (typecheck e) (typecheck-env e env-empty))


; TESTY

(typecheck (parse `(let [x (< 3 4)] (if x (< 4 3) (< 3 5)))))
; expected :
; (some (boolean-type))

(typecheck (parse `(let [x (< 3 4)] (if x x (< 3 5)))))
; expected :
; (some (boolean-type))

(typecheck (parse `(if (< 5 (let [x (let [y 6] (< 2 y))] (if x 1 0))) 1 2)))
; expected :
; (some (number-type))

(typecheck (parse `(let [x 5] (if (+ x 6) (< 4 3) (< 3 x)))))
; expected :
; (none)

(typecheck (parse `(let [x (< 3 4)] (if (= x 6) (< 4 3) (< 3 x)))))
; expected :
; (none)