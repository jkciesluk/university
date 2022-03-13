#lang racket

(define binops '(+ - * / = > <))
(define reserved
  (append binops '(if let fun true false)))

;; Predykaty rozpoznające nazwy zmiennych i funkcji
;; Nazwy zmiennych zaczynają się małą literą, nazwy funkcji — wielką

(define (name-var? x)
  (and (symbol? x)
       (not (member x reserved))
       (char-lower-case? (string-ref (symbol->string x) 0))))

(define (name-fun? f)
  (and (symbol? f)
       (char-upper-case? (string-ref (symbol->string f) 0))))

;; Składnia abstrakcyjna

(struct const (val) #:transparent)
(struct binop (op left right) #:transparent)
(struct let-expr (var bound body) #:transparent)
(struct var-expr (name) #:transparent)
(struct call (fun args) #:transparent)
(struct if-expr (cond true false) #:transparent)

(struct fun (name params body) #:transparent)

(struct prog (funs expr) #:transparent)

(define (expr? e)
  (match e
    [(const n)
     (or (number? n) (boolean? n))]
    [(binop op el er)
     (and (member op binops)
          (expr? el)
          (expr? er))]
    [(var-expr x)
     (name-var? x)]
    [(let-expr x e1 e2)
     (and (name-var? x)
          (expr? e1)
          (expr? e2))]
    [(if-expr eb et ef)
     (and (expr? eb)
          (expr? et)
          (expr? ef))]
    [(call f es)
     (and (name-fun? f)
          (andmap expr? es))]
    [_ false]))

(define (function? p)
  (match p
    [(fun f xs e)
     (and (name-fun? f)
          (andmap name-var? xs)
          (expr? e))]
    [_ false]))

(define (program? p)
  (match p
    [(prog ps e)
     (and (andmap function? ps)
          (expr? e))]
    [_ false]))

;; Przykłady

(define simple
  '(() ; bez funkcji
    (let (x 4)
      (+ (* x x) 26))))

(define fact
  '(((fun Fact (n)
          (if (= n 0) 1 (* n (Fact (- n 1))))))
    (Fact 5)))

(define odd-even
  '(((fun Odd  (n)
          (if (= n 0) false (Even (- n 1))))
     (fun Even (n)
          (if (= n 0) true  (Odd  (- n 1)))))
    (Odd 33)))


;;============================= Zadanie 1 - Parser =============================

(define (parse-expr e)
  (cond
   [(number? e)   (const e)]
   [(eq? e 'true)  (const true)]
   [(eq? e 'false) (const false)]
   [(name-var? e) (var-expr e)]
   [(and (list? e)
         (= 3 (length e))
         (eq? 'let (car e))
         (list? (cadr e))
         (= 2 (length (cadr e)))
         (name-var? (caadr e)))
    (let-expr (caadr e)
              (parse-expr (cadadr e))
              (parse-expr (caddr e)))]
   [(and (list? e)
         (= 4 (length e))
         (eq? 'if (car e)))
    (if-expr (parse-expr (cadr e))
             (parse-expr (caddr e))
             (parse-expr (cadddr e)))]
   [(and (list? e)
         (= 3 (length e))
         (member (car e) binops))
    (binop (car e)
           (parse-expr (cadr e))
           (parse-expr (caddr e)))]
   [(and (list? e)
         (name-fun? (car e)))
    (call (car e) (map parse-expr (cdr e)))]
   [else (error 'parse-error)]))

(define (parse-fun e)
  (cond
   [(and (list? e)
         (= (length e) 4)
         (eq? (car e) 'fun)
         (name-fun? (cadr e)))
    (fun (cadr e) (caddr e) (parse-expr (cadddr e)))]
   [else (error 'parse-error)]))

(define (parse-prog e)
  (cond
   [(and (list? e)
         (= (length e) 2))
    (let ((proc (map parse-fun (car e))))
      (prog proc (parse-expr (cadr e))))]
   [else (error 'parse-error)]))


; testy
(parse-prog simple)
(parse-prog fact)
(parse-prog odd-even)


;;============================ Zadanie 2 - Ewaluator ===========================
;; Środowiska

(struct not-found ())

(define (env-lookup x ρ)
  (let ((p (assoc x ρ)))
    (if (pair? p)
        (cdr p)
        (not-found))))

(define (env-add x v ρ)
  (cons (cons x v) ρ))

(define env-empty null)


;; Ewaluator

(struct unbound-variable (name))
(struct type-error (expected value))
(struct eval-error (reason))

(define (op->proc op)
  (match op
    ['+ +]
    ['- -]
    ['* *]
    ['/ (lambda (x y) (if (= y 0) (raise (eval-error "Division by zero")) (/ x y)))]
    ['= =]
    ['< <]
    ['> >]))

(define (value? v)
  (or (number? v)
      (boolean? v)))

(define (eval-expr ϑ ρ e)
  (match e
    [(const n) n]
    [(var-expr x)
     (match (env-lookup x ρ)
       [(not-found) (raise (unbound-variable x))]
       [v v])]
    [(let-expr x e1 e2)
     (let* ((v (eval-expr ϑ ρ e1))
            (ρ (env-add x v ρ)))
       (eval-expr ϑ ρ e2))]
    [(if-expr eb et ef)
     (match (eval-expr ϑ ρ eb)
       [#t (eval-expr ϑ ρ et)]
       [#f (eval-expr ϑ ρ ef)]
       [v  (raise (type-error 'boolean v))])]
    [(binop op el er)
     (let* ((vl (eval-expr ϑ ρ el))
            (vr (eval-expr ϑ ρ er)))
       ((op->proc op) vl vr))]
    [(call f es)
     (let ((proc (env-lookup f ϑ)))
       (match proc
         [(clo args body)
          (let ((params (map (lambda (x) (eval-expr ϑ ρ x)) es)))
            (eval-expr ϑ (make-env env-empty args params) body))]
         [else (error "Eval error")]))]
     [else (error "Not implemented!")]))

(define (make-env env xs vs)
  (if (null? xs)
      env
      (env-add (car xs) (car vs) (make-env env (cdr xs) (cdr vs)))))

(struct clo (args body) #:transparent)

(define (eval-prog p)
  (define (make-fun-env functions)
    (if (null? functions)
        env-empty
        (match (car functions)
          [(fun Name args body)
           (env-add Name (clo args body) (make-fun-env (cdr functions)))]
          [else (error "Make-fun-env error")])))
  (match p
    [(prog funs expr)
     (eval-expr (make-fun-env funs) env-empty expr)]
    [else (error "Eval-prog error")]))




      