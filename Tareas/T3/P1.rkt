#lang play

#|
<expr> ::= (num <num>)
         | (add <expr> <expr>)
         | (sub <expr> <expr>)
         | (div <expr> <expr>)
         | (if0 <expr> <expr> <expr>)
         | (id <id>)
         | (nil)
         | (lista <expr> <expr>)
         | (fun <id> <expr>)
         | (app <expr> <expr>)
         | (matchh <id> <expr> <id> <id> <expr>)
|#
;; Inductive type for representing (the abstract syntax
;; of) an aritmetical language with first-class functions
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (div l r)
  (if0 c t f)
  (id x)
  (nil)
  (lista a b)
  (fun arg body)
  (app f-name f-arg)
  (matchh x v_nill xl xr e))




;; s-expressions used as concrete syntax for our programs
#|
<s-expr> ::= <num>
           | (list 'cons <s-expr> <s-expr>)
           | 'nil
           | <sym>
           | (list '+  <s-expr> <s-expr>)
           | (list '-  <s-expr> <s-expr>)
           | (list '/  <s-expr> <s-expr>)
           | (list 'if0  <s-expr> <s-expr> <s-expr>)
           | (list 'fun (list <sym>) <s-expr>)
           | (list <s-expr> <s-expr>)
           | (list 'with (list <sym> <s-expr>) <s-expr>)   <- syntactic sugar
           | (list 'match <sym> 'as (list 'nil '=> <s-expr>) (list 'cons  <sym>  <sym> '=> <s-expr>))
|#

;; parse :: s-expr -> Expr
;; converts s-expressions into Exprs
(define (parse s-expr)
  (match s-expr
    [ n #:when (number? n) (num n) ]
    [(list 'cons x xs) (lista (parse x) (parse xs))]
    [ 'nil (nil)]
    [ x #:when (symbol? x) (id x) ]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list '/ l r) (div (parse l) (parse r))]
    [(list 'if0 c t f) (if0 (parse c) (parse t) (parse f))]
    [(list 'fun (list x) b) (fun x (parse b))]
    [(list f a) (app (parse f) (parse a))]    
    [(list 'with (list x e) b) #:when (symbol? x)
         (app (fun x (parse b)) (parse e))]
    [(list 'match x 'as (list 'nil '=> v) (list 'cons xl xr '=> b)) (matchh x (parse v) xl xr (parse b))]))



;; Interface of the Abstract Dada Type (ADT) for  
;; representing idenfifier environments

;; empty-env  :: Env
;; extend-env :: Symbol Value Env -> Env
;; env-lookup :: Symbol Env -> Value

;; Implementation of the ADT

;; <env> ::= mtEnv
;;         | (aEnv <id> <value> <env>)
(deftype Env
  (mtEnv)
  (aEnv id val env))

(define empty-env (mtEnv))
 
(define extend-env aEnv)
 
(define (env-lookup x env)
  (match env
    [(mtEnv) (error 'env-lookup "free identifier: ~a" x)]
    [(aEnv id val rest) (if (symbol=? id x)
                            val
                            (env-lookup x rest))]))

;; values of expressions
(deftype Value
  (numV n)
  (fclosureV id body env)
  (exprV expr env cache))


;; binop :: (Num Num -> Num) -> (Value Value -> Value)
;; Lifts a binary numeric operator to (numeric) Value's 
(define (binop op)
  (λ (n1 n2)
    (def (numV v1) n1) (def (numV v2) n2) (numV (op v1 v2))))

;; unop :: (Num -> A) -> (Value -> A)
;; Lifts a function over Num to (numeric) Value 
(define (unop op)
  (λ (n) (def (numV v) n) (op v)))

;; Further reduces a Value to a numV or closureV
;; and caches the reduced value
;; strict :: Value -> Value [without exprV]
(define (strict v)
  (match v
    [(exprV expr env cache)
     (if (not (unbox cache))
         (let ([val (strict (eval expr env))])
         (set-box! cache val)
         val)
         (unbox cache))]
    [ _ v]))


;; eval :: Expr Env -> Value
;; evaluates an expression in a given environment
;; using static scope and lazy evaluation
(define (eval expr env)
  (match expr
    [(num n) (numV n)]
    [(nil) (exprV (nil) env (box #f))]
    [(fun id body) (fclosureV id body env)]
    [(lista l r) (exprV (lista l r) env (box #f))]
    [(id x) (env-lookup x env)]
    [(add l r) ((binop +) (strict (eval l env)) (strict (eval r env)))]
    [(sub l r) ((binop -) (strict (eval l env)) (strict (eval r env)))]
    [(div l r) ((binop quotient) (strict (eval l env)) (strict (eval r env)))]
    [(matchh x v xl xr b) (let ([listita (env-lookup x env)])
                            (let ([env-lista (exprV-env listita)])
                            (match (exprV-expr listita)
                            [(nil) (strict (eval v env))]
                            [(lista l r) (match b
                                           [(id u) (if (equal? u xl)
                                                       (exprV l env-lista (box #f))
                                                       (exprV r env-lista (box #f)))]
                                           [_ (eval b (aEnv xl (exprV l env-lista (box #f)) (aEnv xr (exprV r env-lista (box #f)) (mtEnv))))]
                                           )])))]
    [(if0 c t f) (if  ((unop zero?) (strict (eval c env)))
                      (eval t env)
                      (eval f env))]
    [(app f e) (def (fclosureV the-arg the-body the-clos-env) (strict (eval f env)))
               (def the-ext-env (extend-env the-arg
                                            (exprV e env (box #f))
                                            the-clos-env))
               (eval the-body the-ext-env)]))


;; run :: s-expr -> value
;; evaluates an expression using static scoping 
(define (run prog)
  (eval (parse prog) empty-env))


;; run :: s-expr -> value
;; evaluates an expression using static scope and lazy evaluation 
;;(define (run prog)
;;void)




