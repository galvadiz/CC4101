;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  BASE - P2                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
Nombre y Apellido: Geraldine Alvadiz
- Modificar y guardar como P2.rkt
- Recuerde hacer los tests en P2_tests.rkt
|#

#lang play

#|
<expr> ::= (num <num>)
         | (add <expr> <expr>)
         | (sub <expr> <expr>)
         | (if0 <expr> <expr> <expr>)
         | (id <id>)
         | (fun <sym> <expr>)
         | (app <expr> <expr>)
         | (seq <expr> <expr>)          parte (a)
|#
;; Inductive type for representing (the abstract syntax
;; of) an aritmetical language with first-class functions
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (if0 c t f)
  (id x)
  (fun arg body)
  (app f-name f-arg)
  (seq l r) ;; parte (a)
  (set id expr) ;; parte (b)
  )



;; s-expressions used as concrete syntax for our programs
#|
<s-expr> ::= <num>
           | <sym>
           | (list '+  <s-expr> <s-expr>)
           | (list '-  <s-expr> <s-expr>)
           | (list 'if0  <s-expr> <s-expr> <s-expr>)
           | (list 'fun (list <sym>) <s-expr>)
           | (list <s-expr> <s-expr>)
           | (list 'with (list <sym> <s-expr>) <s-expr>)   <- syntactical sugar
           | (list 'seq <s-expr> <s-expr>)
           | (list 'set <sym> <s-expr>)
|#

;; parse :: s-expr -> Expr
;; converts s-expressions into Exprs
(define (parse s-expr)
  (match s-expr
    [ n #:when (number? n) (num n) ]
    [ x #:when (symbol? x) (id x) ]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'if0 c t f) (if0 (parse c) (parse t) (parse f))]
    [(list 'fun (list x) b) (fun x (parse b))]
    [(list f a) (app (parse f) (parse a))]    
    [(list 'with (list x e) b) #:when (symbol? x)
         (app (fun x (parse b)) (parse e))]
    [(list 'seq l r) (seq (parse l) (parse r))] ;; parte (a)
    [(list 'set id expr) (set id (parse expr))] ;; parte (b)
    ))



;; Interface of the Abstract Dada Type (ADT) for  
;; representing idenfifier environments

;; empty-env  :: Env
;; extend-env :: Symbol Value Env -> Env
;; lookup-env :: Symbol Env -> Value

;; Implementation of the ADT

;; <env> ::= mtEnv
;;         | (aEnv <id> <value> <env>)
(deftype Env
  (mtEnv)
  (aEnv id val env))

(define empty-env (mtEnv))
 
(define extend-env aEnv)
 
(define (lookup-env x env)
  (match env
    [(mtEnv) (error 'lookup-env "free identifier: ~a" x)]
    [(aEnv id val rest) (if (symbol=? id x)
                            val
                            (lookup-env x rest))]))


;; values of expressions
;; <value> ::= (numV <number>)
;;          |  (closureV <sym> <s-expr> <env>) 
(deftype Value
  (numV n)
  (closureV id body env))

#|-----------------------------
Store abstract data type
 
empty-sto  :: Sto
extend-sto :: Loc Val Sto -> Sto
sto-lookup :: Loc Sto -> Val
 
representation BNF:
<sto> ::= (mtSto)
        | (aSto <loc> <val> <sto>)
<loc> ::= number
|#
(deftype Sto
  (mtSto)
  (aSto loc val sto))
 
(def empty-sto  (mtSto))
 
(def extend-sto aSto)
 
(define (sto-lookup l sto)
  (match sto
    [(mtSto) (error 'sto-lookup "not value at location: ~a" l)]
    [(aSto loc val rest)
     (if (= loc l)
         val
         (sto-lookup l rest))]))

;; value and storage of expresions
;; <value> <sto> ::= (numV <number>)
;;                |  (closureV <sym> <s-expr> <env>)  
(deftype Value*Store
  (v*s val sto))

;; Auxiliary functions handling numeric values
;; binop :: (Num Num -> Num) -> (Value Value -> Value)
;; Lifts a binary numeric operator to (numeric) Value's 
(define (binop op)
  (λ (n1 n2)
    (def (numV v1) n1) (def (numV v2) n2) (numV (op v1 v2))))

;; unop :: (Num -> A) -> (Value -> A)
;; Lifts a function over Num to (numeric) Value 
(define (unop op)
  (λ (n) (def (numV v) n) (op v)))


;; eval :: Expr Env -> Value
;; evaluates an expression in a given
;; environment using static scoping 
(define (eval expr env sto)
  (match expr
    [(num n) (v*s (numV n) sto)]
 
    [(fun id body) (v*s (closureV id body env) sto)]
 
    [(add l r)
     (def (v*s l-val l-sto) (eval l env sto))
     (def (v*s r-val r-sto) (eval r env l-sto))
     (v*s (num+ l-val r-val) r-sto)]
 
    [(sub l r)
     (def (v*s l-val l-sto) (eval l env sto))
     (def (v*s r-val r-sto) (eval r env l-sto))
     (v*s (num- l-val r-val) r-sto)]
 
    [(if0 c t f)
     (def (v*s c-val c-sto) (eval c env sto))
     (if (num-zero? c-val)
         (eval t env c-sto)
         (eval f env c-sto))]
 
    [(id x) (v*s (sto-lookup (lookup-env x env) sto) sto)]
 
    [(app fun-expr arg-expr)
     (def (v*s (closureV id body fenv)
               fun-sto) (eval fun-expr env sto))
     (def (v*s arg-val arg-sto) (eval arg-expr env fun-sto))
     (def new-loc (next-location arg-sto))
     (eval body
             (extend-env id new-loc fenv)
             (extend-sto new-loc arg-val arg-sto))]
 
    [(seq l r)
     (def (v*s _ sto1) (eval l env sto))
     (eval r env sto1)]  ;; parte (a)
    
    [(set id val-expr)
     (def loc (lookup-env id env))
     (def (v*s val-val val-sto) (eval val-expr env sto))
     (v*s val-val
          (extend-sto loc val-val val-sto))]
))

(define (next-location sto)
  (match sto
    [(mtSto) 0]
    [(aSto _ _ rest) (add1 (next-location rest))]))

(define (num+ n1 n2)
  (numV (+ (numV-n n1) (numV-n n2))))
 
(define (num- n1 n2)
  (numV (- (numV-n n1) (numV-n n2))))
 
(define (num-zero? n)
  (zero? (numV-n n)))

;; is-ref-expr?:: sym -> boolean
;; Retorna verdadero si un simbolo corresponde a una expresion de referencia ('&var), o falso sino.
(define (is-ref-expr? expr)
  (def symstr (symbol->string expr))
  (equal? (string-ref symstr 0) #\&)) 

;; get-id:: sym -> sym
;; Retorna el valor de una id de referencia ('&var) en su id real ('var).
(define (get-id id)
  (string->symbol (substring (symbol->string id) 1)))


;; run :: s-expr -> value
;; evaluates an expression using static scoping 
(define (run prog)
  (match (eval (parse prog) empty-env empty-sto)
    [(v*s (numV n) s) (numV n)]
    [(v*s (and f (closureV id body env)) s) f]))