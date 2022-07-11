#lang play
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 P3 - TAREA 2                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOMBRE APELLIDO: GERALDINE ALVADIZ
;; Mucho éxito :)


#| ================================
                PARTE A
   ================================|#

#|
   <expr> ::= (real <num>)
           | (comp <num> <num>)
           | (add <expr> <expr>)
           | (id <id>)
           | (fun <sym> <expr>)
           | (app <expr> <expr>)
|#

(deftype expr
  (real n)
  (comp r i)
  (add l r)
  (id x)
  (fun arg body)
  (app f-name f-arg))


#| ================================
                PARTE B
   ================================ |#

#|
<s-expr> ::= <num>
           | <sym>
           | (list '+  <num> (list <num>)'i)
           | (list '+  <s-expr> <s-expr>)
           | (list 'fun (list <sym>) <s-expr>)
           | (list <s-expr> <s-expr>)
           | (list <s-expr> 'where <sym> '= <s-expr>)   <- syntactical sugar
|#
;; parse :: s-expr -> expr
;; converts s-expressions into exprs
(define (parse s-expr)
  (match s-expr
    [ n #:when (number? n) (real n) ]
    [ x #:when (symbol? x) (id x) ]
    [(list '+ r (list m) 'i) #:when (and (number? r) (number? m)) (comp r m)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list 'fun (list x) b) (fun x (parse b))]
    [(list f a) (app (parse f) (parse a))]
    [(list b 'where x '= e) #:when (symbol? x)
                           (app (fun x (parse b)) (parse e))]))


#| ================================
                PARTE C
   ================================ |#

;; Values of Expressions
;; <value> ::= (realV <number>)
;;          |  (compV <number> <number>)
;;          |  (closureV <sym> <s-expr> <env>) 
(deftype Value
  (realV n)
  (compV r i)
  (closureV id body env))


;; num+ :: Value Value -> Value
(define (num+ n1 n2)
  (match n1
    [(compV real1 img1) (match n2
                          [(compV real2 img2) (compV (+ real1 real2) (+ img1 img2))]
                          [(realV real2) (compV (+ real1 real2) img1)])]
    [(realV real1) (match n2
                     [(compV real2 img2) (compV (+ real1 real2) img2)]
                     [(realV real2) (realV (+ real1 real2))])]))



#| ================================
                PARTE D
   ================================ |#

;; Interfaz del tipo de dato abstracto que
;; representa los ambientes de identificadores.
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


;; eval :: Expr Env -> Value
;; evaluates an expression in a given
;; environment using static scoping 
(define (eval expr env)
  (match expr
    [(real n) (realV n)]
    [(comp r i) (compV r i)]
    [(fun id body) (closureV id body env)]
    [(id x) (env-lookup x env)]
    [(add l r) (num+ (eval l env) (eval r env))]
    [(app f e) (def (closureV the-arg the-body the-claus-env) (eval f env))
               (def the-ext-env (extend-env the-arg (eval e env) the-claus-env))
               (eval the-body the-ext-env)]))


;; run :: s-expr -> value
;; evaluates an expression using static scoping
(define (run prog)
  (eval (parse prog) empty-env))



