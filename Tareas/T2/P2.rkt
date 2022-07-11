;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 P2 - TAREA 2                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOMBRE APELLIDO: GERALDINE ALVADIZ
;; Mucho éxito :)

#lang play

#| <logic> ::= 
    | (bool <bool>)
    | (id <id>)
    | (band <logic> <logic>)
    | (bor <logic> <logic>)
    | (with <id> <logic> <logic>)
|#

#| ================================
                PARTE A
   ================================|#

(deftype Logic
  (bool b)
  (id x)
  (band l r)
  (bor l r)
  (with x l r))


#| ================================
                PARTE B
   ================================ |#

;; s-expressions used as concrete 
;; syntax for our programs
#|
<s-expr> ::= 'True
           | 'False
           | <sym>
           | (list <s-expr> '^ <s-expr>)
           | (list <s-expr> 'v <s-expr>)
           | (list 'with (list <sym> <s-expr>) <s-expr>)
|#

;; parse :: s-expr -> logic
;; converts s-expressions into logic
(define (parse s-expr)
  (match s-expr
    ['True (bool #t)]
    ['False (bool #f)]
    [ x #:when (symbol? x) (id x)]
    [(list l '^ r) (band (parse l) (parse r))]
    [(list l 'v r) (bor (parse l) (parse r))]
    [(list 'with (list x e) b) #:when (symbol? x)
         (with x (parse e) (parse b))]))




#| ================================
                PARTE C 
   ================================ |#

;; Values of logic
;; <LValue> ::= (BoolV <bool>)
(deftype LValue
  (BoolV b))


#| ================================
                PARTE D
   ================================ |#


;; Interfaz del tipo de dato abstracto que
;; representa los ambientes de identificadores.
;; empty-env  :: Env
;; extend-env :: Sym LValue Env -> Env
;; env-lookup :: Sym Env -> LValue
(deftype Env
  (mtEnv)
  (aEnv id val env))

(define empty-env (mtEnv))
 
(define extend-env aEnv)
 
(define (env-lookup x env)
  (match env
    [(mtEnv) (error 'env-lookup "Identificador ~a no definido" x)]
    [(aEnv id val rest) (if (symbol=? id x)
                            val
                            (env-lookup x rest))]))

;; interp :: Expr Env -> LValue
(define (interp expr env)
  (match expr
    [(bool b) (BoolV b)]
    [(id x) (env-lookup x env)]
    [(band l r) (let ([br (BoolV-b (interp r env))]
                      [lr (BoolV-b (interp l env))])
                  (BoolV (and br lr)))]
    [(bor l r) (let ([br (BoolV-b (interp r env))]
                     [lr (BoolV-b (interp l env))])
                 (BoolV (or br lr)))]
    [(with x e b) (def new-env (extend-env x (interp e env) env))
       (interp b new-env)]))
