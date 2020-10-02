#lang play
(require math/flonum)

#|
Complete sus datos personales:
NOMBRE Y APELLIDO: GERALDINE ALVADIZ
RUT: 19872851-9
|#

;; Parte a)
#|
<CFraction> ::= (simple <value>)
             |  (compound <value> <value> <CFraction>
|#
(deftype CFraction
  (simple value)
  (compound a b cfraction))
  


;; Parte b)
;; eval :: CFraction -> Rational
;; Evalúa una fracción continua, devolviendo el número racional que representa.
(define (eval cfraction)
  (match cfraction
    [(simple v) v]
    [(compound a b d) (+ a (/ b (eval d)))]))

;; Parte c)
;; degree ::  CFraction -> Integer
;; Devuelve el grado de una fracción continua.
(define (degree cfraction)
  (match cfraction
    [(simple _) 0]
    [(compound _ _ d) (+ 1 (degree d))]))


;; Parte d)
;; fold :: (Integer -> A) (Integer Integer A -> A) -> (CFraction -> A)
;; Captura el esquema de recursión asociado a CFraction.
(define (fold f g)
  (λ (cfraction)
    (match cfraction
      [(simple v) (f v)]
      [(compound a b d) (g a b ((fold f g) d))])))
  

;; Parte e)
;; eval2 :: CFraction -> Rational
;; Evalúa una fracción continua usando fold.
(define eval2
  (fold identity (λ (a b c) (+ a (/ b c)))))

;; degree2 ::  CFraction -> Integer
;; Devuelve el grado de una función continua usando fold.
(define degree2
  (fold (λ (v) 0) (λ (a b c) (+ 1 c))))


;; Parte f)
;; mysterious-cf :: Integer -> CFraction



;; Parte g)
;; from-to :: Integer -> Integer -> listOf Integer

;; mysterious-list :: Integer -> listOf Float

;; A que numero tiende (mysterious-cf k) cuando k tiende a infinito?



;; Parte h)
;; rac-to-cf :: Rational -> CFraction











