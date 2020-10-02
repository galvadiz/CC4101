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



;; Parte d)
;; fold :: (Integer -> A) (Integer Integer A -> A) -> (CFraction -> A)



;; Parte e)
;; eval2 :: CFraction -> Rational

;; degree2 ::  CFraction -> Integer



;; Parte f)
;; mysterious-cf :: Integer -> CFraction



;; Parte g)
;; from-to :: Integer -> Integer -> listOf Integer

;; mysterious-list :: Integer -> listOf Float

;; A que numero tiende (mysterious-cf k) cuando k tiende a infinito?



;; Parte h)
;; rac-to-cf :: Rational -> CFraction











