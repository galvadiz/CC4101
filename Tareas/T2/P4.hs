{-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 P4 - TAREA 2                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NOMBRE APELLIDO: GERALDINE ALVADIZ
-}

triangulo_stirling :: [[Integer]]
triangulo_stirling = map t_cada_n [1..]

t_cada_n :: Integer -> [Integer]
t_cada_n n = zipWith t_recurrencia (take (fromInteger n) (repeat (n-1))) [0..(n-1)]

t_recurrencia :: Integer -> Integer -> Integer
t_recurrencia 0 0 = 1
t_recurrencia _ 0 = 0
t_recurrencia m k
    | m == k = 1
    | otherwise = (t_recurrencia (m-1) (k-1)) + (k * (t_recurrencia (m-1) k))

main :: IO()
main = return()