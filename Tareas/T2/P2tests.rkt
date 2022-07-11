;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               TESTS - TAREA 2                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang play
(require "P2.rkt")
(print-only-errors #t)

;; Parse

(test (parse'True) (bool #t))
(test (parse'False) (bool #f))
(test (parse'(A v False)) (bor (id'A) (bool #f)))
(test (parse'(C ^ (A v B))) (band (id'C) (bor (id'A) (id'B))))
(test (parse'(with (A True) (A ^ A))) (with'A (bool #t) (band (id'A) (id'A))))

;; ENV

(define newEnv0 empty-env)
(test newEnv0 (mtEnv))

(define newEnv1 (extend-env 'A 3 newEnv0))
(test newEnv1 (aEnv 'A 3 (mtEnv)))

(define newEnv2 (extend-env 'B 4 newEnv1))
(test newEnv2 (aEnv 'B 4 (aEnv 'A 3 (mtEnv))))

(test (env-lookup 'A newEnv2) 3)
(test (env-lookup 'B newEnv2) 4)
(test/exn (env-lookup 'C newEnv2) "Identificador C no definido")

;; interp

(test (interp (parse'True) empty-env) (BoolV #t))
(test/exn (interp (parse'(A ^ False)) empty-env) "Identificador A no definido")
(test/exn (interp (parse'(C ^ (A v B))) empty-env) "Identificador B no definido")
(test (interp (parse'(with (A True) (A ^ False))) empty-env) (BoolV #f))
(test (interp (parse'(with (C True)
                           (with (B False)
                                 (with (A True) (False v A))))) empty-env) (BoolV #t))