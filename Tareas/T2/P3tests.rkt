;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               TESTS - TAREA 2                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang play
(require "P3.rkt")
(print-only-errors #t)

;; Parse

(test (parse 8) (real 8))
(test (parse '(+ 1 (2)i)) (comp 1 2))
(test (parse '(+ 3 6)) (add (real 3) (real 6)))
(test (parse 'x) (id 'x))
(test (parse '(fun (x) (+ 2 x))) (fun 'x (add (real 2) (id 'x))))
(test (parse '((fun (x) (+ 2 x)) (+ 2 3))) (app (fun 'x (add (real 2) (id 'x))) (add (real 2) (real 3))))
(test (parse '((+ y 5) where y = (+ 2 4))) (app (fun 'y (add (id 'y) (real 5))) (add (real 2) (real 4))))


;; num+

(test (num+ (realV 1) (realV 6)) (realV 7))
(test (num+ (compV 1 2) (realV 6)) (compV 7 2))
(test (num+ (realV 6) (compV 1 2)) (compV 7 2))
(test (num+ (compV 1 2) (compV 1 2)) (compV 2 4))


;; ENV

(define newEnv0 empty-env)
(test newEnv0 (mtEnv))

(define newEnv1 (extend-env 'A 3 newEnv0))
(test newEnv1 (aEnv 'A 3 (mtEnv)))

(define newEnv2 (extend-env 'B 4 newEnv1))
(test newEnv2 (aEnv 'B 4 (aEnv 'A 3 (mtEnv))))

(test (env-lookup 'A newEnv2) 3)
(test (env-lookup 'B newEnv2) 4)
(test/exn (env-lookup 'C newEnv2) "free identifier: C")


;; eval

(test (eval (parse '((f (+ 2 (+ 1 (2)i))) where f = (fun (x) (+ x x)))) empty-env) (compV 6 4))
(test/exn (eval (parse '((f (+ 2 (+ 1 (2)i))) where g = (fun (x) (+ x x)))) empty-env) "free identifier: f")

;; run

(test (run '((f (+ 2 (+ 1 (2)i))) where f = (fun (x) (+ x x)))) (compV 6 4))
(test/exn (run '((f (+ 2 (+ 1 (2)i))) where g = (fun (x) (+ x x)))) "free identifier: f")
