;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               TESTS - TAREA 4                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang play
(require "P3.rkt")
(print-only-errors #t)

(define mouse (equilibrium
               [run : (0 → run)
                    (1/2 → eat)
                    (1/2 → sleep)]
               [eat : (1/4 → run)
                    (0 → eat)
                    (3/4 → sleep)]
               [sleep : (1/3 → run)
                       (2/3 → eat)
                       (0 → sleep)]))

(test mouse '(12/53 20/53 21/53))

