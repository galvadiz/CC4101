;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               TESTS - TAREA 4                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#lang play
(require "P2.rkt")
(print-only-errors #t)

(test (run '(with (x 9)
                  (if0 (seq (set x 2)
                            (- 3 3))
                       (+ 1 x)
                       (- 14 x))))
      (numV 3))

(test (run '(with (x 16)
                  (with (f (fun (y) (- x y)))
                             (with (x 5) (- x (f 4))))))
      (numV -7))

(test (run '(with (x 3)
                  (with (f (fun (y) (+ x y)))
                        (f 4))))
      (numV 7))

(test (run '(with (f (fun (y) y)) (f 4)))
      
      (numV 4))


