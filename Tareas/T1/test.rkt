#lang play
(require "T1.rkt")

(print-only-errors #t)

;; Definiciones para hacer tests

(define cf-1 (compound 1 2
                       (compound 3 4
                                 (compound 5 6
                                           (compound 7 8
                                                     (simple 9))))))
(define cf-2 (compound 9 8
                       (compound 7 6
                                 (compound 5 4
                                           (compound 3 2
                                                     (simple 1))))))
(define cf-3 (simple 4))
(define cf-4 (compound 1 4(simple 2)))


;; Parte b)

(test (eval cf-1) 2329/1511)
(test (eval cf-2) 2329/233)
(test (eval cf-3) 4)
(test (eval cf-4) 3)


;; Parte c)

(test (degree cf-1) 4)
(test (degree cf-2) 4)
(test (degree cf-3) 0)
(test (degree cf-4) 1)


;; Parte d)
;; Se prueba con las funciones identity y +
;; Representa la suma de todos los numeros (numeradores y denominadores) de la fraccion continua.
;; Ejemplo: (compound 5 6 (simple 9)) es 20 (5 + 6 + 0 = 20).

(test ((fold identity +) cf-1) 45)
(test ((fold identity +) cf-2) 45)
(test ((fold identity +) cf-3) 4)
(test ((fold identity +) cf-4) 7)


;; Parte e)

(test (eval2 cf-1) 2329/1511)
(test (eval2 cf-2) 2329/233)
(test (eval2 cf-3) 4)
(test (eval2 cf-4) 3)

(test (degree2 cf-1) 4)
(test (degree2 cf-2) 4)
(test (degree2 cf-3) 0)
(test (degree2 cf-4) 1)


;; Parte f)

(test/exn (mysterious-cf -1) "Error: argumento negativo")
(test (mysterious-cf 0) (simple 3))
(test (mysterious-cf 1) (compound 3 1
                                  (simple 6)))
(test (mysterious-cf 2) (compound 3 1
                                  (compound 6 9
                                            (simple 6))))
(test (mysterious-cf 3) (compound 3 1
                                  (compound 6 9
                                            (compound 6 25
                                                      (simple 6)))))
(test (mysterious-cf 4) (compound 3 1
                                  (compound 6 9
                                            (compound 6 25
                                                      (compound 6 49
                                                                (simple 6))))))


;; Parte g)

(test/exn (from-to 10 0) "Error: Entero final es menor que el inicial")
(test (from-to 0 0) '(0))
(test (from-to 1 5) '(1 2 3 4 5))
(test (from-to 128 130) '(128 129 130))
(test (from-to 0 10) '(0 1 2 3 4 5 6 7 8 9 10))

(test/exn (mysterious-list -1) "Error: argumento negativo")
(test (mysterious-list 0) '())
(test (mysterious-list 1) '(3.0))
(test (mysterious-list 2) '(3.0 3.16666666666666666))
(test (mysterious-list 3) '(3.0 3.16666666666666666 3.13333333333333333))
(test (mysterious-list 4) '(3.0 3.16666666666666666 3.13333333333333333 3.14523809523809523))


;; Parte h)

(test (rac-to-cf 45/9) (simple 5))
(test (rac-to-cf 25/4) (compound 6 1 (simple 4)))
(test (rac-to-cf 649/200) (compound 3 1
                                    (compound 4 1
                                              (compound 12 1
                                                        (simple 4)))))
(test/exn (rac-to-cf -24/10) "Error: argumento negativo")