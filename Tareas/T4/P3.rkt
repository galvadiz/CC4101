#lang play
(require math/matrix)
(require math/array)

(defmac (equilibrium (state :(prob → action)...)...)
  #:keywords : →
  (let ([prob-trans (matrix [[prob ...]...])])
    
    (def matriz-probabilidad (matrix-transpose prob-trans)) ;; transponer
    
    (def identidad (identity-matrix (matrix-num-cols matriz-probabilidad))) ;; identidad

    (def unos (make-matrix (matrix-num-rows matriz-probabilidad) (matrix-num-cols matriz-probabilidad) 1)) ;; matriz de unos

    (def matriz-a (matrix- (matrix+ unos matriz-probabilidad) identidad))

    (def matriz-b (make-matrix (matrix-num-rows matriz-probabilidad) 1  1))

    (def matriz-c (matrix-solve matriz-a matriz-b))

    (matrix->list matriz-c)))


