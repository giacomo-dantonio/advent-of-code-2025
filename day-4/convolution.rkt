#lang racket

(require math/array)

;; Array dimensions
(define (array-h A) (vector-ref (array-shape A) 0))
(define (array-w A) (vector-ref (array-shape A) 1))

(define (convolve2d data kernel)
  (define H (array-h data))
  (define W (array-w data))

  (define KH (array-h kernel))
  (define KW (array-w kernel))

  ; kernel center
  (define KH/2 (quotient KH 2))
  (define KW/2 (quotient KW 2))

  ;; Safe access: returns 0 when outside bounds
  (define (ref-pad i j)
    (if (and (>= i 0) (< i H)
             (>= j 0) (< j W))
        (array-ref data (vector i j))
        0))

  ; copy input matrix to a result matrix with padding
  (define padshape
    (vector (+ H (* 2 KH/2)) (+ W (* 2 KW/2))))
  (define result (array->mutable-array (make-array padshape 0)))

  (for* ([i (range 0 W)]
         [j (range 0 H)])

    ; extract subarray from position i - KH/2, j - kcenter - j
    (let ([value (ref-pad  i j)]
          [buf (array-slice-ref result (list (in-range i (+ i KH))
                                             (in-range j (+ j KW))))])
      (array-slice-set! result
                        (list (in-range i (+ i KH))
                              (in-range j (+ j KW)))
                        (array+ buf (array* kernel (array value))))))

  (array-slice-ref result (list (in-range 1 (add1 H)) (in-range 1 (add1 W))))
  )

(provide convolve2d)