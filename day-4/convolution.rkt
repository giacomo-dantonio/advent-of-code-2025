#lang racket

(require math/array)

;; Array dimensions
(define (array-h A) (vector-ref (array-shape A) 0))
(define (array-w A) (vector-ref (array-shape A) 1))

;; Safe access: returns 0 when outside bounds
(define (A-ref-pad A i j)
  (define H (array-h A))
  (define W (array-w A))
  (if (and (>= i 0) (< i H)
           (>= j 0) (< j W))
      (array-ref A (vector i j))
      0))

(define (idxs i j kh kw)
    ;; kernel center
    (define shape (vector kh kw))
    (for*/array: #:shape shape
                 ([ii (range i (+ i kw))]
                  [jj (range j (+ j kh))])
        (vector ii jj)))

(define (convolve2d data kernel)
    (define H (array-h data))
    (define W (array-w data))

    (define KH (array-h kernel))
    (define KW (array-w kernel))

    ; kernel center
    (define kcenter-i (quotient KH 2))
    (define kcenter-j (quotient KW 2))

    ; copy input matrix to a result matrix with padding
    (define padshape
        (vector (+ H (* 2 kcenter-i)) (+ W (* 2 kcenter-j))))
    (define result (array->mutable-array (make-array padshape 0)))
    ; (array-indexes-set! result (idxs 1 1 H W) data)

    (for* ([i (range 0 W)]
           [j (range 0 H)])

        ; extract subarray from position i - kcenter-i, j - kcenter - j
        (let* ([fromi (- i kcenter-i)]
               [fromj (- j kcenter-j)]
               [indexes (idxs i j KH KW)]
               [value (A-ref-pad data i j)]
               [buf (array-indexes-ref result indexes)])
            (array-indexes-set! result indexes
                (array+ buf (array* kernel (array value))))))
    
    (array-indexes-ref result (idxs 1 1 H W))
)

(provide convolve2d)