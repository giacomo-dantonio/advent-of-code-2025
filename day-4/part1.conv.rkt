#lang racket

(require racket/file)
(require math/array)

(require "convolution.rkt")

; Solution which uses convolution to count neighbours.
; Uses a naive algorithm to compute the convolution,
; unfortunately this is slower than the naive solution.

(define (positions lines)
    (define shape (vector (length lines) (string-length (car lines))))
    (define chars (string->list (string-join lines "")))

    (for/array: #:shape shape ([char chars]) (if (not (char=? char #\.)) 1 0)))

(define kernel
    (array #[#[1 1 1]
             #[1 0 1]
             #[1 1 1]]))

(define (neighbourhood mat i j)
    (define (array-h A) (vector-ref (array-shape A) 0))
    (define (array-w A) (vector-ref (array-shape A) 1))

    (let ([fromi (max 0 (- i 1))]
          [toi (min (array-h mat) (+ i 2))]
          [fromj (max 0 (- j 1))]
          [toj (min (array-w mat) (+ j 2))])
        (array-slice-ref mat (list (in-range fromi toi) (in-range fromj toj)))))

(define (neighbours mat i j)
    (if (positive? (array-ref mat (vector i j)))
        (- (array-count positive? (neighbourhood mat i j)) 1)
        0))

(define (values lines)
    (define pos (positions lines))

    (array-map (lambda (sh)
                (let* ([i (vector-ref sh 0)]
                        [j (vector-ref sh 1)]
                        [n (neighbours pos i j)])
                (and (< n 4) (> n 0)))
            )
        (indexes-array (array-shape pos))))

(define (solve lines)
    (define pos (positions lines))
    (define conv (convolve2d pos kernel))

    (define accessible-positions
        (array-map (lambda (sh)
                (let* ([i (vector-ref sh 0)]
                       [j (vector-ref sh 1)]
                       [n (array-ref conv sh)]
                       [p (array-ref pos sh)])
                (if (and (< n 4) (> p 0)) 1 0)))
        (indexes-array (array-shape pos))))

    (array-all-sum accessible-positions))

(define lines (file->lines "./day-4/input"))
(printf "Solution: ~a\n" (solve lines))
