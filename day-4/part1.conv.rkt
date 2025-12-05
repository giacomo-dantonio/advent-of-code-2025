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

(define (solve lines)
  (define pos (positions lines))
  (define conv (convolve2d pos kernel))

  (define accessible-positions
    (array-map (lambda (sh)
                 (let* ([n (array-ref conv sh)]
                        [p (array-ref pos sh)])
                   (if (and (< n 4) (> p 0)) 1 0)))
               (indexes-array (array-shape pos))))

  (array-all-sum accessible-positions))

(define lines (file->lines "./day-4/input"))
(printf "Solution: ~a\n" (solve lines))
