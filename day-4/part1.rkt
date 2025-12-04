#lang racket

; Naive solution which iterates over all positions and counts neighbours.

(require racket/file)
(require math/array)
(require "base.rkt")

(define (values lines)
    (define pos (positions lines))
    (accessible pos))

(define (solve lines) (array-all-sum (values lines)))

(define lines (file->lines "./day-4/input"))
(printf "Solution: ~a\n" (solve lines))
