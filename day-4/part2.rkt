#lang racket

(require racket/file)
(require math/array)
(require "base.rkt")

(define (iter pos partial i)
  (define acc (accessible pos))

  (let ([inc (array-all-sum acc)]
        [next (array- pos acc)])

    (printf "Iteration ~a: ~a\n" i inc)
    (if (<= inc 0)
        partial
        (iter next (+ partial inc) (add1 i)))))

(define (solve lines)
  (define pos (positions lines))
  (iter pos 0 1))

(define lines (file->lines "./day-4/input"))
(printf "Solution: ~a\n" (solve lines))
