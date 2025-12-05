#lang racket

(require racket/file)
(require "common.rkt")

(define (solve input)
  (define-values (ranges _) (parse input))
  (for/fold ([partial 0])
            ([r ranges])
    (+ partial (- (cadr r) (car r)) 1)))

(define input (string-trim (file->string "./day-5/input")))
(printf "Solution: ~a\n" (solve input))
