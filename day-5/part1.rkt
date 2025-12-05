#lang racket

(require racket/file)
(require "common.rkt")

(define (process input)
  ; ids are sorted
  ; ranges are sorted and disjont
  (define-values (ranges ids) (parse input))

  (for/fold ([fresh '()]
             [ranges ranges])
            ([id ids])
    ; remove all the ranges to the left of id
    (let ([ranges (dropf ranges (lambda (r) (< (cadr r) id)))])
      (values
       ; if id is in the first range
       ; (i.e. (>= id (caar range)),
       ; because we know that (>= (cadr range) id))
       ; add it to the fresh ids
       ; else leave fresh as it is
       (if (and (not (empty? ranges)) (>= id (caar ranges)))
           (cons id fresh)
           fresh)
       ranges))))

(define (solve input)
  (define-values (fresh _) (process input))
  (length fresh))

(define input (string-trim (file->string "./day-5/input")))
(printf "Solution: ~a\n" (solve input))
