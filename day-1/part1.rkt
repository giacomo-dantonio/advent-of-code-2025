#lang racket

(require racket/file)
(require "base.rkt")

(define (update-counter distance dial counter)
  (let ([dial (modulo (+ dial distance) 100)])
    (if (= dial 0) (+ counter 1) counter)))

(define (solve rotations start) (solve-base update-counter rotations start))

(define rotations (file->lines "./day-1/input"))
(define start 50)  ; The dial starts by pointing at 50.

(define-values (dial resets) (solve rotations start))

(printf "dial: ~a\t resets: ~a\n" dial resets)
