#lang racket

(require racket/file)
(require "base.rkt")

; If the distance is positive we can count the number of times the dial points at 0 during the
; rotation by using integer division. This, however, doesn't work for negative distances.
; To solve the problem I use simmetry to reduce the problem to the positive case.
(define (update-counter distance dial counter)
  (define (update-counter-pos distance dial counter)
    (+ counter (quotient (+ dial distance) 100)))

  (if (>= distance 0)
      (update-counter-pos distance dial counter)
      (let ([dial (modulo (- 100 dial) 100)]
            [distance (- distance)])
        (update-counter-pos dial distance counter)
        )
      )
  )

(define (solve rotations start) (solve-base update-counter rotations start))

(define rotations (file->lines "./day-1/input"))
(define start 50)  ; The dial starts by pointing at 50.

(define-values (dial resets) (solve rotations start))

(printf "dial: ~a\t resets: ~a\n" dial resets)
