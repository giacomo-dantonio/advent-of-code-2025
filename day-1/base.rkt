#lang racket

(define (solve-base update-counter rotations start)
  (for/fold ([dial start]
             [counter 0])
            ([rotation rotations])
    (let* ([direction (string-ref rotation 0)]
           [distance (string->number (substring rotation 1))]
           [distance (if (char=? direction #\L) (- distance) distance)]
           [next-dial (modulo (+ dial distance) 100)]
           [counter (update-counter distance dial counter)])
      (values next-dial counter))))

(provide solve-base)