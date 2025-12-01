#lang racket

(require racket/file)

(define (solve rotations start)
    (for/fold ([dial start]
               [nzeros 0])
              ([rotation rotations])
        (let* ([direction (string-ref rotation 0)]
               [distance (string->number (substring rotation 1))]
               [dial (modulo (cond [(char=? direction #\R) (+ dial distance)]
                                        [(char=? direction #\L) (- dial distance)]
                                        [else dial])
                                    100)]
               [nzeros (if (= dial 0) (+ nzeros 1) nzeros)])
            (values dial nzeros)
        )))

(define rotations (file->lines "./day-1/input"))
(define start 50)  ; The dial starts by pointing at 50.

(define-values (dial zeros) (solve rotations start))

(printf "dials: ~a\t resets: ~a\n" dial zeros)
