#lang racket

(require racket/file)

; This time is a procedural algorithm.
; The idea is to iterate over the lines and keep a vector that tracks, for each position
; in the line, how many paths lead to it.
; Once all the lines have been processed, I just need to sum all the entries of the vector.
(define (solve lines)
  (define partials
    (for/vector ([ch (car lines)]) (if (char=? ch #\S) 1 0)))

  (for ([line (cdr lines)])
    (for ([(ch i) (in-indexed (string->list line))])
      (when (> (vector-ref partials i) 0)
        (when (char=? ch #\^)
          (let [(n (vector-ref partials i))]
            (vector-set! partials (sub1 i) (+ (vector-ref partials (sub1 i)) n))
            (vector-set! partials (add1 i) (+ (vector-ref partials (add1 i)) n))
            (vector-set! partials i 0))))))

  (for/sum [(x partials)] x))


(module+ main
  (define lines (file->lines "./day-7/input"))
  (printf "Solution: ~a\n" (solve lines)))
