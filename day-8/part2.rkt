#lang racket

(require data/union-find)
(require racket/file)
(require "common.rkt")

; Add junctions until one single circuit is reached
; and return the two points that close the circuit.
(define (circuits points)
  (define vpoints (list->vector points))
  (define indices (for/vector ([i (range 0 (vector-length vpoints))]) (uf-new i)))

  (define (circuits-count indices) (set-count (for/set ([u indices]) (uf-find u))))

  (for/last ([triple (distances points)]
             #:break (= (circuits-count indices) 1))
    (match triple [(list i j _)
                   (let ([ui (vector-ref indices i)]
                         [uj (vector-ref indices j)]
                         [p1 (vector-ref vpoints i)]
                         [p2 (vector-ref vpoints j)])
                     (uf-union! ui uj)
                     (list p1  p2))
                   ])))

(define (solve lines)
  (define points (parse-lines lines))
  (match (circuits points)
    [(list p q)
     (* (car p) (car q))]))

(module+ main
  (define lines (file->lines "./day-8/input"))
  (printf "Solution: ~a\n" (solve lines)))