#lang racket

(require data/union-find)
(require racket/file)

(define (parse-lines lines)
  (for/list ([line lines])
    (map string->number (string-split line ","))))

(define (sqr-distance lhs rhs)
  (for/sum ([a lhs]
            [b rhs])
    (sqr (- b a))))

(define (distances points)
  (sort
   (for*/list ([(lhs i) (in-indexed points)]
               [(rhs j) (in-indexed points)]
               #:when (< i j))
     (list i j (sqr-distance lhs rhs)))
   <
   #:key caddr))

(define (circuits points N)
  (define indices (for/vector ([i (range 0 (length points))]) (uf-new i)))
  (for ([triple (take (distances points) N)])
    (match triple [(list i j _)
                   (let ([ui (vector-ref indices i)]
                         [uj (vector-ref indices j)])
                     (uf-union! ui uj))
                   ]))

  (define result (make-hash))
  (for ([(u i) (in-indexed indices)])
    (let* ([a (uf-find u)]
           [circuit (hash-ref result a '())])
      (hash-set! result a (cons i circuit))))

  (hash-values result))

(define (solve lines)
  (define points (parse-lines lines))
  (define circus (circuits points 1000))

  (define circuit-lengths
    (sort (map length circus) >))
  (apply * (take circuit-lengths 3)))

(define lines (file->lines "./day-8/input"))
(printf "Solution: ~a\n" (solve lines))
