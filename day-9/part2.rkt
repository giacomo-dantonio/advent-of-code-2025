#lang racket

(require racket/draw)
(require racket/file)
(require "common.rkt")

(define lines (file->lines "./day-9/input"))

(define points (parse lines))

(define (edge? p q) (ormap = p q))

(define (edges points)
  (for*/list ([(p i) (in-indexed points)]
              [(q j) (in-indexed points)]
              #:when (and (< i j) (edge? p q)))
    (list p q)))

(define (incidences edges)
  (define result (make-hash))

  (for ([edge edges])
    (match edge [(list p q)
      (let ([hp (hash-ref result p '())]
            [hq (hash-ref result q '())])
        (hash-set! result p (cons edge hp))
        (hash-set! result q (cons edge hq)))
      ]))
  
  result
)

(define (draw edges)
  (define target (make-bitmap 1000 1000))
  (define dc (new bitmap-dc% [bitmap target]))

  (for ([edge edges])
    (match edge [(list p q)
      (send dc draw-line
        (quotient (car p) 100) (quotient (cadr p) 100)
        (quotient (car q) 100) (quotient (cadr q) 100))]))
  
  (define inc (incidences edges))
  (for/list [(p points)
             #:when (> (length (hash-ref inc p)) 2)]
    (let ([x (quotient (car p) 100)]
          [y (quotient (cadr p) 100)]
          [w 2])
      (send dc draw-rectangle
        (- x w) (- y w)
        (* 2 w) (* 2 w))))

  (send target save-file "./day-9/curve.png" 'png)
)