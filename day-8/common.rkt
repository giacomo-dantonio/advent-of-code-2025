#lang racket

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

(provide parse-lines distances)
