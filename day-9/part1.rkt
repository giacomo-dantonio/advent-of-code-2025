#lang racket

(require racket/file)

(define (parse lines)
  (for/list ([line lines]) (map string->number (string-split line ","))))

(define (boxes points)
  (for*/list ([(p i) (in-indexed points)]
              [(q j) (in-indexed points)]
              #:when (< i j))
    (match (list p q)
      [(list (list px py) (list qx qy))
       (let ([minx (min px qx)]
             [maxx (max px qx)]
             [miny (min py qy)]
             [maxy (max py qy)])
         (list (list minx miny) (list maxx maxy)))])))

(define (area box)
  (match box
    [(list (list px py) (list qx qy))
     (* (add1 (- qx px)) (add1 (- qy py)))]))

(define (solve lines)
  (define points (parse lines))

  (apply max
         (for/list ([box (boxes points)])
           (area box))))

(define lines (file->lines "./day-9/input"))
(printf "Solution: ~a\n" (solve lines))
