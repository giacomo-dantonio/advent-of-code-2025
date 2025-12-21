#lang racket

(require data/union-find)
(require racket/draw)
(require racket/file)
(require "common.rkt")

; Ugly montecarlo-like solution:
; After plotting the lines I could see that the lines form a closed curve.
; I now generate a cloud of random points and keep only the points outside the curve.
; Then I iterate over all possible triangles and keep only those
; who do not contain any point from the outside cloud.

; Return #t if an edge should be created between two points.
; This is the case when the points are on the same row or on the same column.
(define (edge? p q) (ormap = p q))

; Compute all the edges between the points.
; This generate some artifacts when more than 2 points are colinear.
; This is the case only for 4 points on the right of the curve.
; This doesn't affect the result, so we ignore the problem.
(define (make-edges points)
  (for*/list ([(p i) (in-indexed points)]
              [(q j) (in-indexed points)]
              #:when (and (< i j) (edge? p q)))
    (list p q)))

; Compute the number of connected components of a set of edges
(define (connected-components edges)
  (let* ([edges (list->vector edges)]
         [ufs (vector-map uf-new edges)])
    (for ([c (combinations (range 0 (vector-length edges)) 2)])
      (let* ([i (car c)]
             [j (cadr c)]
             [ei (vector-ref edges i)]
             [ej (vector-ref edges j)]
             [vertices (list->set (append ei ej))])
        (when (< (set-count vertices) 4)
          (uf-union! (vector-ref ufs i) (vector-ref ufs j)))))

    (set-count (list->set (map uf-find (vector->list ufs))))))

; Return the edges that intersect the ray from p to the left border
(define (intersections edges p)
  (define (intersects-x? edge p)
    (let ([px (car p)]
          [py (cadr p)]
          [ex1 (caar edge)]
          [ex2 (caadr edge)]
          [ey1 (cadar edge)]
          [ey2 (cadadr edge)])
      (and (>= px (min ex1 ex2))
           (>= py (min ey1 ey2))
           (<= py (max ey1 ey2)))))

  (for/list ([edge edges]
             #:when (intersects-x? edge p))
    edge))

; A point is inside the curve if the ray from it to the left border
; intersects the curve an odd number of times.
(define (inside? edges p)
  (odd? (connected-components (intersections edges p))))

(define (inside-rectangle? rectangle point)
  (match rectangle
    [(list (list px py) (list qx qy))
     (let ([fromx (min px qx)]  [fromy (min py qy)]
           [tox (max px qx)]    [toy (max py qy)]
           [pointx (car point)] [pointy (cadr point)])
       (and
        (>= pointx fromx) (<= pointx tox)
        (>= pointy fromy) (<= pointy toy)))]))

(define (rectangle-area rectangle)
  (match rectangle
    [(list (list px py) (list qx qy))
     (* (add1 (abs (- qx px))) (add1 (abs (- qy py))))]))

; Sample points outside of the curve
(define (sample-outside edges N)
  (define (random-points) (for/list ([_ (range 0 N)])
                            (list (random 100000) (random 100000))))

  (filter (lambda (p) (not (inside? edges p))) (random-points)))

(define (rectangles points)
  (for/list ([c (combinations points 2)])
    (let ([p (car c)]
          [q (cadr c)])
      (list p q))))

(define (draw points)
  (define target (make-bitmap 1000 1000))
  (define dc (new bitmap-dc% [bitmap target]))

  (define (scale coordinate) (quotient coordinate 100))

  (define (draw-edge edge)
    (match edge [(list p q)
                 (send dc draw-line
                       (scale (car p)) (scale (cadr p))
                       (scale (car q)) (scale (cadr q)))]))

  (define (draw-point p)
    (let ([x (scale (car p))]
          [y (scale (cadr p))]
          [w 2])
      (send dc draw-rectangle
            (- x w) (- y w)
            (* 2 w) (* 2 w))))

  (define (draw-r r)
    (let* ([p (car r)]
           [q (cadr r)]
           [fromx (min (car p) (car q))]
           [fromy (min (cadr p) (cadr q))]
           [w (abs (- (car p) (car q)))]
           [h (abs (- (cadr p) (cadr q)))])
      (send dc draw-rectangle
            (scale fromx) (scale fromy)
            (scale w) (scale h))))

  (define edges (make-edges points))
  (for ([edge edges]) (draw-edge edge))

  (define outside-points (sample-outside edges 100000))
  (for ([p outside-points]) (draw-point p))

  (define inside-rects
    (for/list ([r (rectangles points)]
               #:when (andmap
                       (lambda (p) (not (inside-rectangle? r p)))
                       outside-points))
      r))

  (send dc set-brush "red" 'solid)
  (send dc set-pen "red" 1 'solid)

  (let ([largest (argmax rectangle-area inside-rects)])
    (draw-r largest)
    (send target save-file "./day-9/curve.png" 'png)

    (rectangle-area largest)))

(define (solve lines)
  (define points (parse lines))
  (draw points))

(module+ main
  (define lines (file->lines "./day-9/input"))
  (printf "Solution: ~a\n" (solve lines)))
