#lang racket

(require math/array)

(define (positions lines)
  (define shape (vector (length lines) (string-length (car lines))))
  (define chars (string->list (string-join lines "")))

  (for/array: #:shape shape ([char chars]) (if (char=? char #\@) 1 0)))

(define (neighbourhood mat i j)
  (define (array-h A) (vector-ref (array-shape A) 0))
  (define (array-w A) (vector-ref (array-shape A) 1))

  (let ([fromi (max 0 (- i 1))]
        [toi (min (array-h mat) (+ i 2))]
        [fromj (max 0 (- j 1))]
        [toj (min (array-w mat) (+ j 2))])
    (array-slice-ref mat (list (in-range fromi toi) (in-range fromj toj)))))

(define (neighbours mat i j)
  (if (positive? (array-ref mat (vector i j)))
      (- (array-count positive? (neighbourhood mat i j)) 1)
      0))

(define (accessible pos)
  (array-map (lambda (sh)
               (let* ([i (vector-ref sh 0)]
                      [j (vector-ref sh 1)]
                      [n (neighbours pos i j)]
                      [p (array-ref pos (vector i j))])
                 (if (and (< n 4) (> p 0)) 1 0)))
             (indexes-array (array-shape pos))))

(provide positions accessible)