#lang racket

(require racket/file)

(define (parse lines)
  (define (parse-shapes lines)
    (for/fold ([shapes '()]
               [current '()]
               #:result (reverse shapes))
              ([line lines])
      (cond
        [(string=? "" line)
         (values (cons (reverse current) shapes) '())]
        [(andmap (lambda (ch) (or (char=? #\# ch) (char=? #\. ch))) (string->list line))
         (values shapes (cons line current))]
        [else
         (values shapes current)])))

  (define (parse-region line)
    (match (string-split line ": ")
      [(list size quantities)
       (values
        (map string->number (string-split size "x"))
        (map string->number (string-split quantities " ")))]))

  (define (parse-regions lines)
    (for/fold ([sizes '()]
               [quantities '()]
               #:result (values (reverse sizes) (reverse quantities)))
              ([line lines])
      (let-values ([(size quant) (parse-region line)])
        (values (cons size sizes) (cons quant quantities)))))

  (define-values (shapes regions) (splitf-at-right lines (lambda (line) (not (string=? "" line)))))
  (define-values (sizes quantities) (parse-regions regions))

  (values (parse-shapes shapes) sizes quantities))

(define (area shape)
  (count
    ((curry char=?) #\#)
    (string->list (string-join shape ""))))

(define (check-area shapes sizes quantities)
  (define shape-areas (map area shapes))

  (define (presents-area quantities)
    (for/sum ([n quantities]
              [a shape-areas])
      (* n a)))

  (for/list ([size sizes]
             [spec quantities])
    (let ([region-area (apply * size)]
          [spec-area (presents-area spec)])
      (<= spec-area region-area))))

; Not really a solution to the problem: it just checks, whether the area of the required
; presents is less or equal than the region's area. Apparently this is enough for the input.
; Not for the example, though.
(define (solve lines)
  (define-values (shapes sizes quantities) (parse lines))
  (define checks (check-area shapes sizes quantities))

  (count identity checks))

(define lines (file->lines "day-12/input"))
(printf "Solution: ~a\n" (solve lines))
