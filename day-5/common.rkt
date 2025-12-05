#lang racket

(define (parse input)
  (define (parse-ranges lines)
    ; parse ranges to a list of pairs, sorted by their lowest extremes
    (define (cmp lhs rhs) (< (car lhs) (car rhs)))
    (define ranges (sort
                    (for/list ([line lines])
                      (match (string-split line "-")
                        [(list from to)
                         (list (string->number from) (string->number to))]))
                    cmp))

    ; merge overlapping ranges
    (reverse
     (for/fold ([acc (take ranges 1)])
               ([range (cdr ranges)])
       ; Check if overlap, if so merge the ranges
       (let ([lfrom (caar acc)]
             [lto (cadar acc)]
             [rfrom (car range)]
             [rto (cadr range)])
         (if (<= rfrom lto)
             ; merge the ranges
             (cons (list (min lfrom rfrom) (max lto rto))
                   (cdr acc))
             (cons range acc)
             )))))

  (define parts (string-split input "\n\n"))

  (define ranges (parse-ranges (string-split (car parts) "\n")))

  (define ids
    (sort
     (map string->number (string-split (cadr parts) "\n"))
     <))

  (values ranges ids))

(provide parse)