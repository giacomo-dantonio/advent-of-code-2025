#lang racket

(require racket/file)

; Makes an invalid number by doubling the input e.g. 123 -> 123123
(define (candidate n)
    (let* ([n (number->string n)]
           [double (string-append n n)])
        (string->number double)))

(define (in-range? start end candidate)
    (and (>= candidate start) (<= candidate end)))

; Generate all invalid ids from a range
; e.g. (11 15) -> (1111 1212 1313 1414 1515)
(define (candidates from to)
    (for/fold ([result '()])
              ([n (range from (+ to 1))])
        (cons (candidate n) result)))

; Return the left part of an id e.g. "1234" -> "12", "123" -> "1"
(define (left-half id roundup)
    (let* ([n (string-length id)]
           [k (if roundup (ceiling (/ n 2)) (quotient n 2))])
        (or (string->number (substring id 0 k)) 0)))

(define (invalids ranges)
    (for/fold ([result '()])
            ([range ranges])
        (match range
            [(list start end)
            (let* ([from (left-half start #f)]
                    [to (left-half end #t)]
                    [start (string->number start)]
                    [end (string->number end)]
                    [candidates (candidates from to)])
                (append result (filter (lambda (x) (in-range? start end x)) candidates)))])))

(define (solve ranges) (apply + (invalids ranges)))

(define db (string-trim (file->string "./day-2/input")))
(define ranges
    (for/list ([range (string-split db ",")]) (string-split range "-")))

(printf "Solution: ~a\n" (solve ranges))