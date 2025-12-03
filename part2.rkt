#lang racket

(require racket/file)

(define (invalid? id)
    (let ([id (number->string id)])
        (define (test id n) (let* ([prefix (substring id 0 n)]
                                [k (quotient (string-length id) (string-length prefix))]
                                [test-string (string-append* (make-list k prefix))])
                                (string=? test-string id)))

        (ormap (lambda (n) (test id n)) (range 1 (+ (quotient (string-length id) 2) 1)))))

(define (invalid-ids ranges)
    (for/fold ([acc '()])
              ([idrange ranges])
        (match idrange
            [(list start end)
             (let ([start (string->number start)]
                   [end (string->number end)])
                (append acc (filter invalid? (range start (+ end 1)))))])))

(define (solve ranges) (apply + (invalid-ids ranges)))

(define db (string-trim (file->string "./day-2/input")))
(define ranges
    (for/list ([range (string-split db ",")]) (string-split range "-")))

(printf "Solution: ~a\n" (solve ranges))
