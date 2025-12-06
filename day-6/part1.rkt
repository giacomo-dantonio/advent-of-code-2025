#lang racket

(require racket/file)

(define (parse-ops line)
  (define string-ops (regexp-match* #rx"\\+|\\*" line))
  (define (parse-op ch)
    (if (string=? ch "*") *
        (if (string=? ch "+") + (raise exn:fail))))

  (map parse-op string-ops))

(define num-exp (regexp "[0-9]+"))
(define (parse-nums line)
  (define string-nums (regexp-match* num-exp line))
  (map string->number string-nums))

(define (results lines)
  (define-values (numlines oplines) (split-at-right lines 1))
  (define ops (parse-ops (car oplines)))

  (for*/fold ([partials (parse-nums (car numlines))])
             ([line (cdr numlines)])
    (let ([line (parse-nums line)])
      (for/list ([op ops]
                 [lhs partials]
                 [rhs line])
        (op lhs rhs)))))

(define (solve lines)
  (apply + (results lines)))

(define lines (file->lines "./day-6/input"))
(printf "Solution: ~a\n" (solve lines))
