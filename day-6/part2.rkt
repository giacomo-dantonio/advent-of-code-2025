#lang racket

(require racket/file)

; extract the columns of the input file as strings
; e.g. '("1" "24" "356" "" "369" "248" "8" "" "32" "581" "175" "" "623" "431" "4")
(define (extract-columns numlines)
  (let [(cols (map string->list numlines))]
    (map (compose string-trim list->string)
         (apply map list cols))))

; split the column list in lists separated by empty strings,
; that is, one list for each operation.
; e.g. '((356 24 1) (8 248 369) (175 581 32) (4 431 623))
(define (split-columns columns)
  (define-values (params last)
    (for/fold ([acc '()]
               [curr '()])
              ([ns columns])
      (if (string=? ns "")
          (values (cons curr acc) '())
          (values acc (cons (string->number ns) curr)))))
  (reverse (cons last params)))

; parse the operation lines and return a list of functions.
; whitespaces are ignored
(define (parse-ops line)
  (define string-ops (regexp-match* #rx"\\+|\\*" line))
  (define (parse-op ch)
    (if (string=? ch "*") *
        (if (string=? ch "+") + (raise exn:fail))))

  (map parse-op string-ops))

(define (solve lines)
  ; split operations line from the rest of the list
  (define-values (numlines oplines) (split-at-right lines 1))

  (define columns (extract-columns numlines))
  (define params (split-columns columns))

  (define ops (parse-ops (car oplines)))
  (define results
    (for/list ([op ops]
               [param params])
      (apply op param)))

  (apply + results))

(define lines (file->lines "./day-6/input"))
(printf "Solution: ~a\n" (solve lines))
