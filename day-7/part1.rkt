#lang racket

(require racket/file)

; Return #t if the two characters lhs and rhs produce a split.
(define (split? lhs rhs)
  (and (char=? lhs #\|) (char=? rhs #\^)))

; Return a list of the positions of the tachyon beam (|) after a move
; and the number of times, it has been split.
; Example:
; > (splits "..|..|..|.." "..^.....^..")
; '(3 1 5 9 7)
; 2
(define (splits lhs rhs)
  (for/fold ([positions '()]
             [splits 0]
             #:result (values (reverse positions) splits))
            ([(clhs i) (in-indexed lhs)]
             [crhs rhs])
    (if (split? clhs crhs)
        (values
         (cons (add1 i) (cons (sub1 i) positions))
         (add1 splits))
        (values
         (if (char=? clhs #\|) (cons i positions) positions)
         splits))))

; Return the updated line lhs after a move and the number of times,
; the tachyon beam has bees split.
(define (update-line lhs rhs)
  (let-values ([(positions splits) (splits lhs rhs)])
    (let ([v-rhs (list->vector (string->list rhs))])
      (for ([pos positions])
        (vector-set! v-rhs pos #\|))
      (values (list->string (vector->list v-rhs)) splits))))

(define (solve lines)
  (define first (string-replace (car lines) "S" "|"))
  (define rest  (cdr lines))

  (printf "~a\n" first)

  (for/fold ([lhs first]
             [splits 0]
             #:result splits)
            ([rhs rest])
    (let-values ([(line new-splits) (update-line lhs rhs)])
      (printf "~a\n" line)
      (values line (+ splits new-splits)))))

(module+ main
  (define lines (file->lines "./day-7/input"))
  (printf "Solution: ~a\n" (solve lines)))
