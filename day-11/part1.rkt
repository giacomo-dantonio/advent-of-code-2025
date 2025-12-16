#lang racket

(require data/queue)
(require racket/file)

(define (parse lines)
  (define result (make-hash))

  (for ([line lines])
    (match (string-split line ": ")
      [(list key values)
        (hash-set! result key (string-split values " "))]))

  result)

; return all the paths between the nodes source and targets
(define (paths edges source target)
  (define q (make-queue))
  (define result (mutable-set))

  (enqueue! q (list source))

  (define (iter)
    (let* ([path (dequeue! q)]
           [last (car path)])
      (if (string=? last target)
        (set-add! result path)
        (for ([next (hash-ref edges last)])
          (enqueue! q (cons next path)))))

    (unless (queue-empty? q) (iter)))

  (iter)
  (map reverse (set->list result)))

(define (solve lines)
  (define edges (parse lines))
  (length (paths edges "you" "out")))

(define lines (file->lines "./day-11/input"))
(printf "Solution: ~a\n" (solve lines))
