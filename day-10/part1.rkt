#lang racket

(require racket/file)
(require data/queue)

(define (parse-lights str)
  (substring str 1 (sub1 (string-length str))))

(define (parse-tuple str)
  (let* ([inner (substring str 1 (sub1 (string-length str)))]
         [parts (string-split inner ",")])
    (map string->number parts)))

(define (parse lines)
  (for/list ([line lines])
    (let* ([line (string-split line " ")]
           [lights (parse-lights (car line))]
           [buttons (map parse-tuple (drop-right (cdr line) 1))])
      (list lights buttons))))

(define (press-button state button)
  (let ([state (string-copy state)])
    (for ([pos button])
      (string-set! state pos (if (char=? (string-ref state pos) #\.) #\# #\.)))
    state))

(define (solve-machine lights buttons)
  (define q (make-queue))
  (define visited (mutable-set))

  ; BFS queue. The elements are pairs of (state, buttons pressed to reach the state)
  (enqueue! q (list (make-string (string-length lights) #\.) '()))

  (define (iter)
    (let* ([current (dequeue! q)]
           [state (car current)]
           [moves (cadr current)]
           [done (string=? state lights)])
      (set-add! visited state)
      (if done
        moves
        (begin
          ; for each button: add button to moves,
          ; compute next state and add them to queue
          (for ([(button i) (in-indexed buttons)])
            (let ([next-moves (cons i moves)]
                  [next-state (press-button state button)])
              (unless (set-member? visited next-state)
                (enqueue! q (list next-state next-moves)))))
          (iter))
      )))

  (iter))

(define (solve lines)
  (define solutions
    (for/list ([line (parse lines)])
      (let* ([lights (car line)]
             [buttons (cadr line)]
             [solution (solve-machine lights buttons)])
        (printf "~a\t~a\t~a\n" lights solution buttons)
        solution)))
  (apply + (map length solutions)))

(module+ main
  (define lines (file->lines "./day-10/input"))
  (printf "Solution: ~a\n" (solve lines)))
