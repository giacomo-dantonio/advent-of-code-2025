#lang racket

(require racket/file)

(define (parse lines)
  (make-immutable-hash (for/list ([line lines])
    (match (string-split line ": ")
      [(list key values)
        (cons key (string-split values " "))]))))

; graph is an hash map mapping node names
; to the list of their adjacent nodes.
(define (topological-sort graph)
  (define (root? graph node)
    (andmap
      (lambda (adj-nodes) (not (member node adj-nodes)))
      (hash-values graph)))

  (let loop ([graph graph]
             [result '()])
    (if (hash-empty? graph)
      (reverse result)
      (let* ([nodes (hash-keys graph)]
           [node (for/first ([node nodes] #:when (root? graph node)) node)])
      (loop (hash-remove graph node) (cons node result))))))

(define (count-paths graph sorted-nodes source target)
  ; Drop the node before source, as they are definitely not
  ; reachable from source.
  (define nodes (dropf sorted-nodes (lambda (node) (not (string=? node source)))))

  ; Since the nodes are sorted topologically it sufficies to
  ; iterate over the list once.
  (define counts
    (for/fold ([result (hash-set (make-immutable-hash) source 1)])
              ([node nodes])
      (let ([value (hash-ref result node 0)])
        (for/fold ([result result])
                  ([next-node (hash-ref graph node)])
          (hash-set result next-node (+ value (hash-ref result next-node 0)))))))
  
  (hash-ref counts target))

(define (solve lines)
  (define graph (parse lines))
  (define sorted-nodes (topological-sort graph))

  ; We know that fft always comes before dac
  (let ([svr-to-fft (count-paths graph sorted-nodes "svr" "fft")]
        [fft-to-dac (count-paths graph sorted-nodes "fft" "dac")]
        [dac-to-out (count-paths graph sorted-nodes "dac" "out")])
    (* svr-to-fft fft-to-dac dac-to-out)))

(module+ main
  (define lines (file->lines "./day-11/input"))
  (printf "Solution: ~a\n" (solve lines)))
