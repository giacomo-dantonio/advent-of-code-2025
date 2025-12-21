#lang racket

(require glpk)
(require racket/file)

; Solve each line as a Mixed Integer Programming problem with GLPK

(define (parse lines)
  (define (parse-tuple str)
    (let* ([inner (substring str 1 (sub1 (string-length str)))]
          [parts (string-split inner ",")])
      (map string->number parts)))

  (for/list ([line lines])
    (let* ([line (string-split line " ")]
           [tuples (map parse-tuple (cdr line))]
           [joltages (last tuples)]
           [buttons (drop-right tuples 1)])
      (list joltages buttons))))

; Problem definition for GLPK
; Return variables, objective, constrains, and boundaries as expected from glpk
(define (make-problem target buttons)
  (define variables (list->vector (map (lambda (_) (gensym)) buttons)))

  (define objective (cons 0 (map (lambda (sym) (list 1 sym)) (vector->list variables))))
  
  ; constraint für eine machine (number from 0 to (length target))
  (define (make-constraint machine)
    (cons (gensym)
          (for/list ([(button i) (in-indexed buttons)]
                     #:when (ormap (lambda (x) (= x machine)) button))
            (let ([sym (vector-ref variables i)]) (list 1 sym)))))

  (define constraints (map make-constraint (range 0 (length target))))

  (define max-target (apply max target))

  (define boundaries (append
    ; Machine constraints: set the joltages values to their targeta
    (for/list ([con constraints]
                [val target])
    (list (car con) val val))

    ; button constraints
    (map (lambda (sym) (list sym 0 max-target)) (vector->list variables))
  ))


  (values (vector->list variables) objective constraints boundaries))

; Solve one line using MIP. Return only the objective value.
(define (solve-machine target buttons)
  (define-values (variables objective constraints boundaries)
    (make-problem target buttons))
  
  (define solution (mip-solve objective 'min constraints boundaries variables))

  (unless (eq? (car solution) 'good)
    (printf "~a\n" solution)
    (raise "Could not solve problem"))

  ; optimal objective value
  (caaddr solution))


(define (solve lines)
  (define solutions
    (for/list ([line (parse lines)])
      (let* ([target (car line)]
             [buttons (cadr line)]
             [solution (solve-machine target buttons)])
        solution)))
        
  (apply + solutions))

(module+ main
  (define lines (file->lines "./day-10/input"))
  (printf "Solution: ~a\n" (solve lines)))
