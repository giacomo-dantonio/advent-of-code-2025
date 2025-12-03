#lang racket

(require racket/file)

(define (char-banks banks)
    (for/list ([bank banks]) (string->list bank)))

(define (char->number c) (- (char->integer c) (char->integer #\0)))

(define (bank->number bank) (string->number (list->string bank)))

(define (max-combination best)
    (let* ([combinations (remove-duplicates (combinations best 11))]
           [combinations (map bank->number combinations)]
           [best (apply max combinations)])
        (string->list (number->string best))))

; Greedy algorithm:
; Start with the last 12 positions of the bank
; and consider the remaining positions from right to left.
; At each iteration, chek if the current position is
; >= than the first position of the guess.
; If so, add it to the guess and remove the position from the guess,
; which results in the maximum value.
(define (max-joltage bank)
    (define (body best buf)
        (let ([head (car buf)]
              [tail (cdr buf)])
            (if (< (char->number head) (char->number (car best)))
                (values best tail)
                ; add head to the maximum 11-combination of best 
                (let ([best (max-combination best)])
                    (values (cons head best) tail)))))

    (define (iter best buf)
        (if (empty? buf)
            best
            (let-values ([(best buf) (body best buf)])
                (iter best buf))))  ; Tail recursion

    (let* ([reverse-bank (reverse bank)]
           [buf-length (- (length bank) 12)]
           [initial (drop bank buf-length)]
           [buf (reverse (take bank buf-length))])
        (iter initial buf)
    )
)

(define (solve banks)
    (define joltages
        (map (lambda (bank) (bank->number (max-joltage bank)))
            (char-banks banks)))
    
    (apply + joltages)
)

(define banks (file->lines "./day-3/input"))
(printf "Solution: ~a\n" (solve banks))
