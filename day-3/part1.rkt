#lang racket

(require racket/file)

(define (numeric-banks banks)
  (for/list ([bank banks])
    (for/list ([battery (string->list bank)])
      (- (char->integer battery) (char->integer #\0)))))

(define (max-joltage bank)
  ; i starts from 0
  (define (test bank i)
    (let* ([suffix (drop bank i)]
           [head (car suffix)]
           [tail (cdr suffix)])
      (+ (* 10 head) (apply max tail))))

  (apply max
         (for/list ([i (range 0 (- (length bank) 1))])
           (test bank i))))

(define (solve banks)
  (apply + (map max-joltage (numeric-banks banks))))

(define banks (file->lines "./day-3/input"))
(printf "Solution: ~a\n" (solve banks))
