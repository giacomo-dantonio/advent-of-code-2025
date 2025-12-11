#lang racket

(define (parse lines)
  (for/list ([line lines]) (map string->number (string-split line ","))))

(provide parse)
