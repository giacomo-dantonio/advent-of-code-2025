#lang racket

(require glpk)

; (lp-solve
;  '(0 (1 a) (1 k) (1 cb) (1 cp) (1 ck))
;  'max
;  '((fb (1 a) (2 k) (1 cb))
;    (fp (1 a) (1 k) (1 cp))
;    (fk (2 a) (1 ck)))
;  '((fb 0 30)
;    (fp 0 20)
;    (fk 0 50)
;    (a 0 posinf)
;    (k 0 posinf)
;    (cb 0 posinf)
;    (cp 0 posinf)
;    (ck 0 posinf)))


; (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
;  a    b    c    d     e     f

(define objective '(0 (1 a) (1 b) (1 c) (1 d) (1 e) (1 f)))
(define constraints '(
  (m0 (1 f) (1 e))
  (m1 (1 b) (1 f))
  (m2 (1 c) (1 d) (1 e))
  (m3 (1 a) (1 b) (1 d))))
(define boundaries '(
  (m0 3 3)
  (m1 5 5)
  (m2 4 4)
  (m3 7 7)
  (a 0 7)
  (b 0 7)
  (c 0 7)
  (d 0 7)
  (e 0 7)
  (f 0 7)))

(define integer-vars '(a b c d e f))

(printf "LP solution: ~a\n"(lp-solve objective 'min constraints boundaries))
(printf "MIP solution: ~a\n"(mip-solve objective 'min constraints boundaries integer-vars))
