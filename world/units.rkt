#lang racket
(provide (all-defined-out))

(struct unit (x y type))

(define (unit-range unit)
  (cond
    [(char=? #\%) 3]))


