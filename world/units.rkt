#lang racket
(require "world.rkt")
(provide (all-defined-out))

(struct unit
  (x y owner-id type [has-moved #:auto])
  #:mutable #:auto-value #f)

(define (unit-range unit)
  (define tunit (unit-type unit))
  (cond
    [(= tunit UNIT_INFANTRY) 8]
    [(= tunit UNIT_TANK) 10]

    [(= tunit UNIT_PLANE) 7]
    [(= tunit UNIT_BOMBER) 5]
    [(= tunit UNIT_HELICOPTER) 6]

    [(= tunit UNIT_DESTROYER) 5]
    [(= tunit UNIT_FERRY) 6]
    [(= tunit UNIT_BATTLESHIP) 3]
    [else 0]))

(define (unit-options unit)
  (define tunit (unit-type unit))
  (cond
    [(= tunit UNIT_INFANTRY) '("attack" "capture" "wait")]
    [(= tunit UNIT_TANK) '("attack" "wait")]

    [(= tunit UNIT_PLANE) '("attack" "wait")]
    [(= tunit UNIT_BOMBER) '("attack" "wait")]
    [(= tunit UNIT_HELICOPTER) '("rescue" "drop" "wait")]

    [(= tunit UNIT_DESTROYER) '("attack" "wait")]
    [(= tunit UNIT_FERRY) '("rescue" "drop" "wait")]
    [(= tunit UNIT_BATTLESHIP) '("attack" "wait")]
    [else #f]))

(define (unit-do world unit option)
  (define x (world-cur-x world))
  (define y (world-cur-y world))
  (when (= option 1)
    (set-unit-has-moved! unit #t)
    (set-unit-x! unit x)
    (set-unit-y! unit y))
  #f)
