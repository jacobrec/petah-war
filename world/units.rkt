#lang racket
(require "world_constants.rkt")
(provide (all-defined-out))

(struct unit (x y type))

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
    [(= tunit UNIT_INFANTRY) '("attack" "wait")]
    [(= tunit UNIT_TANK) '("attack" "wait")]

    [(= tunit UNIT_PLANE) '("attack" "wait")]
    [(= tunit UNIT_BOMBER) '("attack" "wait")]
    [(= tunit UNIT_HELICOPTER) '("attack" "wait")]

    [(= tunit UNIT_DESTROYER) '("attack" "wait")]
    [(= tunit UNIT_FERRY) '("attack" "wait")]
    [(= tunit UNIT_BATTLESHIP) '("attack" "wait")]
    [else #f]))

(define (unit-do world unit option)
  #f)
