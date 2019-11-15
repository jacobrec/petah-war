#lang racket
(require "world.rkt")
(require "types.rkt")
(provide (all-defined-out))

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

(define (unit-options-symbols unit)
  (define tunit (unit-type unit))
  (cond
    [(= tunit UNIT_INFANTRY) '(attack capture wait)]
    [(= tunit UNIT_TANK) '(attack wait)]

    [(= tunit UNIT_PLANE) '(attack wait)]
    [(= tunit UNIT_BOMBER) '(attack wait)]
    [(= tunit UNIT_HELICOPTER) '(rescue drop wait)]

    [(= tunit UNIT_DESTROYER) '(attack wait)]
    [(= tunit UNIT_FERRY) '(rescue drop wait)]
    [(= tunit UNIT_BATTLESHIP) '(attack wait)]
    [else #f]))

(define (unit-options unit)
  (map symbol->string (unit-options-symbols unit)))

(define (option-menu-pick->option-symbol unit option)
  (list-ref (unit-options-symbols unit) option))


(define (unit-do world unit option)
  (define opt (option-menu-pick->option-symbol unit option))
  (define x (world-cur-x world))
  (define y (world-cur-y world))
  (when (eq? opt 'wait)
    (set-unit-has-moved! unit #t)
    (set-unit-x! unit x)
    (set-unit-y! unit y)
    (set-world-selection! world #f)
    (set-world-menu! world #f))
  (when (eq? opt 'capture)
    (define done #f)
    (for ([bld (world-buildings world)]) #:break done
      (when (and (= (building-x bld) (world-cur-x world))
                 (= (building-y bld) (world-cur-y world)))
        (set! done #t)
        (set-building-owner-id! bld (unit-owner-id unit))))
    (set-world-selection! world #f)
    (set-world-menu! world #f))
  (when (eq? opt 'attack)
    (set-world-directional-select! world 'left)))

(define (unit-do-direction world unit option)
  (define opt (option-menu-pick->option-symbol unit option))
  (set-world-status! world (symbol->string opt))
  (define x (world-cur-x world))
  (define y (world-cur-y world))
  (when (eq? opt 'attack)
    (set-world-status! world (symbol->string (world-directional-select world)))
    (set-world-selection! world #f)
    (set-world-menu! world #f)
    (set-world-directional-select! world #f)))








