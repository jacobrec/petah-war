#lang racket

(provide (all-defined-out))
(require "world.rkt")
(require "types.rkt")

(define (spawn-unit world x y type cost)
  (when (>= (world-money world) cost)
    (set-world-money! world (- (world-money world) cost))
    (set-world-units! world
      (cons (unit x y (world-player-id world) type)
            (world-units world)))))


(define (building-options unit)
  (define btype (building-type unit))
  (cond
    [(= btype BUILD_HQ) #f]
    [(= btype BUILD_MONEY) #f]
    [(= btype BUILD_FACTORY) '("tank ($500)" "infantry ($100)"
                               "helicopter ($1000)" "bomber ($2000)" "plane ($1500)")]
    [(= btype BUILD_SEAFACTORY) '("destroyer ($1000)" "ferry ($1000)" "battleship ($1000)")]
    [(= btype BUILD_COVER) #f]
    [else #f]))

(define (building-do world bld option)
  (define btype (building-type bld))
  (cond
    [(= btype BUILD_HQ) #f]
    [(= btype BUILD_MONEY) #f]
    [(= btype BUILD_FACTORY)
     (case option
       [(0) (spawn-unit world (building-x bld) (building-y bld) UNIT_TANK 500)]
       [(1) (spawn-unit world (building-x bld) (building-y bld) UNIT_INFANTRY 100)]
       [(2) (spawn-unit world (building-x bld) (building-y bld) UNIT_HELICOPTER 1000)]
       [(3) (spawn-unit world (building-x bld) (building-y bld) UNIT_BOMBER 2000)]
       [(4) (spawn-unit world (building-x bld) (building-y bld) UNIT_PLANE 1500)])]
    [(= btype BUILD_SEAFACTORY)
     (case option
       [(0) (spawn-unit world (building-x bld) (building-y bld) UNIT_DESTROYER 1000)]
       [(1) (spawn-unit world (building-x bld) (building-y bld) UNIT_FERRY 1000)]
       [(2) (spawn-unit world (building-x bld) (building-y bld) UNIT_BATTLESHIP 1000)])]
    [(= btype BUILD_COVER) #f]
    [else #f])
  (set-world-selection! world #f)
  (set-world-menu! world #f))
