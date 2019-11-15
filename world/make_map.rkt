#lang racket

(require "types.rkt")
(require "game.rkt")
(require "units.rkt")
(require "buildings.rkt")
(require "world.rkt")
(provide make-world)

(define (make-world width height id)
  (define wo (world (make-vector
                      (* width height)
                      TILE_WATER)
                    (make-vector
                      (* width height)
                      DFT)
                    ""                  ; Status
                    #f 0
                    (list)
                    (list)
                    width height
                    #f
                    3000 ; money
                    id ; player id
                    #f ; direction state
                    0 0))               ; cur-x cur-y
  (temp-setup wo))


(define (temp-setup w)
  (set-world-units! w (list (unit 25 11 2 UNIT_INFANTRY)
                            (unit 25 12 2 UNIT_TANK)
                            (unit 25 10 2 UNIT_PLANE)
                            (unit 11 8 2 UNIT_INFANTRY)
                            (unit 8 6 2 UNIT_FERRY)
                            (unit 3 2 2 UNIT_DESTROYER)))
  (set-world-buildings! w (list (building 10 10 1 BUILD_HQ)
                                (building 13 13 2 BUILD_HQ)
                                (building 14 14 1 BUILD_MONEY)
                                (building 15 15 2 BUILD_MONEY)
                                (building 11 11 1 BUILD_FACTORY)
                                (building 11 12 2 BUILD_FACTORY)
                                (building 9 12 1 BUILD_SEAFACTORY)))
  (temp-add-island w)
  (temp-add-tile-set w
                 '((26 . 11)
                   (27 . 11)
                   (27 . 12)
                   (28 . 12)
                   (28 . 13)
                   (26 . 10))
                 TILE_MOUNTAIN)
  (temp-add-tile-set w
                 '((24 . 11)
                   (23 . 11)
                   (23 . 12)
                   (22 . 12)
                   (22 . 13)
                   (24 . 10))
                 TILE_FOREST)
  (temp-add-tile-set w
                 '((25 . 10)
                   (25 . 11)
                   (25 . 12)
                   (25 . 13)
                   (25 . 14)
                   (25 . 15)
                   (25 . 16))
                 TILE_ROAD))

(define (temp-add-tile-set world items tile)
  (for ([item items])
    (temp-add-tile world (car item) (cdr item) tile))
  world)

(define (temp-add-tile world x y tile)
  (vector-set! (world-grid world)
               (+ x (* y (world-width world)))
               tile)
  world)

(define (temp-add-island world)
  (for ([x (range 30)])
    (for ([y (range 15)])
      (vector-set! (world-grid world)
                   (+ 10 x (* (+ y 5) (world-width world)))
                   TILE_GRASS)))
  world)
