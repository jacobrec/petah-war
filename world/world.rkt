#lang racket

(require "world_constants.rkt")
(require "units.rkt")
(require "terrain.rkt")
(provide (all-defined-out))

(struct world (grid
               bg-overlay
               status
               units
               width height
               selection
               cur-x cur-y) #:mutable)

(define (make-world width height)
  (define wo (world (make-vector
                      (* width height)
                      TILE_WATER)
                    (make-vector
                      (* width height)
                      DFT)
                    ""                  ; Status
                    (list)
                    width height
                    #f
                    0 0))               ; cur-x cur-y
  (temp-setup wo))

(define (temp-setup w)
  (set-world-units! w (list (unit 25 11 UNIT_INFANTRY)
                            (unit 25 12 UNIT_TANK)
                            (unit 25 10 UNIT_PLANE)
                            (unit 8 6 UNIT_FERRY)
                            (unit 3 2 UNIT_DESTROYER)))
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

(define (check-unit-hover world)
  (set-world-bg-overlay! world (make-vector
                                 (* (world-width world)
                                    (world-height world))
                                 DFT))
  (if (and
        (world-selection world)
        (unit? (world-selection world)))
      (set-move-overlay world (world-selection world) CYN)
      (for ([u (world-units world)])
        (when (and (= (world-cur-x world) (unit-x u))
                   (= (world-cur-y world) (unit-y u)))
          (set-move-overlay world u MAG)))))
(define (set-move-overlay world unit color)
  (define d (unit-range unit))
  (define h (world-height world))
  (define w (world-width world))
  (define overlay (world-bg-overlay world))
  (define (access x y) (+ x (* w y)))
  (define (fill x y range)
    (when (and (>= range 0)
            (< x w) (< y h) (>= x 0) (>= y 0))
      (vector-set! overlay (access x y) color)
      (define (next x y)
        (when (and (> range 0)
                (< x w) (< y h) (>= x 0) (>= y 0))
            (fill x y
                  (- range
                     (terrain-movement-usage
                       (unit-type unit)
                       (vector-ref (world-grid world) (access x y)))))))
      (next (- x 1) y)
      (next (+ 1 x) y)
      (next x (- y 1))
      (next x (+ 1 y))))
  (fill (unit-x unit)
        (unit-y unit)
        (unit-range unit))

  (void))



(define (update-world world)
  (check-unit-hover world)
  (void))
