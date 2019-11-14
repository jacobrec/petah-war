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
                    0 0))               ; cur-x cur-y
  (temp-setup wo))

(define (temp-setup w)
  (set-world-units! w (list (unit 25 10 #\%)))
  (temp-add-island w)
  (temp-add-mountain w 27 11)
  (temp-add-mountain w 26 13)
  (temp-add-mountain w 25 11)
  (temp-add-mountain w 26 12))

(define (temp-add-mountain world x y)
  (vector-set! (world-grid world)
               (+ x (* y (world-width world)))
               TILE_MOUNTAIN)
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
  (for ([u (world-units world)])
    (when (and (= (world-cur-x world) (unit-x u))
               (= (world-cur-y world) (unit-y u)))
      (set-move-overlay world u))))
(define (set-move-overlay world unit)
  (define d (unit-range unit))
  (define h (world-height world))
  (define w (world-width world))
  (define overlay (world-bg-overlay world))
  (define (access x y) (+ x (* w y)))
  (define (fill x y range)
    (when (and (> range 0)
            (< x w) (< y h) (>= x 0) (>= y 0))
      (vector-set! overlay (access x y) BLU)
      (define (next x y)
        (fill x y
              (- range
                 (terrain-movement-usage
                   unit
                   (vector-ref (world-grid world) (access x y))))))
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
