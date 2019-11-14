#lang racket

(require "world_constants.rkt")
(provide (all-defined-out))

(define (terrain-movement-usage unit tile)
  (define NO 1000000)
  (if (or (= unit UNIT_PLANE)
          (= unit UNIT_BOMBER)
          (= unit UNIT_HELICOPTER))
    1
    (if (or (= unit UNIT_FERRY)
            (= unit UNIT_BATTLESHIP)
            (= unit UNIT_FERRY))
        (if (= TILE_WATER tile) 1 NO)
        (cond [(= tile TILE_MOUNTAIN) 6]
              [(= tile TILE_FOREST) 4]
              [(= tile TILE_ROAD) 1]
              [else 2]))))
