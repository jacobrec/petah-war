#lang racket

(require "world_constants.rkt")
(provide (all-defined-out))

(define (terrain-movement-usage unit tile)
  (cond [(= tile TILE_MOUNTAIN) 6]
        [(= tile TILE_FOREST) 4]
        [(= tile TILE_ROAD) 1]
        [else 2]))
