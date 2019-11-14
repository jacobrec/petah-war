#lang racket

(require "world_constants.rkt")
(provide (all-defined-out))

(define (terrain-movement-usage unit tile)
  (cond [(= tile TILE_MOUNTAIN) 3]
        [else 1]))
