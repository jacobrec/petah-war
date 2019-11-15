#lang racket

(provide (all-defined-out))
(require "world.rkt")

(struct building (x y type))

(define (building-options unit)
  (define btype (building-type unit))
  (cond
    [(= btype BUILD_HQ) #f]
    [(= btype BUILD_MONEY) #f]
    [(= btype BUILD_FACTORY) '("tank" "infantry" "helicopter" "bomber" "plane")]
    [(= btype BUILD_SEAFACTORY) '("destroyer" "ferry" "battleship")]
    [(= btype BUILD_COVER) #f]
    [else #f]))

(define (building-do world unit option)
  #f)
