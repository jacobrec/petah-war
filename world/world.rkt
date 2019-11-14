#lang racket

(require "world_constants.rkt")
(require "units.rkt")
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
  (temp-add-island w))

(define (temp-add-island world)
  (for ([x (range 30)])
    (for ([y (range 15)])
      (vector-set! (world-grid world)
                   (+ 10 x (* (+ y 5) (world-width world)))
                   TILE_GRASS)))
  world)

(define (check-unit-hover world)
  (for ([u (world-units world)])
    (when (and (= (world-cur-x world) (unit-x u))
               (= (world-cur-y world) (unit-y u)))
      (set-world-status! world
                         (string-append "Hover: "
                                        (number->string (unit-range u)))))))
(define (set-move-overlay world unit)
  (define d (unit-range unit))
  (void))



(define (update-world world)
  (check-unit-hover world)
  (void))
