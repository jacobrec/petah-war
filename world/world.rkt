#lang racket

(require "world_constants.rkt")
(require "units.rkt")
(require "terrain.rkt")
(provide (all-defined-out))

(struct world (grid
               bg-overlay
               status
               menu
               menuidx
               units
               buildings
               width height
               selection
               cur-x cur-y) #:mutable)

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
        (unit-range unit)))

(define (do-selection world)
  (define selected #f)
  (when (and (world-selection world)
             (unit? (world-selection world)))
    (define x (world-cur-x world))
    (define y (world-cur-y world))
    (define w (world-width world))
    (when (= CYN (vector-ref (world-bg-overlay world) (+ x (* y w))))
      (set-unit-x! (world-selection world) x)
      (set-unit-y! (world-selection world) y))
    (set! selected #t))
  (for ([u (world-units world)])
    (when (and (not selected)
               (= (world-cur-x world) (unit-x u))
               (= (world-cur-y world) (unit-y u)))
      (set-world-selection! world u)
      (set! selected #t))))


(define (update-world world)
  (check-unit-hover world))

(define (incmenu world)
    (define menu (world-menu world))
    (when menu
      (set-world-menuidx!
        world (modulo (add1 (world-menuidx world))
                      (length menu)))))
