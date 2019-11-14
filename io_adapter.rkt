#lang racket
(require "io/input.rkt")
(require "io/prompt.rkt")
(require "world/units.rkt")
(require "world/world_constants.rkt")
(provide (all-defined-out))

#|

This file is to contain all the code to adapt between the world objects and
the rendering io code. It serves

|#

(require "world/world.rkt")
(require "world/world_constants.rkt")
(require "io/printing/buffer.rkt")
(require "io/printing/printer.rkt")

;;;; Graphics
(define sb (make-buffer 50 25))
(define (get-cell-from-tiletype tile)
  (cond
    [(= tile TILE_GRASS) (cell DFT GRN #\")]
    [(= tile TILE_FOREST) (cell BLK GRN #\%)]
    [(= tile TILE_WATER) (cell DFT BLU #\~)]
    [(= tile TILE_MOUNTAIN) (cell DFT BLK #\^)]
    [(= tile TILE_ROAD) (cell DFT YEL #\.)]))

(define (get-cell-from-unittype unit team)
  (define bg team)
  (cond
    [(= unit UNIT_INFANTRY) (cell DFT bg #\I)]
    [(= unit UNIT_TANK) (cell DFT bg #\T)]

    [(= unit UNIT_DESTROYER) (cell DFT bg #\D)]
    [(= unit UNIT_BATTLESHIP) (cell BLK bg #\S)]
    [(= unit UNIT_FERRY) (cell DFT bg #\F)]

    [(= unit UNIT_PLANE) (cell DFT bg #\P)]
    [(= unit UNIT_BOMBER) (cell DFT bg #\B)]
    [(= unit UNIT_HELICOPTER) (cell DFT bg #\H)]))

(define (draw-world world)
  ;; Set Buffer data from map
  (for [(y (range (world-height world)))]
    (for [(x (range (world-width world)))]
      (define bg-overlay (vector-ref
                           (world-bg-overlay world)
                           (+ x (* y (world-width world)))))
      (define cell (get-cell-from-tiletype
                     (vector-ref
                       (world-grid world)
                       (+ x (* y (world-width world))))))
      (screen-buffer-set-pixel! sb x y
        (cell-fg cell) (if (= DFT bg-overlay)
                         (cell-bg cell)
                         bg-overlay)
        (cell-char cell))))

  ;; Set Buffer data from movables
  (for ([u (world-units world)])
    (define cell (get-cell-from-unittype (unit-type u) RED))
    (screen-buffer-set-pixel! sb
      (unit-x u) (unit-y u)
      (cell-fg cell) (cell-bg cell)
      (cell-char cell)))

  (cursor-set #f)
  (draw-buffer sb)
  (reset-color)
  (clear-line)
  (displayln (world-status world))
  (when (world-menu world)
    (render-choices
      (world-menu world)
      (world-menuidx world)))
  (move-to
    (+ 1 (world-cur-x world)) ; Cursors are weird
    (world-cur-y world))
  (cursor-set #t)
  (flush-output))

(define (start-screen)
  (save-cursor)
  (alternate-screen #t)
  (reset-color)
  (clear-screen))

(define (end-screen)
  (alternate-screen #f)
  (reset-color)
  (restore-cursor))

;;;; Input
(define (do-input world)
  (case (read-char)
    [(#f)  (set-world-status! world "No Key")]
    [(#\q) (begin (stty "sane") (end-screen) (exit))]
    [(#\h) (move-cursor world -1 0)]
    [(#\j) (move-cursor world 0 1)]
    [(#\k) (move-cursor world 0 -1)]
    [(#\l) (move-cursor world 1 0)]
    [(#\tab) (incmenu world)]
    [(#\space #\return) (set-world-status! world "Selected")]
    [else (set-world-status! world "Not a keybinding")]))

(define (move-cursor world x y)
  (set-world-cur-x! world (max 0 (+ x (world-cur-x world))))
  (set-world-cur-y! world (max 0 (+ y (world-cur-y world)))))

(define-struct (quit-exception exn:fail:user) ())
(define (start-input-loop world)
  (define (loop)
    (do-input world)
    (loop))
  (with-raw
    (loop)))
