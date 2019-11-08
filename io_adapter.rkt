#lang racket
(require "io/input.rkt")
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
    [(= tile TILE_WATER) (cell DFT BLU #\~)]))

(define (draw-world world)
  ;; Set Buffer data from map
  (for [(y (range (world-height world)))]
    (for [(x (range (world-width world)))]
      (define cell (get-cell-from-tiletype
                     (vector-ref
                       (world-grid world)
                       (+ x (* y (world-width world))))))
      (screen-buffer-set-pixel! sb
        x y
        (cell-fg cell) (cell-bg cell)
        (cell-char cell))))

  ;; Set Buffer data from movables
  (for ([u (world-units world)])
    (screen-buffer-set-pixel! sb
      (unit-x u) (unit-y u)
      BLK RED
      (unit-type u)))


  (draw-buffer sb)
  (reset-color)
  (displayln (world-status world))
  (move-to
    (+ 1 (world-cur-x world)) ; Cursors are weird
    (world-cur-y world))
  (flush-output))

(define (fix-screen)
  (reset-color)
  (displayln "")
  (clear-screen))

;;;; Input
(define (do-input world)
  (case (read-char)
    [(#f)  (set-world-status! world "No Key")]
    [(#\q) (begin (stty "sane") (clear-screen) (exit))]
    [(#\h) (move-cursor world -1 0)]
    [(#\j) (move-cursor world 0 1)]
    [(#\k) (move-cursor world 0 -1)]
    [(#\l) (move-cursor world 1 0)]
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
