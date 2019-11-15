#lang racket
(require "io/printing/buffer.rkt")
(require "io/printing/printer.rkt")
(require "io/input.rkt")
(require "io/prompt.rkt")
(require "world/types.rkt")
(require "world/game.rkt")
(require "world/units.rkt")
(require "world/buildings.rkt")
(require "world/world.rkt")
(provide (all-defined-out))

#|

This file is to contain all the code to adapt between the world objects and
the rendering io code. It serves

|#


;;;; Graphics
(define sb (make-buffer 50 25))
(define (get-cell-from-tiletype tile)
  (cond
    [(= tile TILE_GRASS) (cell DFT GRN #\")]
    [(= tile TILE_FOREST) (cell BLK GRN #\%)]
    [(= tile TILE_WATER) (cell DFT BLU #\~)]
    [(= tile TILE_MOUNTAIN) (cell DFT BLK #\^)]
    [(= tile TILE_ROAD) (cell DFT BLK #\╬)]))   ; TODO: make these actually look good

(define (get-cell-from-unittype unit team)
  (define bg (get-color-from-player-id team))
  (cond
    [(= unit UNIT_INFANTRY) (cell DFT bg #\I)]
    [(= unit UNIT_TANK) (cell DFT bg #\T)]

    [(= unit UNIT_DESTROYER) (cell DFT bg #\D)]
    [(= unit UNIT_BATTLESHIP) (cell BLK bg #\S)]
    [(= unit UNIT_FERRY) (cell DFT bg #\F)]

    [(= unit UNIT_PLANE) (cell DFT bg #\P)]
    [(= unit UNIT_BOMBER) (cell DFT bg #\B)]
    [(= unit UNIT_HELICOPTER) (cell DFT bg #\H)]))

(define (get-cell-from-buildingtype bld team)
  (define bg team)
  (cond
    [(= bld BUILD_HQ) (cell DFT bg #\@)]
    [(= bld BUILD_MONEY) (cell DFT bg #\$)]
    [(= bld BUILD_FACTORY) (cell DFT bg #\#)]
    [(= bld BUILD_SEAFACTORY) (cell DFT bg #\#)]
    [(= bld BUILD_COVER) (cell DFT bg #\&)]))

(define (get-color-from-player-id id)
  (case id
    ((0) BLK)
    ((1) RED)
    ((2) YEL)
    ((3) GRN)
    ((4) BLU)))

(define (overlay-unit world u)
  (define cell
    (get-cell-from-unittype (unit-type u)
                            (unit-owner-id u)))
  (define fg (cell-fg cell))
  (define bg (cell-bg cell))
  (define char (cell-char cell))
  (define dbg (cell-bg (get-cell-from-tiletype
                         (vector-ref
                           (world-grid world)
                           (+ (unit-x u) (* (unit-y u) (world-width world)))))))
  (when (unit-has-moved u)
    (set! fg bg)
    (set! bg DFT))
  (screen-buffer-set-pixel! sb
    (unit-x u) (unit-y u)
    fg (if (= DFT bg) dbg bg) char))

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

  ;; Set Buffer data from buildings
  (for ([u (world-buildings world)])
    (define cell
      (get-cell-from-buildingtype
        (building-type u)
        (get-color-from-player-id (building-owner-id u))))
    (screen-buffer-set-pixel! sb
      (building-x u) (building-y u)
      (cell-fg cell) (cell-bg cell)
      (cell-char cell)))

  ;; Set Buffer data from movables
  (for ([u (world-units world)])
    (overlay-unit world u))

  (cursor-set #f)
  (draw-buffer sb)
  (reset-color)
  (clear-line)
  (printf "$~a\n" (world-money world))
  (clear-line)
  (displayln (world-status world))
  (if (world-menu world)
    (render-choices
      (world-menu world)
      (world-menuidx world))
    (clear-rest))
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
(define (do-input world brk)
  (define c (read-char))
  (case c
    [(#f) (set-world-status! world "No Key")]
    [(#\q) (begin (stty "sane") (end-screen) (exit))]
    [(#\h #\j #\k #\l) (if (world-menu world)
                         (move-menu world c)
                         (move-world world c))]
    [(#\c) (set-world-selection! world #f)
           (set-world-menu! world #f)]
    [(#\e) (brk)]
    [(#\tab) (incmenu world)]
    [(#\return #\space) (do-option-or-selection world)]
    [else (set-world-status! world "Not a keybinding")]))

(define (do-option-or-selection world)
  (if (world-menu world)
      (do-option world)
      (do-selection world)))
(define (move-menu world char)
  (case char
    [(#\j) (incmenu world)]
    [(#\k) (decmenu world)]
    [else #t]))

(define (move-world world char)
  (case char
    [(#\h) (move-cursor world -1 0)]
    [(#\j) (move-cursor world 0 1)]
    [(#\k) (move-cursor world 0 -1)]
    [(#\l) (move-cursor world 1 0)]
    [else #t]))

(define (move-cursor world x y)
  (set-world-cur-x! world (max 0 (+ x (world-cur-x world))))
  (set-world-cur-y! world (max 0 (+ y (world-cur-y world)))))

(define-struct (quit-exception exn:fail:user) ())
(define (start-input-loop world)
  (call/cc
    (lambda (brk)
      (define (loop)
        (do-input world brk)
        (loop))
      (loop))))
