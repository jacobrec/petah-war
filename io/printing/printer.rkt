#lang racket

(provide (all-defined-out))
(require "buffer.rkt")
(require "../../world/world_constants.rkt")

(define ESC (integer->char 27))


(define (with-color fg bg [of (current-output-port)])
   (fprintf of "~c[~a;~am" ESC (+ fg FG) (+ bg BG)))

(define (reset-color)
  (with-color -30 -40))

(define (move-to x y)
   (printf "~c[~a;~aH" ESC (+ y 1) x))

(define (pixel char x y fg bg)
  (with-color fg bg)
  (move-to x y)
  (printf "~c" char))

(define (clear-screen)
  (reset-color)
  (printf "~c[H~c[2J" ESC ESC))

(define (draw-buffer buf)
  (define cfg 8)
  (define cbg 8)
  (define tmp (open-output-bytes))
  (move-to 0 0)
  (for ([y (range (screen-buffer-height buf))])
    (for ([x (range (screen-buffer-width buf))])
      (define cell (screen-buffer-get-cell buf x y))
      (when (or (not (= (cell-fg cell) cfg))
                (not (= (cell-bg cell) cbg)))
        (set! cfg (cell-fg cell))
        (set! cbg (cell-bg cell))
        (with-color cfg cbg tmp))
      (fprintf tmp "~c" (cell-char cell)))
    (fprintf tmp "~%"))
  (display (get-output-bytes tmp)))



