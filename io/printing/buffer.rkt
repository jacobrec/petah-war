#lang racket

(provide (all-defined-out))

(struct cell (fg bg char) #:mutable)
(struct screen-buffer (width height cells) #:mutable)

(define (make-buffer w h)
  (screen-buffer w h (make-vector (* w h) (cell 0 0 0))))

(define (screen-buffer-get-cell buf x y)
  (vector-ref
    (screen-buffer-cells buf)
    (+ x (* y (screen-buffer-width buf)))))

(define (screen-buffer-set-pixel! buf x y fg bg char)
  (vector-set!
    (screen-buffer-cells buf)
    (+ x (* y (screen-buffer-width buf)))
    (cell fg bg char)))
