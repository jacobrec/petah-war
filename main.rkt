#lang racket

(require "world/game.rkt")
(require "world/make_map.rkt")
(require "world/world.rkt")
(require "io_adapter.rkt")
(require "io/input.rkt")


(define (timed-loop time fn)
  (thread
    (lambda ()
      (timed-loop-thread-body time fn))))
(define (timed-loop-thread-body time fn)
  (define start (current-milliseconds))
  (fn)
  (define end (current-milliseconds))
  (define del (/ (- time (- end start)) 1000))
  (when (> del 0.0001)
    (sleep del))
  (timed-loop time fn))

(define (game)
  (define world (make-world 50 25 3))
  (timed-loop 30
    (lambda ()
      (draw-world world)))
  (timed-loop 100
    (lambda ()
      (update-world world)))
  (start-input-loop world))

;; Start
(start-screen)
(with-handlers ([exn:break? (lambda (exn)
                               (end-screen))])
  (game)
  (sleep 100))

