#lang racket

(require "world/game.rkt")
(require "world/make_map.rkt")
(require "world/world.rkt")
(require "io_adapter.rkt")
(require "io/input.rkt")
(require "multiplayer/start.rkt")
(require "multiplayer/game.rkt")


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

(define (game in out pid)
  (define world (make-world 50 25 pid))
  (timed-loop 30
    (lambda ()
      (draw-world world)))
  (timed-loop 100
    (lambda ()
      (update-world world)))
  (with-raw
    (do-game world out in (= pid 1))
    (end-screen)
    (displayln "Other player disconnected")
    (exit)))

;; Start
(define-values [in out pid] (prompt-handshake))
(start-screen)
(with-handlers ([exn:break? (lambda (exn)
                               (end-screen))])
  (game in out pid)
  (sleep 100))

