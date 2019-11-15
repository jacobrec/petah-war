#lang racket

(require "../world/world.rkt")
(require "../io_adapter.rkt")
(require json)
(provide (all-defined-out))

(define (send-turn world out)
  (write-json (get-world-state-to-send world) out))

(define (receive-turn world in)
  (read-json in))

(define (get-world-state-to-send world)
  (make-hash `((units . ,(world-units world))
               (buildings . ,(world-buildings world)))))

(define (update-world-state-from-recived world data)
  (set-world-units! (dict-ref data 'units))
  (set-world-buildings! (dict-ref data 'buildings)))

(define (do-game world out in first)
  (unless first
    (receive-turn world in))
  (define (loop)
    (start-input-loop world)
    (send-turn world out)
    (receive-turn world in)
    (loop))
  (loop))
