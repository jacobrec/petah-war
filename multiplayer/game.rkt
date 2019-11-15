#lang racket

(require "../world/world.rkt")
(require json)

(define (send-turn world out)
  (write-json (get-world-state-to-send world)) out)

(define (receive-turn world in)
  (read-json (get-world-state-to-send world)) in)

(define (get-world-state-to-send world)
  (make-hash `((units . ,(world-units world))
               (buildings . ,(world-buildings world)))))

(define (update-world-state-from-recived world data)
  (set-world-units! (dict-ref data 'units))
  (set-world-buildings! (dict-ref data 'buildings)))

(define (do-game world out in)
  (define (loop)
    (receive-turn world in)
    (send-turn world out)
    (loop))
  (loop))
