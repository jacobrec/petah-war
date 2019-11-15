#lang racket

(require "../world/world.rkt")
(require "../world/units.rkt")
(require "../world/buildings.rkt")
(require "../io_adapter.rkt")
(require json)
(provide (all-defined-out))

(define (send-turn world out)
  (write-json (get-world-state-to-send world) out))

(define (receive-turn world in)
  (define data (read-json in))
  (update-world-state-from-recived world data))

(define (get-world-state-to-send world)
  (make-hash `((units . ,(map unit->vector (world-units world)))
               (buildings . ,(map building->vector (world-buildings world))))))

(define (unit->vector u)
  [list (unit-x u) (unit-y u) (unit-owner-id u) (unit-type u)])

(define (building->vector u)
  [list (building-x u) (building-y u) (building-owner-id u) (building-type u)])

(define (vector->unit v)
  (building (vector-ref v 0) (vector-ref v 1) (vector-ref v 2) (vector-ref v 3)))

(define (vector->building v)
  (unit (vector-ref v 0) (vector-ref v 1) (vector-ref v 2) (vector-ref v 3)))

(define (update-world-state-from-recived world data)
  (set-world-units! (map vector->unit (dict-ref data 'units)))
  (set-world-units! (map vector->building (dict-ref data 'buildings))))

(define (do-game world out in first)
  (unless first
    (set-world-status! world "Waiting for opponent(s)")
    (receive-turn world in))
  (define (loop)
    (set-world-status! world "Your Turn!")
    (start-input-loop world)
    (send-turn world out)
    (set-world-status! world "Waiting for opponent(s)")
    (receive-turn world in)
    (loop))
  (loop))
