#lang racket

(require "../world/world.rkt")
(require "../world/units.rkt")
(require "../world/buildings.rkt")
(require "../io_adapter.rkt")
(require "../world/types.rkt")
(require json)
(provide (all-defined-out))

(define (send-turn world out)
  (write-json (get-world-state-to-send world) out)
  (flush-output out)
  (win-game world))

(define (receive-turn world in)
  (define data (read-json in))
  (if (eof-object? data)
    data
    (begin
      (update-world-state-from-recived world data)
      (win-game world))))

(define (get-world-state-to-send world)
  (make-hash `((units . ,(map unit->vector (world-units world)))
               (buildings . ,(map building->vector (world-buildings world))))))

(define (unit->vector u)
  [list (unit-x u) (unit-y u) (unit-owner-id u) (unit-type u)])

(define (building->vector u)
  [list (building-x u) (building-y u) (building-owner-id u) (building-type u)])

(define (vector->unit v)
  (unit (list-ref v 0) (list-ref v 1) (list-ref v 2) (list-ref v 3)))

(define (vector->building v)
  (building (list-ref v 0) (list-ref v 1) (list-ref v 2) (list-ref v 3)))

(define (update-world-state-from-recived world data)
  (set-world-units! world (map vector->unit (dict-ref data 'units)))
  (set-world-buildings! world (map vector->building (dict-ref data 'buildings))))

(define (all-same lst)
  (cond
    [(null? (cdr lst)) (car lst)]
    [(equal? (car lst) (all-same (cdr lst)))
     (car lst)]
    [#t #f]))

(define (win-game world)
  (define winner
    (all-same
      (map
        building-owner-id
        (filter
          (lambda (b)
            (= (building-type b) BUILD_HQ))
          (world-buildings world)))))
  (when winner
    (end-screen)
    (if (= winner (world-player-id world))
      (displayln "YOU WIN!")
      (displayln "YOU LOSE!"))
    (exit)))

(define (do-game world out in first)
  (define (loop)
    (set-world-status! world "Your Turn!")
    (start-input-loop world)
    (send-turn world out)
    (set-world-status! world "Waiting for opponent(s)")
    (if (eof-object? (receive-turn world in))
      (set-world-status! world "Other player disconnected")
      (loop)))
  (if first
    (loop)
    (begin
      (set-world-status! world "Waiting for opponent(s)")
      (if (eof-object? (receive-turn world in))
        (set-world-status! world "Other player disconnected")
        (loop)))))
