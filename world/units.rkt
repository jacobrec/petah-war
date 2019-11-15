#lang racket
(require "world.rkt")
(require "types.rkt")
(provide (all-defined-out))

(define (unit-range unit)
  (define tunit (unit-type unit))
  (cond
    [(= tunit UNIT_INFANTRY) 8]
    [(= tunit UNIT_TANK) 10]

    [(= tunit UNIT_PLANE) 7]
    [(= tunit UNIT_BOMBER) 5]
    [(= tunit UNIT_HELICOPTER) 6]

    [(= tunit UNIT_DESTROYER) 5]
    [(= tunit UNIT_FERRY) 6]
    [(= tunit UNIT_BATTLESHIP) 3]
    [else 0]))

(define (unit-options-symbols unit)
  (define tunit (unit-type unit))
  (cond
    [(= tunit UNIT_INFANTRY) '(attack capture wait)]
    [(= tunit UNIT_TANK) '(attack wait)]

    [(= tunit UNIT_PLANE) '(attack wait)]
    [(= tunit UNIT_BOMBER) '(attack wait)]
    [(= tunit UNIT_HELICOPTER) '(rescue drop wait)]

    [(= tunit UNIT_DESTROYER) '(attack wait)]
    [(= tunit UNIT_FERRY) '(rescue drop wait)]
    [(= tunit UNIT_BATTLESHIP) '(attack wait)]
    [else #f]))

(define (unit-options unit)
  (map symbol->string (unit-options-symbols unit)))

(define (option-menu-pick->option-symbol unit option)
  (list-ref (unit-options-symbols unit) option))


(define (unit-do world unit option)
  (define opt (option-menu-pick->option-symbol unit option))
  (define x (world-cur-x world))
  (define y (world-cur-y world))
  (when (eq? opt 'wait)
    (set-unit-has-moved! unit #t)
    (set-unit-x! unit x)
    (set-unit-y! unit y)
    (set-world-selection! world #f)
    (set-world-menu! world #f))
  (when (eq? opt 'capture)
    (define done #f)
    (for ([bld (world-buildings world)]) #:break done
      (when (and (= (building-x bld) (world-cur-x world))
                 (= (building-y bld) (world-cur-y world)))
        (set! done #t)
        (set-building-owner-id! bld (unit-owner-id unit))))
    (set-unit-has-moved! unit #t)
    (set-unit-x! unit x)
    (set-unit-y! unit y)
    (set-world-selection! world #f)
    (set-world-menu! world #f))
  (when (eq? opt 'attack)
    (set-world-directional-select! world 'left))
  #f)

(define (unit-do-direction world unit option)
  (define opt (option-menu-pick->option-symbol unit option))
  (set-world-status! world (symbol->string opt))
  (define x (world-cur-x world))
  (define y (world-cur-y world))
  (when (eq? opt 'attack)
    ; Do attack
    (define-values (dsx dsy) (world-ds world))
    (define hit (get-unit-at? world dsx dsy))
    (define uu hit)
    (set! hit (and hit (does-kill? world unit hit)))

    ; Deal with results
    (if hit
      (set-world-status! world "It's a hit")
      (set-world-status! world "It's a miss"))
    (when hit
      (set-world-units! world
        (remq uu (world-units world))))

    ; Move unit
    (set-unit-has-moved! unit #t)
    (set-unit-x! unit x)
    (set-unit-y! unit y)

    ; Deselect things
    (set-world-selection! world #f)
    (set-world-menu! world #f)
    (set-world-directional-select! world #f)))

(define (does-kill? world unita unitb)
  (set-world-status! world "Attacking")
  (sleep 0.5)
  (set-world-status! world "Attacking.")
  (sleep 0.5)
  (set-world-status! world "Attacking..")
  (sleep 0.5)
  (set-world-status! world "Attacking...")
  (sleep 0.5)
  (> (kill-probability unita unitb) (random 100)))

(define (kill-probability unita unitb)
  (define ta (unit-type unita))
  (define tb (unit-type unitb))
  (cond
    [(= ta UNIT_INFANTRY)
     (cond
       [(= ta UNIT_INFANTRY) 75]
       [(= ta UNIT_TANK) 10]
       [(= ta UNIT_PLANE) 0]
       [(= ta UNIT_HELICOPTER) 3]
       [(= ta UNIT_BOMBER) 0]
       [(= ta UNIT_DESTROYER) 0]
       [(= ta UNIT_FERRY) 3]
       [(= ta UNIT_BATTLESHIP) 0]
       [else 0])]
    [(= ta UNIT_TANK)
     (cond
       [(= ta UNIT_INFANTRY) 100]
       [(= ta UNIT_TANK) 75]
       [(= ta UNIT_PLANE) 10]
       [(= ta UNIT_HELICOPTER) 20]
       [(= ta UNIT_BOMBER) 10]
       [(= ta UNIT_DESTROYER) 10]
       [(= ta UNIT_FERRY) 50]
       [(= ta UNIT_BATTLESHIP) 10]
       [else 0])]
    [(= ta UNIT_PLANE)
     (cond
       [(= ta UNIT_INFANTRY) 3]
       [(= ta UNIT_TANK) 3]
       [(= ta UNIT_PLANE) 90]
       [(= ta UNIT_HELICOPTER) 100]
       [(= ta UNIT_BOMBER) 90]
       [(= ta UNIT_DESTROYER) 3]
       [(= ta UNIT_FERRY) 10]
       [(= ta UNIT_BATTLESHIP) 3]
       [else 0])]
    [(= ta UNIT_HELICOPTER) 0]
    [(= ta UNIT_BOMBER)
     (cond
       [(= ta UNIT_INFANTRY) 100]
       [(= ta UNIT_TANK) 100]
       [(= ta UNIT_PLANE) 3]
       [(= ta UNIT_HELICOPTER) 100]
       [(= ta UNIT_BOMBER) 3]
       [(= ta UNIT_DESTROYER) 100]
       [(= ta UNIT_FERRY) 100]
       [(= ta UNIT_BATTLESHIP) 30]
       [else 0])]
    [(= ta UNIT_DESTROYER)
     (cond
       [(= ta UNIT_INFANTRY) 20]
       [(= ta UNIT_TANK) 20]
       [(= ta UNIT_PLANE) 2]
       [(= ta UNIT_HELICOPTER) 2]
       [(= ta UNIT_BOMBER) 2]
       [(= ta UNIT_DESTROYER) 90]
       [(= ta UNIT_FERRY) 100]
       [(= ta UNIT_BATTLESHIP) 30]
       [else 0])]
    [(= ta UNIT_FERRY) 0]
    [(= ta UNIT_BATTLESHIP)
     (cond
       [(= ta UNIT_INFANTRY) 100]
       [(= ta UNIT_TANK) 100]
       [(= ta UNIT_PLANE) 70]
       [(= ta UNIT_HELICOPTER) 100]
       [(= ta UNIT_BOMBER) 70]
       [(= ta UNIT_DESTROYER) 100]
       [(= ta UNIT_FERRY) 100]
       [(= ta UNIT_BATTLESHIP) 70]
       [else 0])]
    [else 0]))

(define (get-unit-at? world x y)
  (define hit #f)
  (for ([u (world-units world)])
    (when (and (= x (unit-x u)) (= y (unit-y u)))
      (set! hit u)))
  hit)
