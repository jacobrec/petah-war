#lang racket
(provide (all-defined-out))

(struct world (grid
               bg-overlay
               status
               menu
               menuidx
               units
               buildings
               width height
               selection
               money
               player-id
               cur-x cur-y) #:mutable)

(define BLK 0)
(define RED 1)
(define GRN 2)
(define YEL 3)
(define BLU 4)
(define MAG 5)
(define CYN 6)
(define WHT 7)
(define DFT 9) ; IDK why/how/if this one works
(define FG 30)
(define BG 40)

(define TILE_GRASS 0)
(define TILE_WATER 1)
(define TILE_MOUNTAIN 2)
(define TILE_FOREST 3)
(define TILE_ROAD 4)

(define UNIT_INFANTRY 0)  ; Land unit, can take bases, is cheap
(define UNIT_TANK 1)      ; Land unit, powerful
(define UNIT_PLANE 2)     ; Air unit, powerful
(define UNIT_HELICOPTER 3); Air unit, carries infantry
(define UNIT_BOMBER 4)    ; Air unit, attacks land units
(define UNIT_DESTROYER 5) ; Sea unit, powerful
(define UNIT_BATTLESHIP 6); Sea unit, long range, moves slow, attacks any units
(define UNIT_FERRY 7)     ; Sea unit, carries infantry and tanks
;; Costs AIR > SEA > LAND

(define BUILD_HQ 0)
(define BUILD_MONEY 1)
(define BUILD_FACTORY 2)
(define BUILD_SEAFACTORY 2)
(define BUILD_COVER 3)

