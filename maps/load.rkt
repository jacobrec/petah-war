#lang racket

(require "../world/world.rkt")
(require "../world/types.rkt")
(require "../world/buildings.rkt")

(define (read-number-line f)
  (string->number (read-line f)))

(define (parse-map w f)
  (define width (world-width w))
  (define height (world-height w))

  (for ([y (range height)])
    (for ([x (range width)])
      (add-tile w x y (get-tile-from-char (read-char f))))))

(define (parse-buildings w uid f)
  (displayln uid)
  (define width (world-width w))
  (define height (world-height w))

  (for ([y (range height)])
    (for ([x (range width)])
      (define b (get-building-from-char (read-char f)))
      (when b
        (add-building w x y uid b)))))


(define (get-tile-from-char c)
  (cond
    [(char=? c #\~) TILE_WATER]
    [(char=? c #\") TILE_GRASS]
    [(char=? c #\%) TILE_FOREST]
    [(char=? c #\^) TILE_MOUNTAIN]
    [(char=? c #\╬) TILE_ROAD]
    [else TILE_WATER]))

(define (get-building-from-char c)
  (cond
    [(eof-object? c) #f]
    [(char=? c #\@) BUILD_HQ]
    [(char=? c #\$) BUILD_MONEY]
    [(char=? c #\#) BUILD_FACTORY]
    [(char=? c #\*) BUILD_SEAFACTORY]
    [else #f]))



(define (load-map filename)
  (define f (open-input-file filename))
  (define width (read-number-line f))
  (define height (read-number-line f))

  (define w (world (make-vector
                     (* width height)
                     TILE_WATER)
                   (make-vector
                     (* width height)
                     DFT)
                   ""                  ; Status
                   #f 0
                   (list)
                   (list)
                   width height
                   #f
                   3000 ; money
                   0 ; player id
                   #f ; direction state
                   0 0))               ; cur-x cur-y

  (parse-map w f)

  (read-line f)
  (define users (read-number-line f))

  (for ([u (range users)])
    (parse-buildings w (+ 1 u) f)
    (read-line f))
  w)

(define (add-building world x y uid tile)
  (set-world-buildings! world
                        (cons (building x y uid tile)
                              (world-buildings world)))
  world)

(define (add-tile world x y tile)
  (vector-set! (world-grid world)
               (+ x (* y (world-width world)))
               tile)
  world)

(displayln (world-buildings (load-map "test.map")))
