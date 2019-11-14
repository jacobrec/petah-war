#lang racket

(provide (all-defined-out))

(require "unix.rkt")

(define (prompt-handshake)
  (display "join or create game? [j/c] ")
  (case (read-line)
    (["c"] (prompt-create-game))
    (["j"] (prompt-join-game))
    (else (prompt-handshake))))

(define (prompt-create-game)
  (define id (generate-id))
  (printf "gameid: ~a\n" id)
  (displayln "waiting for other user...")
  (host-game id))

(define (prompt-join-game)
  (display "gameid? ")
  (join-game (read-line)))
