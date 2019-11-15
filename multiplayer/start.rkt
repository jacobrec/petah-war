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
  (let-values (([in out] (host-game id)))
    (values in out 1)))

(define (prompt-join-game)
  (display "gameid? ")
  (let-values (([in out] (join-game (read-line))))
    (values in out 2)))
