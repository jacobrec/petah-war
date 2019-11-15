#lang racket

(provide (all-defined-out))

(require racket/unix-socket)

(define socket-name-chars "abcdefghijklmnopqrstuvwxy0123456789")
(define gamedir "/tmp/petah-war")

(define (generate-id [len 8])
  (apply string
         (map
           (lambda (_)
             (string-ref
               socket-name-chars
               (random (string-length socket-name-chars))))
           (range len))))

(define (gameid->path id)
  (format "~a/~a.sock" gamedir id))

(define (host-game id)
  (unless (directory-exists? gamedir)
    (make-directory gamedir))
  (define sock
    (unix-socket-listen
      (gameid->path id)))
  (plumber-add-flush!
    (current-plumber)
    (lambda (_)
      (delete-game id)))
  (unix-socket-accept sock))

(define (join-game code)
  (unix-socket-connect (gameid->path code)))

(define (delete-game code)
  (delete-file (gameid->path code)))
