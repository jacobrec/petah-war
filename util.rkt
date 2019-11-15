#lang racket
(provide (all-defined-out))

(define syncsem (make-semaphore 1))

(define-syntax-rule (syncron E ...)
  (dynamic-wind (lambda () (semaphore-wait syncsem))
                (lambda () E ...)
                (lambda () (semaphore-post syncsem))))
