#lang racket

(provide (all-defined-out))
(struct building (x y owner-id type) #:mutable)
(struct unit
  (x y owner-id type [has-moved #:auto])
  #:mutable #:auto-value #f)
