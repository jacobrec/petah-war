#lang racket

(provide (all-defined-out))

(define (choice-width choices)
  (apply max (map string-length choices)))

(define (render-choices choices i)
  (define w (choice-width choices))
  (displayln (string-append "╔" (make-string (+ 2 w) #\═) "╗"))
  (for ([item choices]
        [idx (range (length choices))])
       (displayln
         (string-append "║"
                        (if (= i idx) "*" " ")
                        item
                        (make-string (- w (string-length item)) #\ )
                        " ║")))
  (displayln (string-append "╚" (make-string (+ 2 w) #\═) "╝")))
