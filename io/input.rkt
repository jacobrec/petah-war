#lang racket
(provide (all-defined-out))

(define keymap (make-hash '(
                            (left . #f)
                            (right . #f)
                            (up . #f)
                            (down . #f))))


(define (stty x) (system (~a "stty " x)) (void))
(define-syntax-rule (with-raw body ...)
  (let ([saved #f])
    (dynamic-wind (λ() (set! saved (with-output-to-string (λ() (stty "-g"))))
                       (stty "raw -echo opost"))
                  (λ() body ...)
                  (λ() (stty saved)))))

(define (get-size)
  (stty "size"))

(define (loop)
    (if (char-ready?)
      (printf "key pressed: [~c]~%" (read-char))
      (displayln "No pressed"))
    (sleep 1)
  (loop))
