(define (f ret)
  (ret 2)
  3)

(display (f (lambda (x) x)))
(newline)

(display (call/cc f))
(newline)

