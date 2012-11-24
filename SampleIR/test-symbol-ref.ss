(define main
  (lambda ()
    (define a (%symbol-addr printf))
    (%funcall putHex a)
    (set! a (%symbol-val printf))
    (%funcall putHex a)
    0))

