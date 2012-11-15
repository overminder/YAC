(define main
  (lambda ()
    (define f (%symbol-addr putInt))
    (%funcall/r f 12345)))

