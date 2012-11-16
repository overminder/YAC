(define main
  (lambda ()
    (%funcall putInt (%funcall get7thArg 0 1 2 3 4 5 6))
    (%funcall putInt (%funcall get8thArg 0 1 2 3 4 5 6 7))
    (%funcall printf (%symbol-addr fmt) 0 1 2 3 4 5 6 7)
    (%funcall manyArgs 0 1 2 3 4 5 6)))

(define manyArgs
  (lambda (a b c d e f g)
    (%funcall putInt (+ a b c d e f g))))

(define fmt "[%ld %ld %ld %ld %ld %ld %ld %ld]\n")
