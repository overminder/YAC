(define main
  (lambda ()
    (funcall putInt (funcall get7thArg 0 1 2 3 4 5 6))
    (funcall manyArgs 0 1 2 3 4 5 6)))

(define manyArgs
  (lambda (a b c d e f g)
    (funcall putInt (+ a b c d e f g))))
