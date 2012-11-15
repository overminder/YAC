(define globalvar 12345)

(define main
  (lambda ()
    (%funcall putInt (%symbol-val globalvar))
    (%funcall putInt (%symbol-addr globalvar))
    0))

