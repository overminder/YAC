(define globalvar 12345)

(define main
  (lambda ()
    (define gvar-reg (%symbol-addr globalvar))
    (%set-quad! gvar-reg 0 54321)
    (%funcall putInt (%symbol-val globalvar))
    (%funcall putInt (%get-quad gvar-reg 0))
    0))

