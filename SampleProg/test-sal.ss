(define main
  (lambda (argc argv)
    (%funcall printArgs 0 argc argv)
    0))

(define printArgs
  (lambda (i argc argv)
    (if (< i argc)
        (begin
	  (%funcall puts (%get-quad argv (%sal i 3)))
	  (%funcall/t printArgs (+ i 1) argc argv))
	0)))

