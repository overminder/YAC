(define main
  (lambda (argc argv)
    (%funcall printf (%symbol-addr str1)
	      argc)
    (%funcall showArgv 0 argc argv)
    0))

(define showArgv
  (lambda (i argc argv)
    (if (< i argc)
        (begin
	  (%funcall printf (%symbol-addr str2)
		    i (%get-quad argv (%sal i 3)))
	  (%funcall/t showArgv (+ i 1) argc argv))
	0)))

(define str1 "argc = %ld\n")
(define str2 "argv[%ld] = %s\n")

