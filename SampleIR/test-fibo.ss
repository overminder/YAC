(define main
  (lambda (argc argv)
    (define n 0)
    (set! n (if (< argc 2)
	        20
	        (%funcall atoi (%get-quad argv 8))))
    (%funcall putInt (%funcall fibo n))
    0))

(define fibo
  (lambda (n)
    (if (< n 2)
        n
	(+ (%funcall fibo (- n 1))
	   (%funcall fibo (- n 2))))))

