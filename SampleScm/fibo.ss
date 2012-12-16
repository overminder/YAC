(define (fibo n)
  (if (< n 2)
      n
      (+ (fibo (- n 1))
	 (fibo (- n 2)))))

(define (make-cell n)
  (lambda ()
    n))

(begin
  (define cell (make-cell (cons 1 2)))
  (display (fibo 35))
  (newline)
  (display (cell))
  (newline))

