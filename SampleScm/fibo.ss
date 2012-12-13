(define (fibo n)
  (if (< n 2)
      n
      (+ (fibo (- n 1))
	 (fibo (- n 2)))))

(define (make-cell n)
  (lambda ()
    n))

(define cell (make-cell 123))
(display (fibo 30))
(newline)
(display (cell))
(newline)

