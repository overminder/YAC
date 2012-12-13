(define (loop-sum n s)
  (if (< n 1) s
      (loop-sum (- n 1) (+ s n))))

(display (loop-sum 10000000 0))

