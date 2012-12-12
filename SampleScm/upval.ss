(define (upval a)
  (lambda (b)
    (lambda (c)
      (c b a))))

