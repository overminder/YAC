(define (null? x)
  (eq? x '()))

(define (for-each proc lst)
  (if (null? lst) '()
      (begin
        (proc (car lst))
        (for-each proc (cdr lst)))))

(for-each display (cons 1 (cons 2 (cons 3 '()))))

