(define (mk-counter i)
  (lambda ()
    (set! i (+ i 1))
    i))

(define ctr (mk-counter 0))
(display (ctr))
(newline)

(display (ctr))
(newline)

(display (ctr))
(newline)

(display ctr)
(newline)

