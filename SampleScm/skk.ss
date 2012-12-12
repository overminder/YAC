(define (s f g x)
  (f x (g x)))

(define (k x y)
  x)

(define i (s k k 3))
(display i)

