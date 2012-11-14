(define add
  (lambda (x y)
    (+ x y)))

(define id
  (lambda (x)
    x))

(define main
  (lambda ()
    (funcall putInt (funcall add (funcall id 1) (funcall id 2)))))

