(define main
  (lambda ()
    (funcall putInt (funcall lt 0 1))
    (funcall putInt (funcall lt 1 1))
    (funcall putInt (funcall lt 2 1))
    (funcall putInt (funcall lt 0 2))))

(define lt
  (lambda (a b)
    (if (< a b) 1 0)))

