(define main
  (lambda ()
    (%funcall testNormalLoop 10)
    (%funcall testBreakLoop 10)
    (%funcall testContinueLoop 10)
    0))

(define testNormalLoop
  (lambda (maxLoop)
    (define i 0)
    (%while (< i maxLoop)
            (%funcall putInt i)
            (set! i (+ i 1)))))

(define testBreakLoop
  (lambda (maxLoop)
    (define i 0)
    (%while 1
            (if (< i maxLoop)
                0
                (%break))
            (%funcall putInt i)
            (set! i (+ i 1)))))

(define testContinueLoop
  (lambda (maxLoop)
    (define i 0)
    (%while (< i maxLoop)
            (%funcall putInt i)
            (set! i (+ i 1))
            (%continue))))
