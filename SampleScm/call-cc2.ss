(define (null? x)
  (eq? x '()))

(define (for-each proc lst)
  (if (null? lst) '()
      (begin
        (proc (car lst))
        (for-each proc (cdr lst)))))

(define (box x) (cons x '()))
(define unbox car)
(define set-box! set-car!)

(define (generate-one-element-at-a-time lst)
  (define ctrl-box (box '()))
  (define (control-state $return)
    (define ret-box (box $return))
    (for-each 
     (lambda (element)
               (set-box! ret-box (call/cc
                                   (lambda (resume-here)
                                    (set-box! ctrl-box resume-here)
                                    ((unbox ret-box) element)))))
     lst)
    ((unbox ret-box) 99999))
  (set-box! ctrl-box control-state)
  (define (generator)
    (call/cc (unbox ctrl-box)) )
  generator)
 
(define generate-digit
  (generate-one-element-at-a-time (cons 0 (cons 1 (cons 2 '())))))

(define (prn x)
  (display x)
  (newline))
 
(prn (generate-digit))
(prn (generate-digit))
(prn (generate-digit))
(prn (generate-digit))

