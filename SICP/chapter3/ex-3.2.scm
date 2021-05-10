(define (make-monitored f)
  (let ((num-calls 0))
    (define (mf x)
      (cond ((eq? x 'how-many-calls) num-calls)
            ((eq? x 'reset-count) (set! num-calls 0))
            (else (set! num-calls (+ num-calls 1)) (f x)))) mf))



(define (make-monitored f)
  (let ((num-calls 0))
    (lambda (x)
      (cond ((eq? x 'how-many-calls) num-calls)
            ((eq? x 'reset-count) (set! num-calls 0))
            (else (set! num-calls (+ num-calls 1)) (f x))))))
