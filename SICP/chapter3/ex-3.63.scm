(define (sqr-root x)
  (define sqrt-improve
    (cons-stream 1.0 (stream-map (lambda (y) (- y (/ (- (square y) x) (* 2 y)))) sqrt-improve)))
  sqrt-improve)

(define (square x) (* x x))

(define (stream-map proc s) ; stream-map on page 320
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))
