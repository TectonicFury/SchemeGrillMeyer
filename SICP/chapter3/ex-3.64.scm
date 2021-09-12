(define (stream-limit stream tolerance)
  (if (< (abs (- (stream-car stream) (stream-car (stream-cdr stream)))) tolerance)
      (stream-car (stream-cdr stream))
      (stream-limit (stream-cdr stream) tolerance)))
