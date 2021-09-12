(define zero-crossings
  (stream-map-gen sign-change-detector sense-data (cons-stream 0 sense-data)))



(define (make-zero-crossings input-stream last-value)
  (cons-stream
    (sign-change-detector (stream-car input-stream) last-value)
    (make-zero-crossings (stream-cdr input-stream) (stream-car input-stream))))

(define zero-crossings (make-zero-crossings sense-data 0))

(define (stream-map-gen proc . argstreams)
  (if (null? (car argstreams))
      the-empty-stream
      (cons-stream
        (apply proc (map stream-car argstreams))
        (apply stream-map-gen (cons proc (map stream-cdr argstreams))))))
