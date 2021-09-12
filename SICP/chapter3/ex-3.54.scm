(define factorials (cons-stream 1 (mul-stream factorials integers)))
(define integers (cons-stream 1 (add-streams ones integers)))
(define ones (cons-stream 1 ones))
(define (mul-streams s1 s2)
  (stream-map-gen * s1 s2))

;aux
(define (add-streams s1 s2)
  (stream-map-gen + s1 s2))

(define (stream-map-gen proc . argstreams)
  (if (null? (car argstreams))
      the-empty-stream
      (cons-stream
        (apply proc (map stream-car argstreams))
        (apply stream-map-gen (cons proc (map stream-cdr argstreams))))))

(define (map proc seq)
  (if (null? seq)
      '()
      (cons (proc (car seq)) (map proc (cdr seq)))))
