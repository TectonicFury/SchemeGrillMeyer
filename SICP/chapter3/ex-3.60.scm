(define (mul-series s1 s2)
  (let
    ((sc1 (stream-car s1))
     (sc2 (stream-car s2)))
    (cons-stream
      (* sc1 sc2)
      (add-streams
        (add-streams (scale-stream (stream-cdr s2) sc1) (scale-stream (stream-cdr s1) sc2))
         (cons-stream 0 (mul-series (stream-cdr s1) (stream-cdr s2)))))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))
