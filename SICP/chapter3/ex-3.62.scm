(define (div-series s_num s_den)
  (if (= 0 (stream-car s_den))
      (error "Division not working -- constant term in denominator is 0")
      (mul-series s_num (invert-series s_den))))

(define (invert-series s)
  (cons-stream 1 (stream-map - (mul-series (stream-cdr s) (invert-series s)))))

(define (mul-series s1 s2)
  (let
    ((sc1 (stream-car s1))
     (sc2 (stream-car s2)))
    (cons-stream
      (* sc1 sc2)
      (add-streams
        (add-streams (scale-stream (stream-cdr s2) sc1) (scale-stream (stream-cdr s1) sc2))
         (cons-stream 0 (mul-series (stream-cdr s1) (stream-cdr s2)))))))
