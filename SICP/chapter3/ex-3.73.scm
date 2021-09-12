(define (RC R C dt)
  (define (func I V0)
    (cons-stream
      V0 (add-streams
          (stream-cdr (integral (stream-map (lambda (x) (/ x C)) I) V0 dt))
          (stream-map (lambda (x) (* x R)) I))))
  func)


(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
      (add-streams (scale-stream integrand dt) int)))
int)

(define (add-streams s1 s2)
  (stream-map-gen + s1 s2))
