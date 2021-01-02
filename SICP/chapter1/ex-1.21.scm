(define (smallest-divisor n) (smallest-divisor-aux n 2))

(define (smallest-divisor-aux n m)
  (cond ((> (sqr m) n) n)
        ((= (remainder n m) 0) m)
        (else (smallest-divisor-aux n (+ m 1)))
  )
)

(define (sqr x) (* x x))
