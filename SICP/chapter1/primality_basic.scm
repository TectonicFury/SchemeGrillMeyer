(define (primality-basic n)
  (= (prime-test n 2) n)
)

(define (prime-test n m)
  (cond ((> (sqr m) n)  n)
        ((divisor-test? n m) m)
        (else (prime-test n (+ m 1)))
  )
)

(define (smallest-divisor n) (smallest-divisor-aux n 2))

(define (smallest-divisor-aux n m)
  (cond ((> (sqr m) n) n)
        ((= (remainder n m) 0) m)
        (else (smallest-divisor-aux n (+ m 1)))
  )
)

(define (divisor-test? n m) (= (remainder n m) 0))

(define (sqr x) (* x x))
