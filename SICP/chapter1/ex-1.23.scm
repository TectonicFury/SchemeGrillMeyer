(define (next a)
  (cond ((= a 2) 3)
        (else (+ a 2))
  )
)

(define (smallest-divisor n) (smallest-divisor-aux n 2))

(define (smallest-divisor-aux n m)
  (cond ((> (sqr m) n) n)
        ((= (remainder n m) 0) m)
        (else (smallest-divisor-aux n (next m)))
  )
)

(define (timed-prime-test n) (newline)
(display n)
(start-prime-test n (runtime)))
(define (start-prime-test n start-time) (if (prime? n)
(report-prime (- (runtime) start-time)))) (define (report-prime elapsed-time)
(display " *** ") (display elapsed-time))

(define (three-primes-larger-than n)
  (define (three-primes-larger-aux m pcount)
    (cond ((even? m) (three-primes-larger-aux (+ m 1) pcount))
          ((and (prime? m) (< pcount 3)) (timed-prime-test m) (three-primes-larger-aux (+ m 2) (+ pcount 1)))
          ((= pcount 3) (newline) (display "Done"))
          (else (three-primes-larger-aux (+ m 2) pcount))
    )
  )
(three-primes-larger-aux (+ n 1) 0))

(define (even? n) (= (remainder n 2) 0))

  (define (prime? n)
    (= (prime-test n 2) n)
  )

  (define (prime-test n m)
    (cond ((> (sqr m) n)  n)
          ((divisor-test? n m) m)
          (else (prime-test n (cond ((= m 2) 3)
                (else (+ m 2))
          ))))
  )


  (define (divisor-test? n m) (= (remainder n m) 0))

  (define (sqr x) (* x x))
