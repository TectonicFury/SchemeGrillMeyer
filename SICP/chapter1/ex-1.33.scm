(define (filtered-accumulate combiner filter-pred? null-value term a next b)
  (if (> a b)
      null-value
      (if (filter-pred? a)
          (combiner (term a) (filtered-accumulate combiner filter-pred? null-value term (next a) next b))
          (filtered-accumulate combiner filter-pred? null-value term (next a) next b)
      )
  )
)
;using lambdas as predicate and for next
(define (rel-prime-prod n)
  (filtered-accumulate * (lambda (x) (= (gcd x n) 1)) 1 (lambda (x) x) 2 (lambda (x) (+ 1 x)) (- n 1))
)

(define (prime? n) (fast-prime-miller-rabin? n 5))

(define (gcd a b)
  (if (= b 0)
       a
       (gcd b (remainder a b))
  )
)



;auxilliary functions from previous exercises (1.28)
(define (expmod-mod base exp m)
  (cond ((and (= (remainder (sqr base) m) 1) (not (or (= base 1) (= base (- m 1))))) 0)
        ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod-mod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod-mod base (- exp 1) m)) m))
  )
)

(define (fast-prime-miller-rabin? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime-miller-rabin? n (- times 1)))
        (else false)
  )
)

(define (sqr x) (* x x))
(define (inc x) (+ 1 x))
(define (identity x) x)
(define (even? x) (= (remainder x 2) 0))
(define (fermat-test n)
  (define (try-it a) (= (expmod-mod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1))))
)
