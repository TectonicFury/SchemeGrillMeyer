(define (suc-sqr-exp b n) (suc-sqr-iter-exp b n 1))

(define (suc-sqr-iter-exp b n a)
  (cond ((= n 0) 1)
        ((= n 1) (* b a))
        ((even? n) (suc-sqr-iter-exp (* b b) (/ n 2) a))
        (else (suc-sqr-iter-exp (* b b) (/ (- n 1) 2) (* a b)))
  )
)

(define (even? x) (= (remainder x 2) 0))
