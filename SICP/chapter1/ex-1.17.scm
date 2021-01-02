(define (fast-mult a b) (fast-mult-inner a b a))

(define (fast-mult-inner a b a_)
  (cond ((or (= b 0) (= a 0)) 0)
        ((= b 1) a)
        ((even? b) (fast-mult-inner (double a) (halve b) a_))
        (else (+ a_ (fast-mult-inner  a (- b 1) a_))) 
  )
)


(define (double a) (+ a a))
(define (halve a) (/ a 2))
(define (even? x) (= (remainder x 2) 0))
