(define (integrate f a b n)
  (define (next x) (+ x (h)))

  (define (term x k)
    (cond ((or (= k 0) (= k n)) (f x))
          ((even? k) (* (f x) 2))
          (else (* (f x) 4))
    )
  )

  (define (h) (/ (- b a) n))

  (* (/ (h) 3.0) (sum term a next b 0 n))
)

(define (sum term a next b k n)
  (if (> a b) 0 (+ (term a k) (sum term (next a) next b (+ k 1) n))))

(define (cube x) (* x x x))
(define (inc x) (+ x 1))

(define (even? x) (= (remainder x 2) 0))
