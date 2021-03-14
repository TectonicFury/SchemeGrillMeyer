(define (product f a next b)
  (if (> a b)
    1
  (* (f a) (product f (next a) next b))))
(define (identity x) x)
(define (inc x) (+ x 1))
(define (factorial n) (product identity 1 inc n))

(define (product-iter f a next b)
  (define (iter a result)
    (if (> a b) result (iter (next a) (* result (f a))))) (iter 1 1))
(define (factorial-iter n) (product-iter identity 1 inc n))
