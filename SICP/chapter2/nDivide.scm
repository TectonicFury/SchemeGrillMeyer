(define (nDivide x y n)
  (if (= n 0)
      x
      (Divide (nDivide x y (- n 1)) y)))

(define (nDivide2 x y n)
  (define (iter a res)
    (if (= a 0)
        res
        (iter (- a 1) (Divide res y))))
        (iter n x))
(define (Divide x y) (/ x y))
