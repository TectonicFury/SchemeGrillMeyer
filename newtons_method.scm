(define (square-root x guess)
  (if (< (abs (- x (* guess guess))) 0.000001)
    guess
      (square-root x (+ (/ guess 2.0) (/ x (* 2 guess))))
  ))
  
