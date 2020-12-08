;ex 1.7 from SICP
;this is the actual newton's method to compute the square root of a number
;which is in reality the solution of the equation x^2 - c = 0

;comparing the consecutive values of the algorithm

;first version without let

(define (square-root-newton x guess)
  (if (< (abs (- guess (+ (/ guess 2.0) (/ x (* 2.0 guess))))) 0.00001)
    guess
    (square-root-newton x (+ (/ guess 2.0) (/ x (* 2.0 guess)))))
)

;second version with let (more elegant)
(define (square-root-newton x guess)
  (let ( (new_guess (+ (/ guess 2.0) (/ x (* 2.0 guess)))))
    (if (< (abs (- guess new_guess)) 0.00001)
    guess
    (square-root-newton x new_guess))
  )
)
