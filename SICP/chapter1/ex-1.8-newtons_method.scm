; ex 1.8 of SICP
; write procedure for evaluation of cube root of a number analogous to the one for square roots

; helper procedure
(define (square x) (* x x))

(define (cube-root-newton x guess)
  (let ( (new_guess (/ (+ (/ x (square guess)) (* 2.0 guess)) 3.0)) )
    (if (< (abs (- guess new_guess)) 0.0001)
    guess
    (cube-root-newton x new_guess)
    )
  )
)
