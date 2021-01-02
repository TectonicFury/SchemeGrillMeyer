(define (sine angle)
  (cond ((< (abs angle) 0.00000001) angle)
        (else (- (* 3.0 (sine (/ angle 3))) (* 4 (cube (sine (/ angle 3))))))
  )
)

(define (cube x) (* x x x))
