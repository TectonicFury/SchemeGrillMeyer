(estimate-integral in-unit-circle? 1000 0.0 2.0 0.0 2.0)

(define (estimate-integral pred trials x-low x-hi y-low y-hi)
  (* (- x-hi x-low) (- y-hi y-low)
    (monte-carlo trials
      (lambda () (pred (random-in-range x-low x-hi) (random-in-range y-low  y-hi))))))

(define (in-unit-circle? x y)
  (<= (+ (square (- 1 x)) (square (- 1 y))) 1))

(define (square x) (* x x))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0) (/ trials-passed trials))
          ((experiment) (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else (iter (- trials-remaining 1) trials-passed))))
          (iter trials 0))
