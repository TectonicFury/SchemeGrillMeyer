(letrec
  ((square (lambda (x) (* x x)))
   (y 2)
  (improve (lambda (x) (/ (+ x (/ y x)) 2)))
  (root
    (lambda (x)
      (if (< (abs (- (square x) y)) 0.0001) x (root (improve x))))))
      (root (read))
)
