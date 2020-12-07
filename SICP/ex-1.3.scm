; ex 1.3 define function to take three arguments and return the sum of squares of the larger two

; helper procedure
(define (square x) (* x x))

(define (sum-of-squares-of-larger-two a b c)
  (- (+ (square a ) (square b) (square c)) (square (min a b c)))
)
