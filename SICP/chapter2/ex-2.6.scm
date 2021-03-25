(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))


;calculating one
(add-1 zero)
(lambda (f) (lambda (x) (f ((n f) x))))

(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))

(lambda (f) (lambda (x) (f ((lambda (x) x) x))))

(lambda (f) (lambda (x) (f x)))
;hence complete
(define one (lambda (f) (lambda (x) (f x))))


; calculating two
(add-1 one)
(lambda (f) (lambda (x) (f ((one f) x))))
(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x))))
(lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
(lambda (f) (lambda (x) (f (f x))))

;hence complete
(define two (lambda (f) (lambda (x) (f (f x)))))
