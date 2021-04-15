(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
          (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp) (make-sum
                              (make-product (multiplier exp) (deriv (multiplicand exp) var))
                              (make-product (deriv (multiplier exp) var) (multiplicand exp))))
        ((exponentiation? exp)
          (make-product (exponent exp)
                        (make-product (make-exponent (base exp) (- (exponent exp) 1)) (deriv (base exp) var))))
        (else
          (error "unknown expression type: DERIV" exp))))

(define (variable? x)
  (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((null? a2) a1)
        ((and (pair? a2) (or (sum? a2) (product? a2) (exponentiation? a2)))
          (cond ((=number? a1 0) a2)
                ((sum? a2) (append (list '+ a1) (cdr a2)))
                (else (list '+ a1 a2))))
        ((pair? a2)
          (let ((a2sum (make-sum (car a2) (cdr a2))))
            (cond ((=number? a1 0) a2sum)
                  ((=number? a2sum 0) a1)
                  ((and (number? a1) (number? a2sum)) (+ a1 a2sum))
                  (else (list '+ a1 a2sum)))))
          (else
            (cond ((=number? a1 0) a2)
                  ((=number? a2 0) a1)
                  ((and (number? a1) (number? a2)) (+ a1 a2))
                  (else (list '+ a1 a2))))))

(define (make-product m1 m2)
  (cond ((null? m2) m1)
        ((and (pair? m2) (or (product? m2) (sum? m2) (exponentiation? m2))) ;m2 is having (* ..) form already
          (cond ((=number? m1 0) 0)
                ((=number? m1 1) m2)
                ((product? m2) (append (list '* m1) (cdr m2)))
                (else (list '* m1 m2))))
        ((pair? m2) ;m2 is just the remainder of the list of multiplcative arguments
          (let ((m2prod (make-product (car m2) (cdr m2))))
            (cond ((or (=number? m1 0) (=number? m2prod 0)) 0)
                  ((=number? m1 1) m2prod)
                  ((=number? m2prod 1) m1)
                  ((and (number? m1) (number? m2prod)) (* m1 m2prod))
                  (else (list '* m1 m2prod)))
            ))
          (else
            (cond ((or (=number? m1 0) (=number? m2 0)) 0)
                  ((=number? m1 1) m2)
                  ((=number? m2 1) m1)
                  ((and (number? m1) (number? m2)) (* m1 m2))
                  (else (list '* m1 m2))))))

(define (make-exponent u n)
  (cond ((=number? n 0) 1)
        ((=number? n 1) u)
        (else (list '** u n))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base x)
  (cadr x))

(define (exponent x)
  (caddr x))

(define (sum? x) (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s)) ; if s is a sum it has to be of the form (+ a1 a2 ..), this is for ex-2.57

(define (augend s)
  (make-sum (caddr s) (cdr (cdr (cdr s)))))

(define (product? x) (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p)
  (make-product (caddr p) (cdr (cdr (cdr p)))))
