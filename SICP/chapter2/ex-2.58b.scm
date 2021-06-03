(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
          (if (same-variable? exp var) 1 0))
        ((sum? exp) (begin (newline) (display "In DERIV:SUM, EXP = ") (display exp)
          (make-sum (deriv (addend exp) var) (deriv (augend exp) var))))
        ((product? exp) (begin (newline) (display "In DERIV:PRODUCT, EXP = ") (display exp) (make-sum
                              (make-product (multiplier exp) (deriv (multiplicand exp) var))
                              (make-product (deriv (multiplier exp) var) (multiplicand exp)))))
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
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (make-exponent u n)
  (cond ((=number? n 0) 1)
        ((=number? n 1) u)
        (else (list u '** n))))

(define (exponentiation? x)
  (and (pair? x) (eq? (cadr x) '**)))

(define (base x)
  (car x))

(define (exponent x)
  (caddr x))

(define (sum? x)
  (cond ((null? x) #f)
        ((eq? (car x) '+) #t)
        (else (sum? (cdr x)))))

(define (addend s)
  (define (iter-addend res rem)
    (cond ((not (eq? (cadr rem) '+)) (iter-addend (append res (list (car rem))) (cdr rem)))
          ((and (eq? (cadr rem) '+) (null? res)) (car rem))
          (else (append res (list (car rem))))))
  (iter-addend '() s))  ; if s is a sum it has to be of the form (+ a1 a2 ..), this is for ex-2.57

(define (augend s)
  (define (iter-augend res rem set-plus-found)
    (cond ((null? rem) res)
          ((= set-plus-found 1) (if (and (null? (cdr rem)) (null? res))
                                    (car rem)
                                    (iter-augend (append res (list (car rem))) (cdr rem) 1)))
          ((eq? (car rem) '+) (iter-augend res (cdr rem) 1))
          (else (iter-augend res (cdr rem) 0))))
  (newline) (display "augend of >> ") (display s) (display " = ") (display (iter-augend '() s 0))
    (iter-augend '() s 0))

(define (product? x) (pair? x)
  (if (memq '* x) #t #f))

(define (multiplier p)
  (newline)(display "multiplier of >> ")(display p) (display " = ")
  (define (iter-multr res rem)
    (cond ((not (eq? (cadr rem) '*)) (iter-multr (append res (list (car rem))) (cdr rem)))
          ((and (eq? (cadr rem) '*) (null? res)) (car rem))
          (else (append res (list (car rem))))))
  (display (iter-multr '() p)) (iter-multr '() p))

(define (multiplicand p)
  (define (iter-mult res rem set-*-found)
    (cond ((null? rem) res)
          ((= set-*-found 1) (if (and (null? (cdr rem)) (null? res))
                                 (car rem)
                                 (iter-mult (append res (list (car rem))) (cdr rem) 1)))
          ((eq? (car rem) '*) (iter-mult res (cdr rem) 1))
          (else (iter-mult res (cdr rem) 0))))
          (iter-mult '() p 0))

(define (accumulate op null-value seq)
  (if (null? seq)
      null-value
      (op (car seq) (accumulate op null-value (cdr seq)))))
