  (define (deriv exp var)
    (cond ((number? exp) 0)
          ((variable? exp)
            (if (same-variable? exp var) 1 0))
          (else ;(display "Expr >> ") (display exp) (display ";")(display "Operator >> ") (display (operator exp)) (newline)
          ((get 'deriv (operator exp)) (operands exp) var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (put key-1 key-2 func)
  ((procedure-table 'insert-proc) (list key-1 key-2) func))
(define (get key-1 key-2)
  ((procedure-table 'lookup-proc) (list key-1 key-2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (variable? x)
  (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

; (define (sum? x) (and (pair? x) (eq? (car x) '+))) ; these functions are not needed since we are not using case analysis, rather we are using data directed programming
; (define (product? x) (and (pair? x) (eq? (car x) '*)))

; we need to have a general-table to store procedures using 'put' which will then be called using 'get'
(define procedure-table (general-table (lambda (x y) (eq? x y))))

(define (install-add-calculus-functions)

(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

  (define (make-sum a1 a2)
    (cond ((null? a2) a1)
          ((and (pair? a2) (or (sum? a2) (product? a2) (exponentiation? a2)))
            (cond ((=number? a1 0) a2)
                  ((sum? a2) (append (list '+ a1) (cdr a2)));just makes it look more simplified
                  (else (list '+ a1 a2))))
          ((pair? a2); if a2 is simply the list of addend terms, then doing (car a2) won't fetch the '+' symbol when doing make-sum
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

  (define (deriv-sum elements var)
    (if (null? elements)
        0
        (make-sum (deriv (car elements) var) (deriv-sum (cdr elements) var))))
  ;installing the procedures
  (put 'deriv '+ deriv-sum))

(define (install-mult-calculus-functions)

  (define (make-product m1 m2)
    (cond ((null? m2) m1)
          ((and (pair? m2) (or (sum? m2) (product? m2) (exponentiation? m2))) ;m2 is having (* ..) form already، so we just make a product out of it
            (cond ((=number? m1 0) 0)
                  ((=number? m1 1) m2)
                  ((product? m2) (append (list '* m1) (cdr m2)))
                  (else (list '* m1 m2))))
          ((pair? m2) ;m2 is just the remainder of the list of multiplcative arguments
            ;(display "m2 >> ") (display m2) (newline)
            (let ((m2prod (make-product (car m2) (cdr m2))))
              ;(display "m2prod >> ") (display m2prod) (newline)
              (cond ((or (=number? m1 0) (=number? m2prod 0)) 0)
                    ((=number? m1 1) m2prod)
                    ((=number? m2prod 1) m1)
                    ((and (number? m1) (number? m2prod)) (* m1 m2prod))
                    (else (list '* m1 m2prod)))))
          (else
            (cond ((or (=number? m1 0) (=number? m2 0)) 0)
                  ((=number? m1 1) m2)
                  ((=number? m2 1) m1)
                  ((and (number? m1) (number? m2)) (* m1 m2))
                  (else (list '* m1 m2))))))

  (define (deriv-product elements var)
    (if (null? (cdr elements))
        (deriv (car elements) var)
        (let ((multiplier (car elements))
              (multiplicand (cdr elements))
              (multipcnd-prod (make-product (cadr elements) (cddr elements))))

              ;(display "elements >> ") (display elements) (newline)
              ;(display "multiplicand-prod >> ") (display multipcnd-prod)(newline)
              (make-sum
                (make-product multiplier
                             (deriv multipcnd-prod var))
                (make-product (deriv multiplier var) multiplicand)))))
  ; installing the procedure
  (put 'deriv '* deriv-product)

  ;auxilliary functions -- same as in the sum/add package, but repeated here to enable independent installation

  (define (sum? x) (and (pair? x) (eq? (car x) '+)))
  (define (product? x) (and (pair? x) (eq? (car x) '*)))
  (define (exponentiation? x)
    (and (pair? x) (eq? (car x) '**)))

  (define (make-sum a1 a2)
    (cond ((null? a2) a1)
          ((and (pair? a2) (or (sum? a2) (product? a2) (exponentiation? a2)))
            (cond ((=number? a1 0) a2)
                  ((sum? a2) (append (list '+ a1) (cdr a2)));just makes it look more simplified
                  (else (list '+ a1 a2))))
          ((pair? a2); if a2 is simply the list of addend terms, then doing (car a2) won't fetch the '+' symbol when doing make-sum
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
  )

  ; auxilliary functions for table

  (define (general-table same-key?)
    (define (assoc key records)
      (cond ((or (null? records) (not (pair? records))) false)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))

    (let ((local-table (list '*table*)))
      (define (lookup key-list)
        (define (iter-lookup sub-table sub-keys)
          (cond ((null? sub-keys) (cdr sub-table))
                (else
                  (let ((record-tbl (assoc (car sub-keys) (cdr sub-table))))
                    (if record-tbl
                        (iter-lookup record-tbl (cdr sub-keys))
                        false)))))
      (iter-lookup local-table key-list))

      (define (insert! key-list value)
        (define (iter-insert! sub-table sub-keys)
          (let ((stbl (assoc (car sub-keys) (cdr sub-table))))
            (if stbl
                (if (null? (cdr sub-keys)) ;did we reach the last key? in this case stbl would be a record
                    (set-cdr! stbl value) ; add the key
                    (iter-insert! stbl (cdr sub-keys))) ; go down further
                ;if no stbl was located, do what's below))
                (if (null? (cdr sub-keys))
                    (set-cdr! sub-table (cons (cons (car sub-keys) value) (cdr sub-table)))
                    (begin (set-cdr! sub-table (cons (list (car sub-keys)) (cdr sub-table)))
                           (iter-insert! (car (cdr sub-table)) (cdr sub-keys)))))))
        (iter-insert! local-table key-list)
        'ok)

      (define (dispatch m)
        (cond ((eq? m 'lookup-proc) lookup)
              ((eq? m 'insert-proc) insert!)
              (else (error "Unknown operation -- TABLE" m))))
      dispatch))
