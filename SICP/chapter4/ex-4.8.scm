(define (let? exp) (tagged-list? exp 'let))
;named-let procedures
(define (named-let? exp)
  (symbol? (cadr exp)))
(define (named-let-def-var exp) ;the name of the would-be procedure
  (cadr exp))
(define (named-let-parameters exp)
  (define (iter res rem)
    (if (null? rem)
        res
        (iter (append res (list(caar rem))) (cdr rem))))
  (iter '() (caddr exp)))

(define (named-let-parameter-bindings exp)
  (define (iter res rem)
    (if (null? rem)
        res
        (iter (append res (list (cadr (car rem)))) (cdr rem))))
  (iter '() (caddr exp)))
(define (named-let-body exp)
  (sequence->exp (cdddr exp)))

(define (let->combination exp)
  (if (named-let? exp)
      (sequence->exp
        (list
            (list 'define
                  (append (list (named-let-def-var exp)) (named-let-parameters exp))
                  (named-let-body exp))
             (append (list (named-let-def-var exp)) (named-let-parameter-bindings exp))))
      (let ((lv (let-variables exp))
           (lb (let-bindings exp))
           (lbody (let-body exp)))
           (list (make-lambda lv lbody) lb))))

; below code from ex 4.6
(define (let-var-binds exp)
  (cadr exp))
(define (let-variables exp)
  (let ((lvb (let-var-binds exp)))
    (define (iter res rem)
      (if (null? rem)
          res
          (iter (append res (list (caar rem))) (cdr rem))))
  (iter '() lvb)))

(define (let-bindings exp)
  (let ((lvb (let-var-binds exp)))
    (define (iter res rem)
      (if (null? rem)
          res
          (iter (append res (list (cadr (car rem)))) (cdr rem))))
  (iter '() lvb)))

(define (let-body exp)
  (cddr exp)) ;<<<<-- lambda-body is (cddr exp), and make-lambda takes (cddr exp) for body straight away, so this is the only way

(define (let->combination exp)
  (let ((lv (let-variables exp))
        (lb (let-bindings exp))
        (lbody (let-body exp)))
        (list (make-lambda lv lbody) lb)))

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))
