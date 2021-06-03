(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (cadr exp))
        (else
          (let ((expr-type-eval-func (get 'eval (operator exp))))
            (if expr-type-eval-func
                (expr-type-eval-func exp env)
                (if (application? exp)
                    (apply (eval (operator exp) env) (list-of-values (operands exp) env))
                    (error "Unknown expression type -- EVAL" exp)))))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define proc-tbl (general-table (lambda (x y) (eq? x y))))

(define (put key-1 key-2 func)
  ((proc-tbl 'insert-proc) (list key-1 key-2) func))

(define (get key-1 key-2)
((proc-tbl 'lookup-proc) (list key-1 key-2)))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
        (eval-sequence (rest-exps exps) env))))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (make-begin seq)
  (cons 'begin seq))
;the below type doesn't take the env variable, so we will just put it into the main eval function
; (define (install-quoted-type)
;   (define (text-of-quotation exp)
;     (cadr exp))
;   (put 'eval 'quote text-of-quotation))

;types installation
(define (install-assignment-type)
  (define (assignment-variable exp)
    (cadr exp))
  (define (assignment-value exp)
    (caddr exp))
  (define (eval-assignment exp env)
    (set-variable-value! (assignment-variable exp) (eval (assignment-value exp) env) env)
    'ok)
  (put 'eval 'set! eval-assignment))

(define (install-definition-type)
  (define (definition-variable exp)
    (if (symbol? (cadr exp))
        (cadr exp)
        (caadr exp)))
  (define (definition-value exp)
    (if (symbol? (cadr exp))
        (caddr exp)
        ((get 'eval 'make-lambda) (cdadr exp) (cddr exp))));make-lambda has to be called using get
  (define (eval-definition exp env)
    (define-variable! (definition-variable exp) (eval (definition-value exp) env) env)
    'ok)
  (put 'eval 'define eval-definition))

(define (install-if-type)
  (define (if-predicate exp) (cadr exp))
  (define (if-consequent exp) (caddr exp))
  (define (if-alternative exp)
    (if (not (null? (cdddr exp)))
        (cadddr exp)
        'false))

  (define (make-if predicate consequent alternative)
    (list 'if predicate consequent alternative))

  (define (eval-if exp env)
    (if (true? (eval (if-predicate exp) env))
        (eval (if-consequent exp) env)
        (eval (if-alternative exp) env)))

  (put 'eval 'if eval-if))

(define (install-lambda-type)
  (define (lambda-parameters exp) (cadr exp))
  (define (lambda-body exp) (cddr exp))

  (define (lambda-eval exp env)
    (make-procedure (lambda-parameters exp) (lambda-body exp) env))
  (put 'eval 'lambda lambda-eval))

(define (install-begin-type)
  (define (begin-actions exp) (cdr exp))

  (define (begin-eval exp env)
    (eval-sequence (begin-actions exp) env))
  (put 'eval 'begin begin-eval))

;...doing cond would be a grind so it's not done
