(define (eval exp env) ; eval takes as arguments an expression and an environment
  ;eval is structured as a case analysis of the syntactic type of the expression to be evaluated

  (cond ((self-evaluating? exp) exp) ; for self-evaluating expressions, such as numbers, eval returns the expression itself

        ((variable? exp) (lookup-variable-value exp env)); eval must look up variables in the environment to find their values

        ((quoted? exp) (text-of-quotation exp)); for quoted expressions, eval returns the expression that was quoted

        ((assignment? exp) (eval-assignment exp env)); an assignment to (or definition of) a variable must recursively call eval to --
                                                     ;-- compute the new value to be associated with the variable.The environment --
                                                     ;-- must be modified to change (or create) the binding of the variable

        ((definition? exp) (eval-definition exp env))

        ((if? exp) (eval-if exp env)); an if exp requires special processing of its parts, so as to evaluate the consequent
                                     ; if the predicate is true, and otherwise to evaluate the alternative
        ((let? exp) (eval (let->combination exp) env))
        ((lambda? exp); a lambda expression must be transformed into an applicable procedure by packaging together the parameters --
                      ; -- and body specified by the lambda expression with the environment of the evaluation
          (make-procedure (lambda-parameters exp)
                          (lambda-body exp)
                          env))
        ((begin? exp); a begin expression requires evaluating its sequence of expressions in the order in which they appear
          (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env)); a case analysis (cond) is transformed into a nest of if expressions and then --
                                               ; -- evaluated
        ((application? exp); for a procedure application, eval musty recursively evaluate the operator part and the operands of the --
                           ; -- combination. The resulting procedure and arguments are passed to apply, which handles the actual --
                           ; procedure application.
          (apply (eval (operator exp) env) (list-of-values (operands exp) env)))
        (else (error "Unknown expression type -- EVAL" exp))))


(define (let? exp) (tagged-list? exp 'let))
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
