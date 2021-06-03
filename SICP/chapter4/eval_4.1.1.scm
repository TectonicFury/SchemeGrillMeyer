;section 4.1.1

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

        ((lambda? exp); a lambda expression must be transformed into an applicable procedure by packaging together the parameters --
                      ; -- and body specified by the lambda expression with the environment of the evaluation
          (make-procedure (lambda-parameters exp)
                          (lambda-body exp)
                          env))
        ((begin? exp); a beging expression requires evaluating its sequence of expressions in the order in which they appear
          (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env)); a case analysis (cond) is transformed into a nest of if expressions and then --
                                               ; -- evaluated
        ((application? exp); for a procedure application, eval musty recursively evaluate the operator part and the operands of the --
                           ; -- combination. The resulting procedure and arguments are passed to apply, which handles the actual --
                           ; procedure application.
          (apply (eval (operator exp) env) (list-of-values (operands exp) env)))
        (else (error "Unknown expression type -- EVAL" exp))))


(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
          (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
          (eval-sequence
            (procedure-body procedure)
            (extend-environment
              (procedure-parameters procedure)
              arguments
              (procedure-environment procedure))))
        (else (error "Unknown procedure type: APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env) (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp) (eval (assignment-value exp) env) env)
  'ok)
(define (eval-definition exp env)
  (define-variable! (definition-variable exp) (eval (definition-value exp) env) env)
  'ok)

;section 4.1.2

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp)
  (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) (cddr exp))))

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp)
  (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

;derived expressions
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clause exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF" clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

;;solutions to exercises



;ex 4.3
