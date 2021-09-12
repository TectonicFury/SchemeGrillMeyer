

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
        ((while? exp) (eval-while exp env)) ; ex 4.9
        ((for? exp) (eval (for->combination exp) env))
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

;(for (initialise-bindings) (condition-to-check-evry-loop) (update) body)

(define (for? exp) (tagged-list? exp 'for))
(define (for-initial-var-binds exp) (cadr exp))
(define (for-counter exp) (car (cadr exp)))
(define (for-counter-val exp) (cadr (cadr exp)))
(define (for-predicate exp) (caddr exp))
(define (for-update exp) (cadddr exp))
(define (for-body exp) (cddddr exp))

(define (for->combination exp)
  (list 'let () ; since it is going to be a function definition, don't want it to be installed in global env
    (list 'define
      (list 'proc (for-counter exp))
        (list 'if (for-predicate exp)
                  (make-begin (list (make-begin (for-body exp)) (for-update exp) (list 'proc (for-counter exp))))
                  ''for-done))
    (list 'proc (for-counter-val exp)) ; calling the proc function
    ))


(define (while? exp)
  (tagged-list? exp 'while))
; (while predicate-is-true body)

(define (while-predicate exp) (cadr exp))
(define (while-body exp) (cddr exp))
(define (eval-while exp env)
  (if (true? (eval (while-predicate exp) env))
      (begin (display "predicate is true")
             (eval-sequence (while-body exp) env)
             (eval-while exp env))
      'while-done))


;
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))
