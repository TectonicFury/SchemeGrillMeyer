#! /usr/local/bin/mit-scheme --load

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

        ((while? exp) (eval-while exp env)) ; ex-4.9

        ((for? exp) (eval (for->combination exp) env)) ; ex-4.9

        ((let? exp) (eval (let->combination exp) env)) ; ex-4.6

        ((letrec? exp) (eval (letrec->let exp) env)) ; ex 4.20

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


(define apply-in-underlying-scheme apply)

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

(define (scan-out-defines body) ; ex-4.16
  (define (iter-def bdy);take out defines
    (if (null? bdy)
        '()
        (let ((exp (car bdy)))
          (if (definition? exp)
              (cons (list (definition-variable exp) ''*unassigned*) (iter-def (cdr bdy)))
              (iter-def (cdr bdy))))))
  (define (iter-non-def bdy);take out non defines
    (if (null? bdy)
        '()
        (let ((exp (car bdy)))
          (if (not (definition? exp))
              (cons exp (iter-non-def (cdr bdy)))
              (iter-non-def (cdr bdy))))))
  (define (iter-set-list bdy) ; the set! statements
    (if (null? bdy)
        '()
        (let ((exp (car bdy)))
          (if (definition? exp)
              (cons (list 'set! (definition-variable exp) (definition-value exp))
                    (iter-set-list (cdr bdy)))
              (iter-set-list (cdr bdy))))))

  (let ((iter-val (iter-def body))
        (iter-non-val (iter-non-def body))
        (iter-set (iter-set-list body)))
    (if (null? iter-val)
        body
        (list (append (list 'let iter-val) iter-set iter-non-val)))))

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
(define (cond-clauses exp) (cdr exp))
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


;section 4.1.3
(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))
(define (make-procedure parameters body env)
;(list 'procedure parameters body env))
  (list 'procedure parameters (scan-out-defines body) env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p)
  (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

; (define (lookup-variable-value var env)
;   (define (env-loop env)
;     (define (scan vars vals)
;       (cond ((null? vars) (env-loop (enclosing-environment env)))
;             ((eq? var (car vars)) (car vals))
;             (else (scan (cdr vars) (cdr vals)))))
;     (if (eq? env the-empty-environment)
;         (error "Unbound variable" var)
;         ;#f
;         (let ((frame (first-frame env)))
;           (scan (frame-variables frame)
;                 (frame-values frame)))))
;   (env-loop env))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
              (if (eq? (car vals) '*unassigned*)
                  (error "Attempt to use unassigned variable -- LOOKUP" var)
                  (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        ;#f
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame) (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars) (add-binding-to-frame! var val frame))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame) (frame-values frame))))
;;solutions to exercises

; section 4.1.4

(define (setup-environment)
  (let ((initial-env
          (extend-environment
            (primitive-procedure-names) (primitive-procedure-objects) the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
  initial-env))

(define (primitive-procedure? proc) (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '< <)
        (list '> >)
        (list '= =)
        (list 'abs abs)
        (list 'sqrt sqrt-newton)
        (list 'display display)))

(define (primitive-procedure-names) (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc))) primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme (primitive-implementation proc) args))

(define input-prompt "M-Eval input:")
(define output-prompt "M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
         (announce-output output-prompt)
         (user-print output)))
(driver-loop))

(define (prompt-for-input string)
(newline) (newline) (display string) (newline))

(define (announce-output string) (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
    (display (list 'compound-procedure (procedure-parameters object) (procedure-body object) '<procedure-env>)) (display object)))


;; additional forms

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
; (while predicate-is-true body) -- this is the form

(define (while-predicate exp) (cadr exp))
(define (while-body exp) (cddr exp))
(define (eval-while exp env)
  (if (true? (eval (while-predicate exp) env))
      (begin (eval-sequence (while-body exp) env)
             (eval-while exp env))
      'while-done))

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
             (cons (make-lambda lv lbody) lb))))

;ex 4.20

(define (letrec? exp) (tagged-list? exp 'letrec))
(define (letrec-inits exp) (cadr exp))
(define (letrec-vars exp) (map car (letrec-inits exp)))
(define (letrec-vals exp) (map cadr (letrec-inits exp)))
(define (letrec-body exp)
  (cddr exp))
(define (letrec->let exp)
  (let ((inits-unassigned (map (lambda (x) (list x ''*unassigned*)) (letrec-vars exp))))
       (define (set-block vars vals)
         (if (null? vars)
             '()
             (cons (list 'set! (car vars) (car vals)) (set-block (cdr vars) (cdr vals)))))
    (append (list 'let) (list inits-unassigned) (set-block (letrec-vars exp) (letrec-vals exp)) (letrec-body exp))))

; sqrt from newton method
(define (sqrt-newton n)
  (define (iter x)
    (if (< (abs (- (* x x) n)) 0.001)
        x
        (iter (- x (/ (- (* x x) n) (* 2 x))))))
  (iter 1.0))

  (define the-global-environment (setup-environment))

  (driver-loop)
