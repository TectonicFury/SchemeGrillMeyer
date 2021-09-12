; a.
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
; b.

(define (scan-out-defines body)
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
    (append (list 'let iter-val) iter-set iter-non-val)))

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




(define (definition? exp)
(tagged-list? exp 'define))
