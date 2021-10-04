(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))

  (define (improve guess)
    (average guess (/ x guess)))

  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

; simple version
(controller
  (assign x (op read))
  (assign guess (const 1.0))
  test-good-enough
    (test (op good-enough?) (reg guess))
    (branch (label sqrt-done))
    (assign guess (op improve) (reg guess))
    (goto (label test-good-enough))
  sqrt-done)

; expanded version
(controller
  (assign x (const 2))
  (assign guess (const 1.0))
  test-good-enough
  ; the comparison test would use the < operation to compare values in two registers
  ; hence we need to put the values into new registers (here t) before calculation
    (assign t (op *) (reg guess) (reg guess))
    (assign t (op -) (reg t) (reg x))
    (test (op <) (reg t) (const 0))
    (branch (label abs-negative))
  tolerance-check
    (test (op <) (reg t) (const 0.001))
    (branch (label sqrt-done))
    (assign t (op /) (reg x) (reg guess))
    (assign guess (op +) (reg guess) (reg t))
    (assign guess (op /) (reg guess) (const 2.0))
    (goto (label test-good-enough))
  abs-negative
    (assign t (op -) (cosnt 0) (reg t))
    (goto (label tolerance-check))
  sqrt-done
  )

  (define m (make-machine(list
                            (list '* *)
                            (list '- -)
                            (list '< <)
                            (list '+ +)
                            (list '/ /))
  '((assign x (const 2.0))
    (assign guess (const 1.0))
    test-good-enough
    (assign t (op *) (reg guess) (reg guess))
    (assign t (op -) (reg t) (reg x))
    (test (op <) (reg t) (const 0))
    (branch (label abs-negative))
    tolerance-check
      (test (op <) (reg t) (const 0.001))
      (branch (label sqrt-done))
      (assign t (op /) (reg x) (reg guess))
      (assign guess (op +) (reg guess) (reg t))
      (assign guess (op /) (reg guess) (const 2.0))
      (goto (label test-good-enough))
      abs-negative
        (assign t (op -) (const 0) (reg t))
        (goto (label tolerance-check))
      sqrt-done)))

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))


(controller
  (assign x (op read))
  (assign guess (const 1.0))
  test-goodenough
    (test (op good-enough?) (reg guess))
    (branch (label sqrt-done))
    (assign guess (op improve) (reg guess))
    (goto (label test-goodenough))
  sqrt-done)

(controller
  (assign x (op read))
  (assign guess (const 1.0))
  test-goodenough
    (assign t (op *) (reg guess) (reg guess))
    (assign t (op -) (reg t) (reg x))
    (test (op <) t (const 0))
    (branch (label abs))
  tolerance-check
    (test (op <) (reg t) (const 0.001))
    (branch (label sqrt-done))
  improve-guess
    (assign t (reg guess))
    (assign guess (op /) (reg x) (reg guess))
    (assign guess (op +) (reg guess) (reg t))
    (goto (label test-goodenough))
  abs
    (assign t (op -) (const 0) (reg t))
    (goto (label tolerance-check))
  sqrt-done)
