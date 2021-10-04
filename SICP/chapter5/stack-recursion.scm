(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(controller
  (assign n (op read))
  (assign continue (label fact-done))
  (assign product (const 1))
  test-n
    (test (op =) (reg n) (const 1))
    (branch (reg continue))
    (save n)
    (assign n (op -) (reg n) (const 1))
    (save continue)
    (assign continue (label after-fact))
    (goto (label test-n))
  after-fact
    (restore continue)
    (restore n)
    (assign product (op *) (reg n) (reg product))
    (goto (reg continue))
  fact-done)

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(controller
  (assign n (const 6))
  (assign continue (label fib-done))

  fib-loop
    (test (op <) (reg n) (const 2))
    (branch (label base-case))
    (save continue)
    (save n)
    (assign n (op -) (reg n) (const 1))
    (assign continue (label after-fib1))
    (goto (label fib-loop))
  after-fib1
    (restore n)
    (assign n (op -) (reg n) (const 2))
    (assign continue (label after-fib2))
    (save val)
    (goto (label fib-loop))
  after-fib2
    (assign n (reg val))
    (restore val)
    (restore continue)
    (assign val (op +) (reg val) (reg n))
    (goto (reg continue))
  base-case
    (assign val (reg n))
    (goto (reg continue))
  fib-done)
