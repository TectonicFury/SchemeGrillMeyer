(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))
; recursive version
(controller
  (assign continue (label expt-done))

  expt-loop
  (test (op =) (reg n) (const 0))
  (branch (label base-case))

  (save continue)
  (assign continue (label after-expt))
  (assign n (op -) (reg n) (const 1))
  (goto (label expt-loop))

  after-expt
  (restore continue)
  (assign val (op *) (reg b) (reg val))
  (goto (reg continue))

  base-case
  (assign val (const 1))
  (goto (reg continue))
  expt-done)

(define (expt b n)
  (define (expt-iter counter product)
    (if (= counter 0)
        product
        (expt-iter (- counter 1) (* b product))))
  (expt-iter n 1))

(controller
  (assign product (const 1))
  test-cnt
  (test (op =) (reg counter) (const 0))
  (branch (label expt-done))
  (assign counter (op -) (reg counter) (const 1))
  (assign product (op *) (reg product) (reg b))
  (goto (label test-cnt))
  expt-done)
