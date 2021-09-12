; factorial machine
(controller
 (assign n (const 10))
 (assign product (const 1))
 (assign counter (const 1))
 test-c ;label
  (test (op >) (reg counter) (reg n))
  (branch (label fact-done))
  (assign product (op *) (reg product) (reg counter))
  (assign counter (op +) (reg counter) (const 1))
  (goto (label test-c))
  fact-done)

(define f (make-machine '(n product counter)
                        (list
                          (list '> >)
                          (list '* *)
                          (list '+ +))
'((assign n (const 10))
(assign product (const 1))
(assign counter (const 1))
test-c ;label
 (test (op >) (reg counter) (reg n))
 (branch (label fact-done))
 (assign product (op *) (reg product) (reg counter))
 (assign counter (op +) (reg counter) (const 1))
 (goto (label test-c))
 fact-done)))
; gcd machine

(controller
  test-b
  (test (op =) (reg b) (const 0))
  (branch (label gcd-done))
  (assign t (op rem) (reg a) (reg b))
  (assign a (reg b))
  (assign b (reg t))
  (goto (label test-b))
  gcd-done)

(controller gcd-loop
  (assign a (op read))
  (assign b (op read))
  test-b
  (test (op =) (reg b) (const 0))
  (branch (label gcd-done))
  ;(assign t (op rem) (reg a) (reg b))
  rem-loop
  (test (op <) (reg a) (reg b))
  (branch (label rem-done))
  (assign a (op -) (reg a) (reg b))
  (goto (label rem-loop))
  rem-done
  (assign t (reg a))
  (assign a (reg b))
  (assign b (reg t))
  (goto (label test-b))
  gcd-done
  (perform (op print) (reg a))
  (goto (label gcd-loop)))
