(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else (+ (count-leaves (car tree)) (count-leaves (cdr tree))))))

(controller
  (assign continue (label count-leaves-done))
  (assign tree (op read))
  count-loop
    (test (op null?) tree)
    (branch (label base-case-null))
  test-not-pair
    (assign pair-flag (op pair?) (reg tree))
    (test (op not) (reg pair-flag))
    ;(test (op not) (op pair?) (reg tree))
    (branch (label base-case-not-pair))
    (save continue)
    (save tree)
    (assign continue (label after-count-car))
    (assign tree (op car) (reg tree))
    (goto (label count-loop))
  after-count-car
    (restore tree)
    (assign tree (op cdr) (reg tree))
    (assign continue (label after-count-cdr))
    (save val)
    (goto (label count-loop))
  after-count-cdr
    (assign n (reg val))
    (restore val)
    (assign val (op +) (reg n) (reg val))
    (restore continue)
    (goto (reg continue))
  base-case-null
    (assign val (const 0))
    (goto (reg continue))
  base-case-not-pair
    (assign val (const 1))
    (goto (reg continue))
  count-leaves-done)
  ;same as above minus the comment
  (controller
    (assign continue (label count-leaves-done))
    (assign tree (op read))
    count-loop
      (test (op null?) (reg tree))
      (branch (label base-case-null))
    test-not-pair
      (assign pair-flag (op pair?) (reg tree))
      (test (op not) (reg pair-flag))
      (branch (label base-case-not-pair))
      (save continue)
      (save tree)
      (assign continue (label after-count-car))
      (assign tree (op car) (reg tree))
      (goto (label count-loop))
    after-count-car
      (restore tree)
      (assign tree (op cdr) (reg tree))
      (assign continue (label after-count-cdr))
      (save val)
      (goto (label count-loop))
    after-count-cdr
      (assign n (reg val))
      (restore val)
      (assign val (op +) (reg n) (reg val))
      (restore continue)
      (goto (reg continue))
    base-case-null
      (assign val (const 0))
      (goto (reg continue))
    base-case-not-pair
      (assign val (const 1))
      (goto (reg continue))
    count-leaves-done)




















(define (count-leaves tree)
  (define (count-iter tree n)
    (cond ((null? tree) n)
          ((not (pair? tree)) (+ n 1))
          (else (count-iter (cdr tree) (count-iter (car tree) n)))))
  (count-iter tree 0))

(controller
  (assign tree (op read))
  (assign n (const 0))
  test-null
    (test (op null?) tree)
    (branch (label base-case-null))
  base-case-null
