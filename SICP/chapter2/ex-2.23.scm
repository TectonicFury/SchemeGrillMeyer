(define (for-each f items)
  (define (temp f items tmp)
    (if (null? items)
      true
      (temp f (cdr items) (f (car items)))
    )
  )
  (temp f items 0)
)

(for-each (lambda (x) (newline) (display (* x x))) (list 1 2 3 4 5 6 7 8 9 10 11 12))
