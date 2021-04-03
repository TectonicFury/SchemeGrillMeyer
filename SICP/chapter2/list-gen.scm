(define (list-gen n)
  (define (iter a res)
    (if (= a 0)
        res
        (iter (- a 1) (cons a res))))
          (iter n '()))
