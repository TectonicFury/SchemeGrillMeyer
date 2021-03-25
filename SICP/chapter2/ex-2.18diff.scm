(define (reverse lst)
  (define (iter z result)
    (if (null? z)
        result
        (iter (cdr z) (cons (car z) result))
    )
  )
  (iter lst '())
)
