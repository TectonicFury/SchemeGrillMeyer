(define (duplicate-items lst dup-count)
  (let ((dupc (if (< (length dup-count) (length lst))
                  (append-times dup-count (+ 1 (quotient (length lst) (length dup-count)))) ; make the length of dupc bigger than lst)
                  dup-count)))
       (define (dup-items z dc res)
         (if (null? z)
             res
             (dup-items (cdr z) (cdr dc) (append (append-times (cons (car z) '()) (car dc)) res))
         )) (reverse (dup-items lst dupc '())))
)

(define (append-times items n) ; append a list n times for making dup-count have sufficient length
  (define (iter z a res)
    (if (= a 0)
        res
        (iter z (- a 1) (append z res))
    )
  )
  (iter items n '())
)

(define (reverse lst)
  (define (iter z result)
    (if (null? z)
        result
        (iter (cdr z) (cons (car z) result))
    )
  )
  (iter lst '())
)

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))
  )
)
