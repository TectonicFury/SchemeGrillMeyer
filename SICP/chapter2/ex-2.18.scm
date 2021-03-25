(define (reverse lst)
  (if (= (length lst) 1)
      lst
      (cons (last-pair lst) (reverse (all-but-last lst)))
  )
)

;the list with all but the last element 
(define (all-but-last lst)
  (if (= (length lst) 1)
      '()
      (cons (car lst) (all-but-last (cdr lst)))
  )
)

;from ex-2.17
(define (last-pair lst)
  (if (null? (cdr lst))
      (car lst)
      (last-pair (cdr lst))
  )
)

;from book
(define (length items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))
    )
  )
  (length-iter items 0)
)
