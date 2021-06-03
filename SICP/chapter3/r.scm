

(define (make-table)

  (let ((local-table (list '*table*)))

    (define (assoc key records)
      (cond ((null? records) false)
            ((equal? key (caar records)) (car records))
            (else (assoc key (cdr records)))))

    (define (lookup . keys)
      (display local-table)(newline)
      (let ((record (assoc keys (cdr local-table))))
        (if record
            (cdr record)
            false)))

    (define (insert! value . keys)
      (let ((record (assoc keys (cdr local-table))))
        (if record
            (set-cdr! record value)
                  (set-cdr! local-table
                            (cons (cons keys value)
                                  (cdr local-table)))))
      'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))
