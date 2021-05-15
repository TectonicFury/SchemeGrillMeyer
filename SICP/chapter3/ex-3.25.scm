(define (general-table same-key?)
  (define (assoc key records)
    (cond ((or (null? records) (not (pair? records))) false)
          ((same-key? key (caar records)) (car records))
          (else (assoc key (cdr records)))))

  (let ((local-table (list '*table*)))
    (define (lookup key-list)
      (define (iter-lookup sub-table sub-keys)
        (cond ((null? sub-keys) (cdr sub-table))
              (else
                (let ((record-tbl (assoc (car sub-keys) (cdr sub-table))))
                  (if record-tbl
                      (iter-lookup record-tbl (cdr sub-keys))
                      false)))))
    (iter-lookup local-table key-list))

    (define (insert! key-list value)
      (define (iter-insert! sub-table sub-keys)
        (let ((stbl (assoc (car sub-keys) (cdr sub-table))))
          (if stbl
              (if (null? (cdr sub-keys)) ;did we reach the last key? in this case stbl would be a record
                  (set-cdr! stbl value) ; add the key
                  (iter-insert! stbl (cdr sub-keys))) ; go down further
              ;if no stbl was located, do what's below))
              (if (null? (cdr sub-keys))
                  (set-cdr! sub-table (cons (cons (car sub-keys) value) (cdr sub-table)))
                  (begin (set-cdr! sub-table (cons (list (car sub-keys)) (cdr sub-table)))
                         (iter-insert! (car (cdr sub-table)) (cdr sub-keys)))))))
      (iter-insert! local-table key-list)
      'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

    ; (define (insert! key-1 key-2 value)
    ;   (let ((subtable (assoc key-1 (cdr local-table))))
    ;     (if subtable
    ;         (let ((record (assoc key-2 (cdr subtable))))
    ;           (if record
    ;               (set-cdr! record value)
    ;               (set-cdr! subtable (cons (cons key-2 value) (cdr subtable)))))
    ;         (set-cdr! local-table (cons (list key-1 (cons key-2 value))
    ;                                         (cdr local-table)))))
    ;   'ok)