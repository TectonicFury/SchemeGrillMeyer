(define (reverse sequence)
  (accumulate (lambda (x y) (append y (cons x '()))) '() sequence))
(define (reverse-fl sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))


  (define (fold-left op initial sequence)
    (define (iter result rest)
      (if (null? rest) result
          (iter (op result (car rest)) (cdr rest))))
  (iter initial sequence))
