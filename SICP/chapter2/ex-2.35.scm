(define (fun)
  (define (iter z) (cond ((null? z) 0)
                         ((not (pair? z)) 1)
                         (else (+ (iter (car z)) (iter (cdr z)))))) iter)
