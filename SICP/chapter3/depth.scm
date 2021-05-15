(define (depth tree)
  (if (null? tree)
      0
      (max-items (map (lambda (x)
                    (cond ((not (pair? x)) 1)
                          (else (+ 1 (depth x))))) tree))))

(define (max-items items)
  (define (iter lst cur-max)
    (cond ((null? lst) cur-max)
          ((> (car lst) cur-max) (iter (cdr lst) (car lst)))
          (else (iter (cdr lst) cur-max))))
  (iter (cdr items) (car items)))
