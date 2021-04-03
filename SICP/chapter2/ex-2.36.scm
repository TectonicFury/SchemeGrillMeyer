(define (accumulate-n op init seqs)
  (if (null? (car seqs)) '()
      (cons (accumulate op init (append-first seqs)) (accumulate-n op init (append-rest seqs)))))

(define (append-first seqs)
  (define (iter items res)
    (if (null? items)
        res
        (iter (cdr items) (cons (car (car items)) res))))
          (reverse (iter seqs '())))
(define (append-rest seqs)
  (define (iter items res)
    (if (null? items)
        res
        (iter (cdr items) (cons (cdr (car items)) res)))
  ) (reverse (iter seqs '())))

(define (reverse items)
  (define (iter z res)
    (if (null? z)
        res
        (iter (cdr z) (cons (car z) res))))
          (iter items '()))
