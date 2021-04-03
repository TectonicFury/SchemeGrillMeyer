(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product v x)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))
    
(define (dot-product v w)
  (accumulate + 0 (map * v w)))



(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs)) (accumulate-n op init (map cdr seqs)))))

(define (accumulate op null-value seq)
  (if (null? seq)
      null-value
      (op (car seq) (accumulate op null-value (cdr seq)))))
