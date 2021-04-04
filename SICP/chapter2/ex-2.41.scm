(define (sum-equal-to n s)
  (let ((seq (enumerate-interval 1 n)))
    (filter (lambda (z) (= (+ (car z) (cadr z) (cadr (cdr z))) s))
      (accumulate append '() (flatmap (lambda (i)
        (let ((sqn (remove i seq)))
             (map (lambda (j)
                    (map (lambda (k) (list i j k)) (remove j sqn))) sqn))) seq)))))


(define (filter pred seq)
  (define (iter z res)
      (cond ((null? z) res)
            ((pred (car z)) (iter (cdr z) (append res (cons (car z) '()))))
            (else (iter (cdr z) res))))
            (iter seq '()))

(define (permutations s)
  (if (null? s) ; empty set?
    (list nil) ; sequence containing empty set
    (flatmap (lambda (x) (map (lambda (p) (cons x p)) (permutations (remove x s))))
            s)))


(define (remove item sequence)
  (filter (lambda (x) (not (= x item))) sequence))
