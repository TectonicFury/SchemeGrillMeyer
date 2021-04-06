(define (mergesort items)
  (define (merge y z)
    (define (merge-iter a b res)
      (cond ((null? a) (append res b))
            ((null? b) (append res a))
            (else ((lambda (x)
                           (if x
                               (merge-iter (cdr a) b (append res (list (car a))))
                               (merge-iter a (cdr b) (append res (list (car b))))))
                                 (< (car a) (car b)))))) ;merge-iter closes
      (merge-iter y z '())) ; merge body closes
  (if (= (accumulate + 0 (map (lambda (x) 1) items)) 1)
      items
      (merge (mergesort (list (car items))) (mergesort (cdr items))))) ;mergesort closes

(define (accumulate op null-value seq)
  (if (null? seq)
      null-value
      (op (car seq) (accumulate op null-value (cdr seq)))))
