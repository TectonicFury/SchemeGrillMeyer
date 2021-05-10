(define (mergesort cmp items)
  (define (merge y z)
    (define (merge-iter a b res)
      (cond ((null? a) (append res b))
            ((null? b) (append res a))
            (else ((lambda (x)
                           (if x
                               (merge-iter (cdr a) b (append res (list (car a))))
                               (merge-iter a (cdr b) (append res (list (car b))))))
                               (cmp (car a) (car b)))))) ;merge-iter closes
      (merge-iter y z '())) ; merge body closes
  (if (= (accumulate + 0 (map (lambda (x) 1) items)) 1)
      items
      (let ((divide-items (two-halves items)))
        (merge (mergesort cmp (car divide-items)) (mergesort cmp (cadr divide-items)))))) ;mergesort closes

(define (two-halves items)
  (let ((len (accumulate + 0 (map (lambda (x) 1) items))))
    (define (iter first second count)
      (if (>= count (/ len 2))
          (list (reverse first) second)
          (iter (cons (car second) first) (cdr second) (+ count 1))))
          (iter '() items 0)))

(define (reverse lst)
  (define (iter z result)
    (if (null? z)
        result
        (iter (cdr z) (cons (car z) result))))
  (iter lst '()))

(define (accumulate op null-value seq)
  (if (null? seq)
      null-value
      (op (car seq) (accumulate op null-value (cdr seq)))))
