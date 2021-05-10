(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (if (= (length leaf-set) 2)
      (make-code-tree (car leaf-set) (cadr leaf-set))
      (let ((node (make-code-tree (car leaf-set) (cadr leaf-set))))
        (successive-merge (adjoin-set node (cddr leaf-set))))))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))
      
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) (cadr pair))
          (make-leaf-set (cdr pairs))))))

; x is never already in set if it is being added
(define (adjoin-set x set)
  (cond ((null? set) (list x))
   ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))
