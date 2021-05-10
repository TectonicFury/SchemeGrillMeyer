(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree (make-leaf 'B 2)
                                  (make-code-tree (make-leaf 'D 1) (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define (decode bits tree)
  (define (iter rem-bits branch-tree)
    (cond ((null? rem-bits) '())
          (else
            (let ((bit (car rem-bits)))
              (cond ((= 0 bit)
                      (let ((lb (left-branch branch-tree)))
                        (if (leaf? lb)
                            (append (symbols lb) (decode (cdr rem-bits) tree))
                            (iter (cdr rem-bits) lb))))
                    ((= 1 bit)
                      (let ((rb (right-branch branch-tree)))
                        (if (leaf? rb)
                            (append (symbols rb) (decode (cdr rem-bits) tree))
                            (iter (cdr rem-bits) rb)))))))))
  (iter bits tree))

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))
