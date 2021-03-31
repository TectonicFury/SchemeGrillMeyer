(define (tree-map proc tree)
  (map (lambda (z) (if (pair? z)
                       (tree-map proc z)
                       (proc z))) tree)
)

(define (square-tree tree) (tree-map square tree))  
(define (square x) (* x x))
