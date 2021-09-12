(define s (cons-stream 1 (merge (merge (scale-stream s 2) (scale-stream s 3)) (scale-stream s 5))))
