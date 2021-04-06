(define (amicable-sum n)
  (define (iter a sum)
    (if (= a n)
        sum
        (iter (+ a 1) (+ sum ((lambda (x) (if (and (< (d x) n) (not (= x (d x))) (= x (d (d x)) ))
                                              x
                                              0)) a)))))
   (iter 2 0))

(define (d n)
  (define (iter a sum)
    (if (> a (sqrt n))
        (+ 1 sum)
        (iter (+ a 1) (+ sum ((lambda (x) (if x (+ a (/ n a)) 0)) (= (remainder n a) 0))))))
        (iter 2 0))
