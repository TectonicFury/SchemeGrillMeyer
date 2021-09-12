(define (integrate-series stream)
  (define (iter count s)
    (cons-stream (/ (car s) count) (iter (+ count 1) (stream-cdr s))))
  (iter 1 stream))

(define cosine-series (cons-stream 1 (stream-map - (integrate-series sine-series))))
(define sine-series (cons-stream 0 (integrate-series cosine-series)))
(define sine-series2 (cons-stream 0 (integrate-series (cons-stream 1 (integrate-series (stream-map (lambda (x) (* -1 x)) sine-series2))))))
(define cosine-series2 (cons-stream 1 (integrate-series (stream-map - (cons-stream 0 (integrate-series cosine-series2))))))
