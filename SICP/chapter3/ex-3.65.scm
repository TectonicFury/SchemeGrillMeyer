(define (ln2-summands n)
  (cons-stream (/ 1.0 n) (stream-map - (ln2-summands (+ n 1)))))

(define ln2-stream (partial-sums (ln2-summands 1)))

(display-stream (euler-transform ln2-stream))

(display-stream
  (accelerated-sequence euler-transform ln2-stream))

(define (pi-summands n)
  (cons-stream (/ 1.0 n) (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

(define (partial-sums s)
  (cons-stream (stream-car s) (add-streams (stream-cdr s) (partial-sums s))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))
(define (square x) (* x x))

(define (stream-map proc s) ; stream-map on page 320
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
       (s1 (stream-ref s 1))
       (s2 (stream-ref s 2)))
(cons-stream (- s2 (/ (square (- s2 s1)) (+ s0 (* -2 s1) s2))) (euler-transform (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
(display x))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
(stream-for-each proc (stream-cdr s)))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))
(define (add-streams s1 s2)
  (stream-map-gen + s1 s2))

  (define (stream-map-gen proc . argstreams)
    (if (null? (car argstreams))
        the-empty-stream
        (cons-stream
          (apply proc (map stream-car argstreams))
          (apply stream-map-gen (cons proc (map stream-cdr argstreams))))))
