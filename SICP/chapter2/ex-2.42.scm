(define (queens board-size)
  (let ((empty-board (empty-board-gen board-size)))
    (define (queen-cols k)
      (if (= k 0)
          (list empty-board)
          (filter (lambda (positions) (safe? k positions))
            (flatmap
              (lambda (rest-of-queens)
                (map (lambda (new-row)
                        (adjoin-position
                          new-row k rest-of-queens))
                          (enumerate-interval 1 board-size)))
                          (queen-cols (- k 1))))))
      (queen-cols board-size)) ;end of let block
     )

(define (adjoin-position new-row k rqn)
  (let ((board-dim (accumulate + 0 (map (lambda (x) 1) rqn))));length of board
    (replace-elem (replace-elem 1 new-row (gen-zero-vector board-dim)) k rqn)))

(define (safe? k positions)
  (let ((row-pos-k (non-zero-index (item-at-index k positions))))
    (define (iter-check col rem)
      (let ((rw-col (non-zero-index (car rem))))
        (cond ((= k 1) #t)
              ((= col k) #t)
              ((or (= row-pos-k rw-col) (= (- k col) (abs (- row-pos-k rw-col)))) #f)
              (else (iter-check (+ col 1) (cdr rem))))))
    (iter-check 1 positions)))


(define (item-at-index n items)
  (define (iter a rem)
    (if (= a n)
        (car rem)
        (iter (+ a 1) (cdr rem))))
        (iter 1 items))

(define (non-zero-index items)
  (define (iter a rem)
    (if (> (car rem) 0)
        a
        (iter (+ a 1) (cdr rem))))
        (iter 1 items))

(define (empty-board-gen n)
  (map (lambda (x) (map (lambda (y) 0) (enumerate-interval 1 n))) (enumerate-interval 1 n)))

;auxilliary functions
(define (replace-elem new-elem pos items) ;replace item at position pos in items by new-elem
  (define (iter i res rem)
    (if (= i pos)
        (append res (list new-elem) (cdr rem))
        (iter (+ i 1) (append res (list(car rem))) (cdr rem)))) (iter 1 '() items))

(define (gen-zero-vector n) ;generate a list of zeros of length n
  (define (iter a res)
    (if (> a n)
        res
        (iter (+ a 1) (append res (list 0))))) (iter 1 '()))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (length items)
  (accumulate + 0 (map (lambda (x) 1) items)))

(define (accumulate op null-value seq)
  (if (null? seq)
      null-value
      (op (car seq) (accumulate op null-value (cdr seq)))))

      (define (enumerate-interval low high)
        (define (iter a b res)
          (if (> a b)
              res
              (iter (+ a 1) b (append res (cons a '())))))
              (iter low high '()))
