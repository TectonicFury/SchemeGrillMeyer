(define (make-deque) (cons '() '()))

(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))

(define (set-front-ptr! deque item)
  (set-car! deque item))
(define (set-rear-ptr! deque item)
  (set-cdr! deque item))

(define (front-deque deque)
  (cond ((empty-deque? deque)
          (error "FRONT called on empty deque" deque))
          (else (cadr (front-ptr deque)))))

(define (rear-deque deque)
  (cond ((empty-deque? deque)
          (error "REAR called on empty deque" deque))
        (else (cadr (rear-ptr deque)))))

(define (front-insert-deque! deque item)
  (let ((new-entry (cons '() (cons item '()))))
  (cond ((empty-deque? deque)
          (set-front-ptr! deque new-entry)
          (set-rear-ptr! deque new-entry)
          (print-deque deque))
          (else
            (set-cdr! (cdr new-entry) (front-ptr deque))
            (set-car! (front-ptr deque) new-entry)
            (set-front-ptr! deque new-entry)
            (print-deque deque)))))

(define (rear-insert-deque! deque item)
  (let ((new-entry (cons '() (cons item '()))))
    (cond ((empty-deque? deque)
            (set-front-ptr! deque new-entry)
            (set-rear-ptr! deque new-entry)
            (print-deque deque))
          (else
            (set-cdr! (cdr (rear-ptr deque)) new-entry)
            (set-car! new-entry (rear-ptr deque))
            (set-rear-ptr! deque new-entry)
            (print-deque deque)))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
          (error "FRONT --DELETE called on empty deque" deque))
        (else
          (set-front-ptr! deque (cddr (front-ptr deque)))
          (print-deque deque))))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
          (error "REAR --DELETE called on empty deque" deque))
        (else
          (set-rear-ptr! deque (car (rear-ptr deque)))
          (set-cdr! (cdr (rear-ptr deque)) '())
          (print-deque deque))))

(define (print-deque deque)
  (define (iter item-pair)
    (cond ((null? item-pair) (newline))
          (else (display (car (cdr item-pair))) (display " ") (iter (cddr item-pair)))))
  (iter (front-ptr deque)))

(define (empty-deque? deque)
  (eq? (car deque) '()))
