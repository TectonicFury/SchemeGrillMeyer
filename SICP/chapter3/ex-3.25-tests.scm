;tests are incomplete
(define sbl (general-table (lambda (x y) (eq? x y))))
(define (tests)
  (if (not (eq? ((sbl 'lookup-proc) '()) '()))
      (error "TEST FAILED -- LOOKUP procedure on empty table using null as argument")
      "TEST PASSED");returns ()
  (if (not (eq? ((sbl 'lookup-proc) '(compound)) false)) ;returns false
      (error "TEST FAILED -- LOOKUP procedure using 'compound' key on empty table")
      "TEST PASSED")
  (if (not (eq? ((sbl 'insert-proc) '(compound molekule) 44) 'ok)) ;returns ok
      (error "TEST FAILED -- INSERT procedure failed in inserting data")
      "TEST PASSED")
  (if (not (eq? ((sbl 'insert-proc) '(compound molekule) 46) 'ok)) ;returns ok
          (error "TEST FAILED -- LOOKUP procedure giving wrong answer")
          "TEST PASSED"))
  (if (not (eq? ((sbl 'insert-proc) '(compound molekule) 44) 'ok)) ;returns ok
      (error "TEST FAILED -- INSERT procedure failed in inserting data")
      "TEST PASSED")
      (if (not (eq? ((sbl 'insert-proc) '(compound molekule) 44) 'ok)) ;returns ok
          (error "TEST FAILED -- INSERT procedure failed in inserting data")
          "TEST PASSED")
          (if (not (eq? ((sbl 'insert-proc) '(compound molekule) 44) 'ok)) ;returns ok
              (error "TEST FAILED -- INSERT procedure failed in inserting data")
              "TEST PASSED")
              (if (not (eq? ((sbl 'insert-proc) '(compound molekule) 44) 'ok)) ;returns ok
                  (error "TEST FAILED -- INSERT procedure failed in inserting data")
                  "TEST PASSED")
                  (if (not (eq? ((sbl 'insert-proc) '(compound molekule) 44) 'ok)) ;returns ok
                      (error "TEST FAILED -- INSERT procedure failed in inserting data")
                      "TEST PASSED")
; ok
((sbl 'insert-proc) '(compound molecule co2) 46) ;returns ok
((sbl 'insert-proc) '(chemical poisonous) 24) ;returns ok
((sbl 'lookup-proc) '(compound molekule)) ; should return 44
((sbl 'lookup-proc) '(compound molekule molecule)) ; should return false
((sbl 'insert-proc) '(compound) 44) ;returns ok
((sbl 'lookup-proc) '(compound)) ;should return 44
