;ex 3.13 average of 3 numbers
;grill meyers book exercises
(define (avg_of_3 x1 x2 x3)
(/
(+ x1 x2 x3)
3))
(avg_of_3 2.0 3 6)

;ex 3.14 function taking 5 arguments and returns average of mid 3

(define (avg_of_mid_3 x1 x2 x3 x4 x5)
(/(+ x1 x2 x3 x4 x5 (- (max x1 x2 x3 x4 x5)) (- (min x1 x2 x3 x4 x5))) 3))

(avg_of_mid_3 1 2 3 4 500)

;ex 3.15 fahrenheit to celsius
(define (fahrenheit_to_celsius temp_in_f) (/ (* (- temp_in_f 32) 5) 9))

(define t (fahrenheit_to_celsius 212))

;ex 3.16 celsius to fahrenheit
(define (celsius_to_fahrenheit temp_in_c) (+ (/ (* temp_in_c 9) 5) 32))

(celsius_to_fahrenheit t)

;ex 3.21
(define (palindrome_year year)
  (let (
    ( first_two (- year 1) )
    ( third (remainder (- year 1) 10) )
    ( fourth (/ year 10) )
    ))
       (+ (* first_two 100) (* third 10) fourth))

(palindrome_year 20)
