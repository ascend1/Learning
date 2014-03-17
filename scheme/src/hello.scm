; as a variable
(define vhello "Hello world!")

; as a function
(define fhello (lambda () "Hello world!"))

; function with argument
(define hello (lambda (name) (string-append "Hello " name "!")))

; inc
(define (inc v) (+ v 1))

; dec
(define (dec v) (- v 1))