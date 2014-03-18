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

; abs
(define my-abs (lambda (v) (if (positive? v) v (- 0 v))))

; i2a
(define (i2a n) (if (<= 33 n 126) (integer->char n) #f))

; pos-mul
(define (pos-mul a b c) (if (and (positive? a) (positive? b) (positive? c)) 
                          (* a b c) 
                          "non-positive number detected."))

; neg-mul
(define (neg-mul a b c) (if (or (negative? a) (negative? b) (negative? c)) 
                          (* a b c)
                          "at least one negative number."))

; grade
(define (grade score)
  (cond
    ((<= 80 score 100) "A")
    ((<= 60 score 79) "B")
    ((<= 40 score 59) "C")
    ((<= 0 score 39) "D")
    (else "Illegal input")
    ))

; quadraic-eq-solver
(define (qe-solver a b c)
  (if (zero? a)
    (- 0 (/ c b))
    (let ((d (- (* b b) (* 4 a c))))
      (if (negative? d)
        "no rational solution"
        (let ((e (/ b a -2)))
          (if (zero? d)
            (list e)
            (let ((f (/ (sqrt d) a 2))) (list (+ e f) (- e f)))))))))
