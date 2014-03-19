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

; fact
(define (fact n) (if (or (negative? n) (zero? n))
                   "n must be an integer greater than 0"
                   (if (= n 1)
                     1
                     (* n (fact (- n 1))))))

; fact, tail recursive version
(define (factt n)
  (facttt n n))

(define (facttt t r)
  (if (= t 1)
    r
    (let ((tt (- t 1)))
      (facttt tt (* r tt)))))

; my-len
(define (my-len ls) (if (equal? ls '())
                      0
                      (+ 1 (my-len (cdr ls)))))

; sum
(define (sum ls) (if (equal? ls '())
                   0
                   (+ (car ls) (sum (cdr ls)))))

; list-delete
(define (list-delete ls x) (if (null? ls) 
                             '()
                             (let ((t (car ls)))
                               (if (= t x)
                                 (list-delete (cdr ls) x)
                                 (cons t (list-delete (cdr ls) x))))))

; list-index
(define (list-index ls x idx) 
  (if (null? ls)
    #f
    (let ((t (car ls)))
      (if (= t x)
        idx
        (list-index (cdr ls) x (+ idx 1))))))

; my-reverse, tail recursive version
(define (my-reverse ls)
  (my-reverse-rec ls '()))

(define (my-reverse-rec ls templs)
  (if (null? ls)
    templs
    (my-reverse-rec (cdr ls) (cons (car ls) templs))))

; range, named let
(define (range n)
  (let loop ((i (- n 1)) (ls '()))
    (if (= i -1)
      ls
      (loop (- i 1) (cons i ls)))))

; string to int, letrec version
(define (str2i str)
  (let ((ls (string->list str)))
    (letrec ((iter (lambda (lst res)
                     (if (null? lst)
                       res
                       (iter (cdr lst) (+ (* res 10) (- (char->integer (car lst)) 48)))))))
      (iter ls 0))))
