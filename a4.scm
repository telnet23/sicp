"problem 1. (a)"
(define (lucas n) (cond ((= n 0) 2)
                        ((= n 1) 1)
                        ((> n 1) (+ (lucas (- n 1))
                                    (lucas (- n 2))))))
;(lucas 30)
;(lucas 35)
;(lucas 40)

"problem 1. (b)"
(define (fast-Lucas-help n k lucas-a lucas-b)
  (if (= n k)
      lucas-a
      (fast-Lucas-help n (+ k 1) (+ lucas-a lucas-b) lucas-a )))
(define (fast-Lucas n) (fast-Lucas-help n 1 1 2))

"       Recursive calls      Recursive calls made by"
"       made by (Lucas k)    (fast-Lucas-help k 1 1 2)"
"k=1    0                    0"
"k=2    2                    1"
"k=3    4                    2"
"k=4    8                    3"
"k=5    16                   4"
"k=6    24                   5"

"problem 2. (a)"
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
(define (harmonic n) (sum (lambda (x) (/ 1 x)) 1 (lambda (x) (+ x 1)) n))

"problem 2. (b)"
(define (sum-i term a next b accumulator)
  (if (> a b)
      accumulator
      (sum-i term (next a) next b (+ accumulator (term a)))))
(define (harmonic-i n) (sum-i (lambda (x) (/ 1 x)) 1 (lambda (x) (+ x 1)) n 0))

"problem 2. (c)"
(harmonic 1)
(harmonic-i 1)
(harmonic 50)
(harmonic-i 50)
(harmonic 100)
(harmonic-i 100)

"problem 3."
(define (compose f g) (lambda (x) (f (g x))))
((compose (lambda (x) (* x x)) (lambda (x) (+ x 1))) 6)

"problem 4."
(define (repeated f n)
  (if (= n 0)
    (lambda (x) x)
    (compose (repeated f (- n 1)) f)))

"problem 5."
(define (m91 x)
  (if (> x 100)
      (- x 10)
      ((repeated m91 91) (+ x 901))))
