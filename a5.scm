"problem 1. (a)"
(define (usd-to-bitcoin usd) (/ usd 3760.58))

"problem 1. (b)"
(define (energy-from-mass m) (let ((c 299792458)) (* m c c)))

"problem 1. (c)"
(define (triangle-area b h) (* 0.5 b h))

"problem 1. (d)"
(define (total-value p n d q)
  (+ (* 0.01 p)
     (* 0.05 n)
     (* 0.10 d)
     (* 0.25 q)))

"problem 2. (a)"
(define (rep-dec n)
  (define (iter i a)
    (if (= i n)
        a
        (iter (+ i 1) (+ a (/ 1 (expt 10 (+ i 1)))))))
  (iter 0 0))

"problem 2. (b)"
(define (rep-dec-2 n)
  (define (iter i a)
    (if (= i n)
        a
        (iter (+ i 1) (+ a (if (= i 0)
                               0
                               (/ (if (even? i) 5 4)
                                  (expt 10 (+ i 1))))))))
  (iter 0 0))

"problem 2. (c)"
(define (blocks h)
  (define (square x) (* x x))
  (define (ith-odd i) (+ (* 2 i) 1))
  (define (iter i a)
    (if (= i h)
        a
        (iter (+ i 1) (+ a (square (ith-odd i))))))
  (iter 0 0))

"problem 3."
(define (plants t)
  (define (square n) (* n n))
  (define (double n) (* 2 n))
  (if (= t 0)
      2
      (let ((cur (plants (- t 1))))
        (if (< cur 10000)
            (square cur)
            (double cur)))))

"problem 4."
(define (yawning k)
  (define (iter i a)
    (if (> i k)
        a
        (iter (+ i 1) (+ a (* (expt 2 i) 3)))))
  (iter 0 0))

"problem 5."
(define (sum-of-squares n)
  (define (square n) (* n n))
  (define (iter i j)
    (cond ((= (+ (square i) (square j)) n) #t)
          ((> (square i) n) #f)
          ((> (square j) n) (iter (+ i 1) 0))
          (else (iter i (+ j 1)))))
  (iter 0 0))
