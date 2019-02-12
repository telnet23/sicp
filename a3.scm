"problem 1. (a)"
(define (candy-left daynum pieces) (if (= daynum 1)
                                       (* pieces (- 1 (/ 1 2)))
                                       (* (candy-left (- daynum 1) pieces)
                                          (- 1 (/ 1 daynum)))))

"problem 1. (b)"

(define (candy-left-discrete daynum pieces)
  (define prev (if (= daynum 1) 0 (candy-left-discrete (- daynum 1) pieces)))
  (if (= daynum 1)
      (/ pieces 2)
      (- prev (if (= (/ prev daynum) (floor (/ prev daynum)))
                  (/ prev daynum)
                  (floor (+ (/ prev daynum) 1))))))

"problem 2. (a)"
(define (pell-num n) (cond ((= n 0) 0)
                           ((= n 1) 1)
                           (#t (+ (* 2 (pell-num (- n 1)))
                                  (pell-num (- n 2))))))

"problem 2. (b)"
(define (comp-pell-num n) (if (or (= n 0) (= n 1))
                              2
                              (+ (* 2 (comp-pell-num (- n 1)))
                                 (comp-pell-num (- n 2)))))

"problem 2. (c)"
(define (sqrt-2-approx n) (/ (/ (comp-pell-num n) 2)
                             (pell-num n)))

"problem 3. (a)"
(define (square a) (* a a))
(define (fastexp b e) (cond ((= e 0) 1)
                            ((even? e) (square (fastexp b (/ e 2))))
                            ((odd? e) (* b (square (fastexp b (/ (- e 1) 2)))))))

"problem 3. (b)"
"n multiplications vs. sqrt(n) mulitplications"

"problem 4. (a)"
(define (cont-frac k x) (if (= k 0)
                            0
                            (/ (- x 1)
                               (+ 2 (cont-frac (- k 1) x)))))

"problem 4. (b)"
(define (new-sqrt x n) (+ 1 (cont-frac n x)))

(abs (- (new-sqrt 2 1) (sqrt 2)))
(abs (- (new-sqrt 2 10) (sqrt 2)))
(abs (- (new-sqrt 2 100) (sqrt 2)))

(abs (- (new-sqrt 64 1) (sqrt 64)))
(abs (- (new-sqrt 64 10) (sqrt 64)))
(abs (- (new-sqrt 64 100) (sqrt 64)))