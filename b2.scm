"problem 1."
(define (piecewise x) (if (and (>= x -2)
                               (<= x  2))
                          (- 4 (* x x))
                          (- (* x x) 4)))

"problem 2. (a)"
(define (odd-sum n) (if (= n 0)
                        0
                        (+ (- (* 2 n) 1)
                           (odd-sum (- n 1)))))

"problem 2. (b)"
(odd-sum 1)
(odd-sum 2)
(odd-sum 3)
(odd-sum 4)
(odd-sum 5)
(odd-sum 6)
(odd-sum 7)

"problem 2. (c)"
(define (sum-from-to a b) (if (= a b)
                              a
                              (+ b
                                 (sum-from-to a (- b 1)))))

"problem 3."
(define (apery k) (if (= k 0)
                      0
                      (+ (expt k -3)
                         (apery (- k 1)))))

"problem 4."
(define (k-product k) (if (= k 1)
                          1
                          (* (k-product (- k 1)) (- 1 (expt k -2)))))

"problem 5. (a)"
(define (babylonian x k) (if (= k 0)
                             (/ x 2)
                             (/ (+ (babylonian x (- k 1))
                                   (/ x
                                      (babylonian x (- k 1))))
                                2)))

"problem 5. (b)"
(define (first-value-k-or-higher x tol k) (if (<= (abs (- (expt (babylonian x k) 2)
                                                          x))
                                                  tol)
                                              k
                                              (first-value-k-or-higher x tol (+ k 1))))
(define (terms-needed x tol) (first-value-k-or-higher x tol 1))

"problem 5. (c)"
(define (x S n)
  (if (= n 0)
    (/ S 2)
    (- (+ (x S (- n 1))
          (/ (- S (expt (x S (- n 1)) 2))
             (* 2 (x S (- n 1)))))
       (/ (expt (/ (- S (expt (x S (- n 1)) 2))
                   (* 2 (x S (- n 1))))
                2)
          (* 2
             (+ (x S (- n 1))
                (/ (- S (expt (x S (- n 1)) 2))
                   (* 2 (x S (- n 1))))))))))
(define (bakhshali S k) (x S k))

"problem 5. (d)"
(define (first-value-k-or-higher-b x tol k) (if (<= (abs (- (expt (bakhshali x k) 2)
                                                          x))
                                                  tol)
                                              k
                                              (first-value-k-or-higher-b x tol (+ k 1))))
(define (terms-needed-b x tol) (first-value-k-or-higher-b x tol 1))

"problem 6."
(define (factorial n) (if (= n 0)
                          1
                          (* n
                             (factorial (- n 1)))))
(define (new-sin-term x k) (* (expt -1 k)
                              (/ (expt x
                                       (+ (* 2 k) 1))
                                 (factorial (+ (* 2 k) 1)))))
(define (new-sin x n) (if (= n 0)
                      (new-sin-term x n)
                      (+ (new-sin-term x n)
                         (new-sin x (- n 1)))))
