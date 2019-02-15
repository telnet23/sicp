"problem 1. (a)"
(define (harmonic n) (if (= n 1)
                         1
                         (+ (harmonic (- n 1))
                            (/ 1 n))))

"problem 1. (b)"
(define (euler-approx n) (- (harmonic n) (log n)))

"problem 2. (a)"
(define (prime? n)
  (define (divisor? k) (= 0 (modulo n k)))
  (define (divisors-upto k) (and (> k 1)
                                 (or (divisor? k)
                                     (divisors-upto (- k 1)))))
  (and (> n 1)
       (not (divisors-upto (- n 1)))))

(define (next-prime-after n) (if (prime? (+ n 1))
                                 (+ n 1)
                                 (next-prime-after (+ n 1))))

"problem 2. (b)"
(define (kth-prime-after n k) (if (prime? (+ n 1))
                                  (if (= k 1)
                                      (+ n 1)
                                      (kth-prime-after (+ n 1) (- k 1)))
                                  (kth-prime-after (+ n 1) k)))

"problem 2. (c)"
(define (kth-prime k) (kth-prime-after 1 k))

"problem 3. (a)"
(define (fast-expt b e) (cond ((= e 0) 1)
                              ((even? e) (fast-expt (* b b) (/ e 2)))
                              ((odd? e) (* b (fast-expt (* b b) (/ (- e 1) 2))))))

"problem 3. (b)"
(define (root-converge n x a b tol) (if (< (abs (- a b)) tol)
                                        a
                                        (let ((avg (/ (+ a b) 2)))
                                          (if (> (fast-expt avg n) x)
                                            (root-converge n x a avg tol)
                                            (root-converge n x avg b tol)))))
(define (nth-root-approx x n tol) (root-converge n x 1 x tol))

"problem 3. (c)"
(define (nth-root-of-n-approx n tol) (nth-root-approx n n tol))
(nth-root-of-n-approx 1 0.00001)
(nth-root-of-n-approx 10 0.00001)
(nth-root-of-n-approx 100 0.00001)
(nth-root-of-n-approx 1000 0.00001)
(nth-root-of-n-approx 10000 0.00001)
(define (nth-root-of-n-has-limit?) #t)

"problem 4. (a)"
(define (golden n) (if (= n 0)
                       1
                       (+ 1 (/ 1 (golden (- n 1))))))

"problem 4. (b)"
(define (golden-sqrt n) (if (= n 0)
                            1
                            (sqrt (+ 1 (golden-sqrt (- n 1))))))

"problem 5. (a)"
(define (random-int k) (inexact->exact (ceiling (* (random)
                                                   k))))

"problem 5. (b)"
(define (random-int-range a b) (inexact->exact (ceiling (+ (- a 1)
                                                           (* (random)
                                                                  (- b
                                                                     (- a 1)))))))

"problem 6. (a)"
(define (mod-test n a) (= (modulo (fast-expt a n)
                                  n)
                          a))

"problem 6. (b)"
(define (fermat-test n) (mod-test n (random-int-range 2 (- n 1))))

"problem 6. (c)"
(define (fermat-prime? n k) (cond ((= k 1) #t)
                                  ((< n 2) #f)
                                  ((= n 2) #t)
                                  ((> n 2) (if (fermat-test n)
                                               (fermat-prime? n (- k 1))
                                               #f))))

"problem 6. (d)"
(define (count-fermat-positives n a count) (if (= a 1)
                                             count
                                             (if (mod-test n a)
                                                 (count-fermat-positives n (- a 1) (+ count 1))
                                                 (count-fermat-positives n (- a 1) count))))
(define (proportion-fermat-positives n) (/ (count-fermat-positives n (- n 1) 0)
                                           (- n 2)))

"problem 7. (a)"
(define (throw-one-die) (random-int-range 1 6))

"problem 7. (b)"
(define (dice-throw) (+ (throw-one-die)
                        (throw-one-die)))

"problem 7. (c)"
(define (make-point point) (let ((throw (dice-throw)))
                                (cond ((= throw point) #t)
                                      ((= throw 7) #f)
                                      (else (make-point point)))))

"problem 7. (d)"
(define (make-pass) (let ((throw (dice-throw)))
                         (cond ((or (= throw 7) (= throw 11)) #t)
                               ((or (= throw 2) (= throw 3) (= throw 12)) #f)
                               (else (make-point throw)))))

"problem 7. (e)"
(define (count-win-odds n count) (cond ((= n 0) count)
                                       ((make-pass) (count-win-odds (- n 1) (+ count 1)))
                                       (else (count-win-odds (- n 1) count))))
(define (win-odds n) (/ (count-win-odds n 0) n))
(win-odds 1000)
"it looks like each player wins with probablity 0.5"
