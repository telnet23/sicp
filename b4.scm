"problem 1. (a)"
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

"problem 1. (b)"
(define (product-i term a next b)
  (define (product-i-help term a next b accumulator)
    (if (> a b)
        accumulator
        (product-i-help term (next a) next b (* accumulator (term a)))))
  (product-i-help term a next b 1))

"problem 1. (c)"
(define (wallis-pi n) (* 2
                         (product-i (lambda (n) (* (/ (* 2 n)
                                                   (- (* 2 n) 1))
                                                (/ (* 2 n)
                                                   (+ (* 2 n) 1))))
                                 1 (lambda (n) (+ n 1)) n)))

"problem 2. (a)"
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))
(define (acc-sum term a next b)
  (accumulate + 0 term a next b))
(define (acc-product term a next b)
  (accumulate * 1 term a next b))

"problem 2. (b)"
(define (catalan n) (accumulate * 1 (lambda (k) (/ (+ n k) k)) 2 (lambda (k) (+ k 1)) n))

"problem 2. (c)"
(define (leibniz-pi n) (* 4 (accumulate + ; combiner
                                   0 ; null-value
                                   (lambda (k) (/ (expt -1 (- k 1)) (- (* 2 k) 1))) ; term
                                   1 ; a
                                   (lambda (k) (+ k 1)) ; next
                                   n))) ; b

"problem 2. (d)"
(define (accumulate-tr combiner null-value term a next b)
  (define (accumulate-tr-help combiner null-value term a next b acc)
    (if (> a b)
        acc
        (accumulate-tr-help combiner null-value term (next a) next b (combiner (term a) acc))))
  (accumulate-tr-help combiner null-value term a next b null-value))

"problem 2. (e)"
(define (fact n)
  (accumulate-tr * 1 (lambda (k) k) 1 (lambda (k) (+ k 1)) n))

"problem 2. (f)"
(define (e-to-x x n)
  (accumulate-tr + 1 (lambda (k) (/ (expt x k) (fact k))) 1 (lambda (k) (+ k 1)) n))

"problem 3. (a)"
(define (der f h)
  (lambda (x) (/ (- (f (+ x h))
                    (f x))
                 h)))

"problem 3. (b)"
(define (fun x) (+ (* 3 x x)
                   (* -2 x)
                   7))

"problem 3. (c)"
(define (nth-deriv f n h)
  (if (= n 0)
      f
      (nth-deriv (der f h) (- n 1) h)))

"problem 4. (a)"
(define (smooth f dx) (lambda (x) (/ (+ (f (- x dx))
                                        (f x)
                                        (f (+ x dx)))
                                     3)))

"problem 4. (b)"
(define (compose f g) (lambda (x) (f (g x))))
(define (repeated f n)
  (if (= n 0)
    (lambda (x) x)
    (compose (repeated f (- n 1)) f)))
(define (n-fold-smooth f dx n)
  (repeated (smooth f dx) n))

"problem 5."
(define (max-fg f g)
  (lambda (x) (if (> (f x) (g x))
                  (f x)
                  (g x))))

"problem 6."
(define (romberg f a b n m)
  (define (h n) (/ (- b a)
                   (expt 2 n)))
  (cond ((and (= n 0) (= m 0)) (* (h 1)
                                  (+ (f a)
                                           (f b))))
        ((and (not (= n 0)) (= m 0)) (+ (/ (romberg f a b (- n 1) 0)
                                           2)
                                        (* (h n) (accumulate-tr +
                                                             0
                                                             (lambda (k) (f (+ a
                                                                               (* (h n)
                                                                                  (- (* 2 k) 1)))))
                                                             1
                                                             (lambda (k) (+ k 1))
                                                             (expt 2 (- n 1))))))
        
        (else (+ (romberg f a b n (- m 1))
                 (/ (- (romberg f a b n (- m 1))
                       (romberg f a b (- n 1) (- m 1)))
                    (- (expt 4 m) 1))))))