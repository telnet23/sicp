"problem 1."
(define (factorial n) (if (= n 0)
                          1
                          (* n
                             (factorial (- n 1)))))
(define (n-choose-k n k) (if (or (< n k)
                                 (< k 0))
                             0
                             (/ (factorial n)
                                (* (factorial (- n k))
                                   (factorial k)))))

"problem 2."
(define (pow b e) (if (= e 0)
                      1
                      (* b
                         (pow b (- e 1)))))

"problem 3."
(define (zeno n) (if (= n 1)
                     (/ 1 2)
                     (+ (zeno (- n 1))
                        (/ 1 (pow 2 n)))))

"problem 4."
(define (num-digits n) (if (= (floor n) 0) 0 (+ (num-digits (/ n 10)) 1)))

"problem 5. (a)"
(define (a n) (pow 2 n))

"problem 5. (b)"
(define (num-ancestors n) (if (= n 1)
                              (a 1)
                              (+ (num-ancestors (- n 1))
                                 (a n))))