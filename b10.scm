(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream head tail)
     (cons head (delay tail)))))

(define (stream-car s) (car s))

(define (stream-cdr s) (force (cdr s)))

(define empty-stream? null?)

(define (stream-filter f s)
  (cond ((empty-stream? s)
         '())
        ((f (stream-car s))
         (cons-stream (stream-car s)
                      (stream-filter f (stream-cdr s))))
        (else
         (stream-filter f (stream-cdr s)))))

(define (stream-map f s)
  (if (empty-stream? s)
      '()
      (cons-stream (f (stream-car s))
                   (stream-map f (stream-cdr s)))))

(define (stream-scale k s)
  (stream-map (lambda (x) (* k x)) s))

(define (stream-nth n s)
  (if (= n 1)
      (stream-car s)
      (stream-nth (- n 1)
                  (stream-cdr s))))

(define (str-to-list s n)
  (if (or (empty-stream? s) (= n 0))
      '()
      (cons (stream-car s)
            (str-to-list (stream-cdr s)
                         (- n 1)))))

(define (add-streams s1 s2)
  (cond ((and (empty-stream? s1)
              (empty-stream? s2)) '())
        ((empty-stream? s1) s2)
        ((empty-stream? s2) s1)
        (else (cons-stream (+ (stream-car s1)
                              (stream-car s2))
                           (add-streams (stream-cdr s1)
                                        (stream-cdr s2))))))

(define (mult-streams s1 s2)
  (cond ((and (empty-stream? s1)
              (empty-stream? s2)) '())
        ((empty-stream? s1) s2)
        ((empty-stream? s2) s1)
        (else
         (cons-stream (* (stream-car s1)
                         (stream-car s2))
                      (mult-streams (stream-cdr s1)
                                    (stream-cdr s2))))))

(define (divide-streams s1 s2)
  (cond ((and (empty-stream? s1)
              (empty-stream? s2)) '())
        ((empty-stream? s1) s2)
        ((empty-stream? s2) s1)
        (else
         (cons-stream (/ (stream-car s1)
                         (stream-car s2))
                      (divide-streams (stream-cdr s1)
                                      (stream-cdr s2))))))

(define (partial-sums s)
  (if (empty-stream? s)
      '()
      (cons-stream (stream-car s)
                   (stream-map (lambda (x) (+ (stream-car s) x))
                               (partial-sums (stream-cdr s))))))

(define (integers-from a)
  (cons-stream a (integers-from (+ a 1))))

"problem 1."
(define (golden-stream)
  (define (stream x)
    (cons-stream x (stream (+ 1 (/ 1 x)))))
  (stream 2))

"problem 2. (a)"
(define (prime? n)
  (define (recurse k)
    (or (> (* k k) n)
        (and (not (= (modulo n k) 0))
             (recurse (+ k 1)))))
  (and (> n 1)
       (recurse 2)))
(define (primes)
  (stream-filter prime? (integers-from 2)))

"problem 2. (b)"
(define (mersenne-candidates)
  (stream-map (lambda (k) (- (expt 2 k) 1)) (primes)))

"problem 2. (c)"
(define (mersenne-primes)
  (stream-filter prime? (mersenne-candidates)))

"problem 3. (a)"
(define (imp-facts)
  (define (imp-facts-from current index)
    (let ((next (* current index)))
      (cons-stream next (imp-facts-from next (+ index 1)))))
  (cons-stream 1 (imp-facts-from 1 1)))

"problem 3. (b)"
(define (imp-neg-powers-of-2)
  (define (neg-powers-from value)
    (cons-stream value (neg-powers-from (/ value 2))))
  (neg-powers-from (/ 1 2)))

"problem 4. (a)"
(define (e-to-the-x-terms x)
  (divide-streams (stream-map (lambda (n) (expt x n))
                              (integers-from 0))
                  (imp-facts)))

"problem 4. (b)"
(define (e-to-the-x-approx x)
  (partial-sums (e-to-the-x-terms x)))

"problem 5. (a)"
(define (stream-merge s1 s2)
  (merge-weighted s1 s2 (lambda (x) x)))

"problem 5. (b)"
 (define (235-stream)
  (define (235-member? x)
    (cond ((= x 1) #t)
          ((= (modulo x 2) 0) (235-member? (/ x 2)))
          ((= (modulo x 3) 0) (235-member? (/ x 3)))
          ((= (modulo x 5) 0) (235-member? (/ x 5)))
          (else #f)))
 (stream-filter 235-member? (integers-from 1)))

"problem 6. (a)"
(define (merge-weighted s1 s2 w)
  (cond ((empty-stream? s1) s2)
        ((empty-stream? s2) s1)
        ((equal? (stream-car s1)
                 (stream-car s2))
         (merge-weighted (stream-cdr s1) s2 w))
        ((< (w (stream-car s1))
            (w (stream-car s2)))
         (cons-stream (stream-car s1)
                      (merge-weighted (stream-cdr s1) s2 w)))
        (else
         (cons-stream (stream-car s2)
                      (merge-weighted s1 (stream-cdr s2) w)))))

"problem 6. (b)"
(define (weighted-pairs s1 s2 w)
  (if (or (empty-stream? s1)
          (empty-stream? s2))
      '()
      (cons-stream (cons (stream-car s1) (stream-car s2))
                   (merge-weighted
                    (stream-map (lambda (x) (cons (stream-car s1) x)) (stream-cdr s2))
                    (weighted-pairs (stream-cdr s1) s2 w)
                    w))))

"problem 6. (c)"
(define (weighted-pairs-stream)
  (weighted-pairs (integers-from 1)
                  (integers-from 1)
                  (lambda (p) (+ (car p) (cdr p)))))

"problem 6. (d)"
(define (ramanujan-numbers)
  (define (w p) (+ (expt (car p) 3)
                   (expt (cdr p) 3)))
  (define (filter-and-convert wp)
    (if (= (w (stream-car wp))
           (w (stream-car (stream-cdr wp))))
        (cons-stream (w p)
                     (filter-and-convert (stream-cdr wp)))
          (filter-and-convert (stream-cdr wp))))
  (filter-and-convert (weighted-pairs (integers-from 1)
                                      (integers-from 1)
                                      w)))
