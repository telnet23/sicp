"problem 1. (a)"
(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (fold-right op initial (cdr sequence)))))

"problem 1. (b)"
(define (fold-left op initial sequence)
  (define (iter rem acc)
    (if (null? rem)
      acc
      (iter (cdr rem) (op (car rem) acc))))
  (iter sequence initial))

"problem 1. (c)"
(define (my-map p sequence)
  (fold-right (lambda (x y) (cons (p x) y)) '() sequence))

"problem 1. (d)"
(define (my-append seq1 seq2)
  (fold-right cons seq2 seq1))

"problem 1. (e)"
(define (my-length sequence)
  (fold-right (lambda (x y) (+ y 1)) 0 sequence))

"problem 1. (f)"
(define (reverse-r sequence)
  (fold-right (lambda (x y) (my-append y (list x))) '() sequence))

"problem 1. (g)"
(define (reverse-l sequence)
  (fold-left (lambda (x y) (cons x y)) '() sequence))

"problem 1. (h)"
(define (horner-eval x coefficient-list)
  (fold-left (lambda (first acc) (+ first (* x acc))) 0 (reverse-l coefficient-list)))

"problem 2. (a)"
(define (explode x)
  (define (recurse remaining)
    (if (= remaining 0)
        (list)
        (cons (modulo remaining 10)
              (recurse (floor (/ remaining 10))))))
  (if (= x 0)
      (list 0)
      (reverse (recurse x))))

"problem 2. (b)"
(define (implode l)
  (define (iter remaining accumulator)
    (if (null? remaining)
        accumulator
        (iter (cdr remaining)
              (+ accumulator
                 (* (car remaining)
                    (expt 10 (- (length remaining) 1)))))))
  (iter l 0))

"problem 2. (c) i."
(define (prime? n)
  (define (divisor? k) (= 0 (modulo n k)))
  (define (divisors-upto k) (and (> k 1)
                                 (or (divisor? k)
                                     (divisors-upto (- k 1)))))
  (and (> n 1)
       (not (divisors-upto (- n 1)))))
(define (left-truncatable-prime? p)
  (define (recurse exploded)
    (or (null? exploded) 
        (and (not (= (car exploded) 0))
             (prime? (implode exploded))
             (recurse (cdr exploded)))))
  (recurse (explode p)))

"problem 2. (c) ii."
(define (find sequence test n)
  (define (iter i count)
    (display i) (newline)
    (if (test (sequence i))
        (if (= count n)
            i
            (iter (+ i 1) (+ count 1)))
        (iter (+ i 1) count)))
  (iter 1 1))
(define (nth-left-trunc-prime n)
  (find (lambda (i) i) left-truncatable-prime? n))

"problem 2. (d) i."
(define (right-truncatable-prime? p)
  (define (recurse exploded)
    (or (null? exploded)
        (and (prime? (implode (reverse exploded)))
             (recurse (cdr exploded)))))
  (recurse (reverse (explode p))))

"problem 2. (d) ii."
(define (nth-right-trunc-prime n)
  (find (lambda (i) i) right-truncatable-prime? n))

"problem 2. (e) i."
(define (two-sided-prime? p)
  (and (left-truncatable-prime? p)
       (right-truncatable-prime? p)))
(define (nth-two-sided-prime n)
  (find (lambda (i) i) two-sided-prime? n))

"problem 3. (a)"
(define (split l)
  (define (iter acc rem)
    (if (>= (length acc) (length rem))
        (cons (reverse acc) rem)
        (iter (cons (car rem) acc) (cdr rem))))
  (iter '() l))

"problem 3. (b)"
(define (merge l1 l2)
  (display l1) (display " ") (display l2) (newline)
  (cond ((null? l1) l2)
        ((null? l2) l1)
        ((< (car l1) (car l2)) (cons (car l1) (merge (cdr l1) l2)))
        (else (cons (car l2) (merge l1 (cdr l2))))))

"problem 4. (c)"
(define (mergesort l)
  (if (or (null? l) (null? (cdr l)))
      l
      (let ((spl (split l)))
        (merge (mergesort (car spl))
               (mergesort (cdr spl))))))