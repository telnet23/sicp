"problem 1. (a)"
(define (count-positives lst)
  (define (iter l count)
    (cond ((= (length l) 0) count)
          ((> (car l) 0) (iter (cdr l) (+ count 1)))
          (else (iter (cdr l) count))))
  (iter lst 0))

"problem 1. (b)"
(define (multiply-list lst)
  (define (iter acc l)
    (if (= (length l) 0)
        acc
        (iter (* acc (car l)) (cdr l))))
  (iter 1 lst))

"problem 1. (c)"
(define (consecutive-ints a b)
  (define (iter l i)
    (if (< i a)
        l
        (iter (cons i l) (- i 1))))
  (iter (list) b))

"problem 1. (d)"
(define (consecutive-squares a b)
  (define (iter l i)
    (if (< i a)
        l
        (iter (cons (* i i) l) (- i 1))))
  (iter (list) b))

"problem 2."
(define (count-if f lst)
  (define (iter l count)
    (cond ((= (length l) 0) count)
          ((f (car l)) (iter (cdr l) (+ count 1)))
          (else (iter (cdr l) count))))
  (iter lst 0))

"problem 3. (a)"
(define (nth-filtered f n)
  (define (iter i count)
    (if (f i)
        (if (= count n)
            i
            (iter (+ i 1) (+ count 1)))
        (iter (+ i 1) count )))
  (iter 1 1))

"problem 3. (b)"
(define (min-value f a b)
  (if (< (abs (- b a)) 0.0001)
      (f a)
      (let ((m (/ (+ a b) 2)))
        (min (min-value f a m) (min-value f m b)))))