"problem 1."
(define (filter f lst)
  (cond ((null? lst) (list))
        ((f (car lst)) (cons (car lst)
                             (filter f (cdr lst))))
        (else (filter f (cdr lst)))))

"problem 2."
(define (fun-not f)
  (lambda (a) (not (f a))))

"problem 3."
(define (map-consecutive f a b)
    (if (> a b)
        (list)
        (cons (f a) (map-consecutive f (+ a 1) b))))

"problem 4. (a)"
(define (is-prefix? x y)
  (or (null? x)
      (and (not (null? y))
           (= (car x) (car y))
           (is-prefix? (cdr x) (cdr y)))))

"problem 4. (b)"
(define (is-sublist? x y)
  (or (is-prefix? x y)
      (and (not (null? y))
           (is-sublist? x (cdr y)))))

"problem 4. (c)"
(define (longest-common-prefix x y)
  (if (or (null? x)
          (null? y)
          (not (= (car x) (car y))))
      (list)
      (cons (car x) (longest-common-prefix (cdr x) (cdr y)))))

"problem 5."
(define (nested-reverse lst)
  (define (iterate accumulator remaining)
    (if (null? remaining)
        accumulator
        (iterate (cons (if (list? (car remaining))
                           (iterate (car remaining) (list))
                           (car remaining))
                       accumulator)
                 (cdr remaining))))
  (iterate lst (list)))

"problem 6. (a)"
(define (every? f lst)
  (or (null? lst)
      (and (f (car lst))
           (every? f (cdr lst)))))

"problem 6. (b)"
(define (some? f lst)
  (and (not (null? lst))
       (or (f (car lst))
           (some? f (cdr lst)))))

"problem 6. (c)"
(define (notany? f lst)
  (not (some? f lst)))

"problem 7. (a)"
(define (position lst v)
  (define (loop i remaining)
    (cond ((null? remaining) -1)
          ((equal? (car remaining) v) i)
          (else (loop (+ i 1) (cdr remaining)))))
  (loop 1 lst))

"problem 7. (b)"
(define (value-at-position lst k)
  (define (loop i remaining)
    (cond ((null? remaining) "list too short")
          ((= i k) (car remaining))
          (else (loop (+ i 1) (cdr remaining)))))
  (loop 1 lst))

"problem 7. (c)"
(define (prime? n)
  (define (divisor a) (= (modulo n a) 0))
  (define (smooth k)
    (and (>= k 2)
         (or (divisor k)
             (smooth (- k 1)))))
  (and (> n 1)
       (not (smooth (floor (sqrt n))))))

(define (nth-prime-between a b n)
  (value-at-position (filter prime? (map-consecutive (lambda (x) x) a b)) n))

"problem 8. (a)"
(define (tailmap f lst)
  (if (null? lst)
      (list)
      (cons (f lst) (tailmap f (cdr lst)))))

"problem 8. (b)"
(define (reduce f lst init)
  (if (null? lst)
      init
      (f (car lst) (reduce f (cdr lst) init))))