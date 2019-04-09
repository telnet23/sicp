"problem 1. (a)"
(define (explode x)
  (define (recurse remaining)
    (if (= remaining 0)
        (list)
        (cons (modulo remaining 10)
              (recurse (floor (/ remaining 10))))))
  (if (= x 0)
      (list 0)
      (reverse (recurse x))))

(define (explode x)
  (define (iterate remaining accumulator)
    (if (< remaining 10)
        (cons remaining accumulator)
        (iterate (quotient remaining 10) ; quotient is integer division
                 (cons (remainder remaining 10) accumulator))))
  (iterate x (list)))
    
"problem 1. (b)"
(define (implode l)
  (define (iterate remaining accumulator)
    (if (null? remaining)
        accumulator
        (iterate (cdr remaining)
                 (+ accumulator
                    (* (car remaining)
                       (expt 10 (- (length remaining) 1)))))))
  (iterate l 0))

"problem 1. (c)"
(define (sum-list list)
  (if (null? list)
      0
      (+ (car list)
         (sum-list (cdr list)))))
(define (has-property x)
  (let ((s (sum-list (explode x))))
    (= x (* s (implode (reverse (explode s)))))))

"problem 1. (d)"
(define (find sequence test n)
  (define (iter i count)
    (if (test (sequence i))
        (if (= count n)
            i
            (iter (+ i 1) (+ count 1)))
        (iter (+ i 1) count)))
  (iter 1 1))

"problem 1. (e)"
(find (lambda (x) x) has-property 1)
(find (lambda (x) x) has-property 2)
(find (lambda (x) x) has-property 3)
;(find (lambda (x) x) has-property 4)

"problem 2. (a)"
;(define (swap-first-two l)
;  (cons (car (cdr l)) (cons (car l) (cdr (cdr l)))))

"problem 2. (b)"
(define (swap-first-two l)
  (if (< (length l) 2)
      l
      (cons (car (cdr l)) (cons (car l) (cdr (cdr l))))))

"problem 2. (c)"
(define (bubble-up l)
  (if (null? (cdr l))
      l
      (let ((l (if (> (car l) (car (cdr l)))
                   (swap-first-two l)
                   l)))
          (cons (car l) (bubble-up (cdr l))))))

"problem 2. (d)"
(define (bubble-sort-aux l n)
  (if (= n 0)
      l
      (bubble-sort-aux (bubble-up l) (- n 1))))

"problem 2. (e)"
(define (bubble-sort l)
  (bubble-sort-aux l (length l)))

(define (bubble-sort lst)
  (define (bubble-up l)
    (cond ((or (null? l) (null? (cdr l))) l)
          ((> (car l) (cadr l)) (cons (cadr l)
                                      (bubble-up (cons (car l)
                                                       (cddr l)))))
          (else (cons (car l)
                      (bubble-up (cdr l))))))
  (define (bubble-aux l i)
    (if (= i 0)
        l
        (bubble-aux (bubble-up l) (- i 1))))
  (bubble-aux lst (length lst)))