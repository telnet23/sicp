"problem 1. (a)"
(+ 11 13 27 -16 29 36 2)

"problem 1. (b)"
(* 12345679 241304201)

"problem 1. (c)"
(/ (+ 5 4 (- 2 (- 3 (/ 4 5)))) (* 3 (- 6 2) (- 2 7)))

"problem 2. (a)"
(define (my-abs x)
  (if (< x 0)
      (* -1 x)
      x))

"problem 2. (b)"
(define (f-to-c f)
  (* (/ 5 9) (- f 32)))
(define (c-to-f c)
  (+ (* (/ 9 5) c) 32))

"problem 2. (c)"
(define (discount p d)
  (* p (- 1 (/ d 100))))

"problem 2. (d)"
(define (tip b)
  (ceiling (* b 1.15)))

"problem 2. (e)"
(define (coat l w p?)
  (if p?
      (ceiling (/ (* l w) 400))
      (ceiling (/ (* l w) 500))))

"problem 2. (f)"
(define (vec-prod u v)
  (if (null? u)
      0
      (+ (* (car u) (car v))
         (vec-prod (cdr u) (cdr v)))))

"problem 2. (g)"
(define (perpendicular? u v)
  (= (vec-prod u v) 0))

"problem 3."
(define (towers-of-hanoi n source temp dest)
  (towers-of-hanoi n-1 source dest temp))

"problem 4."
(define (euclid-gcd m n)
  (cond ((= m 0) n)
        ((= n 0) m)
        ((> n m) (euclid-gcd m (- n m)))
        (else (euclid-gcd (- m n) n))))

"problem 5."
(define (repeated f n)
  (if (= n 0)
      (lambda (x) x)
      (lambda (x) (f ((repeated f (- n 1)) x)))))

"problem 6."
(define (pq-to-ab p q)
  (let* ((a (/ (- (cdr q)
                  (cdr p))
               (- (car q)
                  (car p))))
         (b (- (cdr q)
               (* a
                  (car p)))))
  (cons a b)))

"problem 7. (a)"

"problem 7. (b)"
(define (differences l)
  (if (null? (cdr l))
      '()
      (cons (- (cadr l)
               (car l))
            (differences (cdr l)))))

"problem 7. (c)"
(define (range l)
  (define (iterate min max rem)
    (cond ((null? rem) (cons min max))
          ((< (car rem) min) (iterate (car rem)
                                      max
                                      (cdr rem)))
          ((> (car rem) max) (iterate min
                                      (car rem)
                                      (cdr rem)))
          (else (iterate min
                         max
                         (cdr rem)))))
  (iterate (car l) (car l) (cdr l)))

"problem 8. (a)"
(define (maketree v left-tree  right-tree)
  (list v left-tree  right-tree ))
(define (value T)
  (car T))
(define (left T)
  (cadr T))
(define (right T)
  (caddr T))
(define (insert x T)
  (cond ((null? T)
         (make-tree x  '()  '()))
        ((eq? x (value T)) T)
        ((< x (value T))
         (make-tree (value T)
                    (insert x (left T))
                    (right T)))
        ((> x (value T))
         (make-tree (value T)
                    (left T)
                    (insert x (right T))))))

(define (count-one-child t)
  (cond ((null? t)
         0)
        ((and (null? (left t))
              (not (null? (right t))))
         (+ 1
            (count-one-child (right t))))
        ((and (null? (right t))
              (not (null? (left t))))
         (+ 1
            (count-one-child (left t))))
        (else
         (+ (count-one-child (left t))
            (count-one-child (right t))))))

"problem 8. (b)"
(define (count t v)
  (cond ((null? t)
         0)
        ((equal? (value t) v)
         (+ 1
            (count (left t) v)
            (count (right t) v)))
        (else
         (+ (count (left t) v)
            (count (right t) v)))))

"problem 9. (a), (b)"
(define (new-account initial-balance initial-password)
  (let ((balance initial-balance)
        (password initial-password)
        (interest-rate 0.01))
    (define (deposit f p)
      (if (not (equal? p password))
          "access denied!"
          (begin
            (set! balance
                  (+ balance f))
            balance)))
    (define (withdraw f p)
      (if (not (equal? p password))
          "access denied!"
          (begin
            (set! balance
                  (- balance f))
            balance)))
    (define (bal-inq p)
      (if (not (equal? p password))
          "access denied!"
          balance))
    (define (accrue)
      (begin (set! balance
                   (+  balance
                       (*  balance
                           1
                           interest-rate)))
             balance))
    (define (set-interest-rate r) (set! interest-rate r))
    (define (set-password p) (set! password p))
    (lambda (method)
      (cond ((eq? method 'deposit) deposit)
            ((eq? method 'withdraw) withdraw)
            ((eq? method 'balance-inquire) bal-inq)
            ((eq? method 'accrue) accrue)
            ((eq? method 'setrate) setrate)
            ((eq? method 'setpassword) set-password)))))
