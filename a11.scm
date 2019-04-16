"problem 1. (a)"
(define (fact n)
  (let ((product 1)
        (count 0))
    (define (helper)
      (if (= count n)
          'done
          (begin (set! count (+ count 1))
                 (set! product (* product count))
                 (helper))))
    (helper)
    product))

"problem 1. (b)"
(define (hailstone n)
  (let ((seq (list n)))
    (define (helper)
      (if (= (car seq) 1)
          'done
          (begin (if (even? (car seq))
                     (set! seq (cons (/ (car seq) 2) seq))
                     (set! seq (cons (+ (* 3 (car seq)) 1) seq)))
                 (helper))))
    (helper)
    (reverse seq)))

"problem 2."
(define (new-account initial-balance)
  (let ((balance initial-balance)
        (rate 0.01))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (withdraw amount)
      (if (> amount balance)
          (string-append "Insufficent Funds: Withdrawal amount exceeds balance by "
                         (number->string (- amount balance)))
          (begin (set! balance (- balance amount))
                 balance)))
    (define (balance-inquire)
      balance)
    (define (accrue)
      (set! balance (+ balance (* initial-balance rate)))
      balance)
    (define (setrate r)
      (set! rate r))
    (lambda (method)
      (cond ((eq? method 'deposit) deposit)
            ((eq? method 'withdraw) withdraw)
            ((eq? method 'balance-inquire) balance-inquire)
            ((eq? method 'accrue) accrue)
            ((eq? method 'setrate) setrate)))))

"problem 3."
(define jane (new-account 200))
(define john (new-account 100))
((jane 'deposit) 50)
((john 'deposit) 100)
((jane 'setrate) 0.015)
((john 'setrate) 0.02)
((jane 'accrue))
((john 'accrue))
((jane 'balance-inquire))
((john 'balance-inquire))

"problem 4."
(define (make-stack)
  (let ((stack '()))
    (define (is-empty?)
      (null? stack))
    (define (push element)
      (set! stack (cons element stack)))
    (define (top)
      (car stack))
    (define (pop)
      (let ((save (car stack)))
        (set! stack (cdr stack))
        save))
    (lambda (method)
      (cond ((eq? method 'is-empty) is-empty?)
            ((eq? method 'push) push)
            ((eq? method 'top) top)
            ((eq? method 'pop) pop)))))

"problem 5."
(define (nconc! x y)
  (define (nconc-helper z)
    (if (null? (cdr z))
        z
        (nconc-helper (cdr z))))
  (cond ((and (null? x) (null? y)) '())
        ((null? x) "Error: x is null")
        (else (set-cdr! (nconc-helper x) y)
              x)))

(define a (list 1 2 3))
(define b (list 4 5 6))
(append a b)
a
b
(nconc! a a)
a
"a has been mutated because set-cdr! has been used"
b

"      +---+---+    +---+---+    +---+---+"
"a --> | 1 | *-+--> | 2 | *-+--> | 3 | \\ |"
"      +---+---+    +---+---+    +---+---+"
""
"      +---+---+    +---+---+    +---+---+"
"a --> | 1 | *-+--> | 2 | *-+--> | 3 | *-+--+"
"      +---+---+    +---+---+    +---+---+  |"
" +-----------------------------------------+"
" |    +---+---+    +---+---+    +---+---+"
" +--> | 1 | *-+--> | 2 | *-+--> | 3 | \\ |"
"      +---+---+    +---+---+    +---+---+"