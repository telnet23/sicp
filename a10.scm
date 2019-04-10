"problem 1."
(define (same-length l1 l2)
  (cond ((and (null? l1) (null? l2)) #t)
        ((or (null? l1) (null? l2)) #f)
        (else (same-length (cdr l1) (cdr l2)))))

"problem 2."
(define (isomorphic? l1 l2)
  (or (and (integer? l1)
           (integer? l2))
      (and (null? l1)
           (null? l2)
           (list? l1)
           (list? l2))
      (and (not (null? l1))
           (not (null? l2))
           (list? l1)
           (list? l2)
           (isomorphic? (car l1) (car l2))
           (isomorphic? (cdr l1) (cdr l2)))))

"problem 3."
(define (average l)
  (define (sum-len sum-acc len-acc rem)
    (if (null? rem)
        (cons sum-acc len-acc)
        (sum-len (+ sum-acc (car rem))
                 (+ len-acc 1)
                 (cdr rem))))
  (let ((sum-len-pair (sum-len 0 0 l)))
    (/ (car sum-len-pair)
       (cdr sum-len-pair))))

"problem 4."
(define (filter f l)
  (cond ((null? l) l)
        ((f (car l)) (cons (car l) (filter f (cdr l))))
        (else (filter f (cdr l)))))

"problem 5."
(define (new-append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (new-append (cdr l1) l2))))

"problem 6."
(define (remove-zeros l)
  (filter (lambda (x) (not (= x 0))) l))

"problem 7."
(define (list-sum l)
  (if (null? l)
      0
      (+ (car l) (list-sum (cdr l)))))

"problem 8."
(define (intersection l1 l2)
  (cond ((or (null? l1) (null? l2)) (list))
        ((member (car l1) l2) (cons (car l1) (intersection (cdr l1) l2)))
        (else (intersection (cdr l1) l2))))

"problem 9."
(define (replace find rplc l)
  (cond ((null? l) l)
        ((equal? (car l) find) (cons rplc (cdr l)))
        (else (cons (car l) (replace find rplc (cdr l))))))

"problem 10."
(define (replace-all find rplc l)
  (cond ((null? l) l)
        ((equal? (car l) find) (cons rplc (replace-all find rplc (cdr l))))
        (else (cons (car l) (replace-all find rplc (cdr l))))))

"problem 11."
(define (new-reverse l)
  (if (null? l)
      l
      (new-append (new-reverse (cdr l)) (list (car l)))))
(define (new-equal? l1 l2)
  (or (and (null? l1)
           (null? l2))
      (and (not (null? l1))
           (not (null? l2))
           (equal? (car l1) (car l2))
           (new-equal? (cdr l1) (cdr l2)))))
(define (palindrome? l)
  (equal? (reverse l) l))

"problem 12."
(define (make-tree v left-tree right-tree)
  (list v left-tree right-tree))
(define (value T) (car T))
(define (left T) (cadr T))
(define (right T) (caddr T))
(define (bst-insert item bs-tree same? less-than?)
  (display "trying to insert ")(display item)(display " into ")(display bs-tree)(newline)
  (cond ((null? bs-tree) (make-tree item '() '()))
        ((same? item (value bs-tree)) bs-tree)
        ((less-than? item (value bs-tree)) (make-tree (value bs-tree)
                                                      (bst-insert item (left bs-tree) same? less-than?)
                                                      (right bs-tree)))
        (else (make-tree (value bs-tree)
                         (left bs-tree)
                         (bst-insert item (right bs-tree) same? less-than?)))))
(define (build-tree cur-tree rem-items)
  (if (null? rem-items)
      cur-tree
      (build-tree (bst-insert (car rem-items) cur-tree = <) (cdr rem-items))))

(define (count-ge-bst tree z)
  (cond ((null? tree) 0)
        ((< (value tree) z) (count-ge-bst (right tree) z))
        ((= (value tree) z) (+ 1
                               (count-ge-bst (right tree) z)))
        ((> (value tree) z) (+ 1
                               (count-ge-bst (left tree) z)
                               (count-ge-bst (right tree) z)))))

"problem 13."
(define (create-heap h-min left right) (list h-min left right))
(define (h-min h) (car h))
(define (left h) (cadr h))
(define (right h) (caddr h))
(define (insert x h)
  (cond ((null? h) (create-heap x
                                (list)
                                (list)))
        ((= x (h-min h)) h)
        ; assumes x is not already in heap
        ; swap left and right to keep heap balanced
        ((> x (h-min h)) (create-heap (h-min h)
                                      (right h)
                                      (insert x (left h))))
        ((< x (h-min h)) (create-heap x
                                      (right h)
                                      (insert (h-min h) (left h))))))
(define (list-to-heap l)
  (if (null? l)
      l
      (insert (car l) (list-to-heap (cdr l)))))

(define (list-to-heap-tr acc rem)
  (if (null? rem)
      acc
      (list-to-heap-tr (insert (car rem) acc) (cdr rem))))

"problem 14."
(define (count-le-heap h z)
  (cond ((null? h) 0)
        ((> (h-min h) z) 0)
        ((= (h-min h) z) 1)
        ((< (h-min h) z) (+ 1
                            (count-le-heap (left h) z)
                            (count-le-heap (right h) z)))))

"problem 15. 1."
(define (sum-polynomials p1 p2)
  (define (iterate sum-acc p1-rem p2-rem)
    (cond ((and (null? p1-rem) (null? p2-rem)) sum-acc)
          ((null? p1-rem) (iterate (cons (car p2-rem) sum-acc)
                                   p1-rem
                                   (cdr p2-rem)))
          ((null? p2-rem) (iterate (cons (car p1-rem) sum-acc)
                                   (cdr p1-rem)
                                   p2-rem))
          (else (iterate (cons (+ (car p1-rem) (car p2-rem)) sum-acc)
                         (cdr p1-rem)
                         (cdr p2-rem)))))
  (iterate (list) (reverse p1) (reverse p2)))

"problem 15. 2."
(define (evaluate p x)
  (define (iterate sum power rem)
    (if (null? rem)
        sum
        (iterate (+ sum
                    (* (car rem)
                       (expt x power)))
                 (+ power 1)
                 (cdr rem))))
  (iterate 0 0 (reverse p)))

"problem 15. 3."
(define (convert-to-fn p)
  (lambda (x) (evaluate p x)))