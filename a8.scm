"problem 1."
(define (has-duplicates? l)
  (and (not (null? l))
       (or (list? (member (car l) (cdr l)))
           (has-duplicates? (cdr l)))))

"problem 2."
(define (num-zeroes l)
  (define (loop rem acc)
    (cond ((null? rem) acc)
          ((list? (car rem)) (loop (cdr rem)
                                   (+ acc (num-zeroes (car rem)))))
          ((equal? (car rem) 0) (loop (cdr rem)
                                      (+ acc 1)))
          (else (loop (cdr rem)
                      acc))))
  (loop l 0))

"problem 3. (a)"
(define (insert item lst precedes)
  (if (or (null? lst) (precedes item (car lst)))
      (cons item lst)
      (cons (car lst) (insert item (cdr lst) precedes))))

(define (insert item lst precedes)
  (define (iterate acc rem)
    (cond ((or (null? rem) (precedes item (car rem)))
           (append acc (list item) rem))
          (else
           (iterate (append acc (list (car rem))) (cdr rem)))))
  (iterate '() lst))

"problem 3. (b)"
(define (insert-all lst precedes)
  (define (loop rem acc)
    (if (null? rem)
        acc
        (loop (cdr rem) (insert (car rem) acc precedes))))
  (loop lst '()))

"problem 4."
(define (make-tree value left right)
  (list value left right))
(define (value tree)
  (car tree))
(define (left tree)
  (cadr tree))
(define (right tree)
  (caddr tree))
(define empty-tree?
  null?)
(define testtree
  (make-tree 1
             (make-tree 3
                        (make-tree 7 '() '())
                        (make-tree 9 '() '()))
             (make-tree 5 '() '())))

"problem 4. (a)"
(define (tree-node-count t)
  (if (empty-tree? t)
      0
      (+ 1
         (tree-node-count (left t))
         (tree-node-count (right t)))))

"problem 4. (b)"
(define (tree-node-sum t)
  (if (empty-tree? t)
      0
      (+ (value t)
         (tree-node-sum (left t))
         (tree-node-sum (right t)))))

"problem 4. (c)"
(define (tree-height t)
  (if (empty-tree? t)
      -1
      (+ 1
         (max (tree-height (left t))
              (tree-height (right t))))))

"problem 4. (d)"
(define (tree-map f t)
  (if (empty-tree? t)
      t
      (make-tree (f (value t))
                 (tree-map f (left t))
                 (tree-map f (right t)))))