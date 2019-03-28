(define (symbol<? a b) (string<? (symbol->string a) (symbol->string b)))
(define (make-tree value left right) (list value left right))
(define (value tree) (car tree))
(define (left tree) (cadr tree))
(define (right tree) (caddr tree))
(define empty-tree? null?)
(define example
  (make-tree
   '+
   (make-tree
    '*
    (make-tree
     '-
     (make-tree 0
                '()
                '())
     (make-tree 2
                '()
                '()))
    (make-tree 3
               '()
               '()))
   (make-tree
    '*
    (make-tree
     '-
     (make-tree 4
                '()
                '())
     (make-tree 5
                '()
                '()))
    (make-tree'1/
              '()
              (make-tree 6
                         '()
                         '())))))

"problem 1."
(define (apt-value t)
  (cond ((eq? (value t) '1/) (/ 1 (apt-value (right t))))
        ((symbol? (value t)) ((cond ((eq? (value t) '+) +)
                                    ((eq? (value t) '-) -)
                                    ((eq? (value t) '*) *))
                              (apt-value (left t))
                              (apt-value (right t))))
        (else (value t))))

"problem 2. (a)"
(define (extract-preorder t)
  (if (null? t)
      t
      (append (list (value t))
              (extract-preorder (left t))
              (extract-preorder (right t)))))

"problem 2. (b)"
(define (extract-postorder t)
  (if (null? t)
      t
      (append (extract-postorder (left t))
              (extract-postorder (right t))
              (list (value t)))))

"problem 2. (c)"
(define (extract-inorder t)
  (if (null? t)
      t
      (append (extract-inorder (left t))
              (list (value t))
              (extract-inorder (right t)))))

"problem 2. (d)"
(define (extract-infix-unique t)
  (if (null? t)
      t
      (let ((l (extract-infix-unique (left t)))
            (r (extract-infix-unique (right t))))
        (cond ((null? (right t)) (value t)) ; if right is null, then left is also null
              ((null? (left t)) (append (list (value t)) (list r)))
              (else (append (list l) (list (value t)) (list r)))))))

"problem 3. (a)"
(define (bst-element? item bs-tree same? less-than?)
  (display "looking for ") (display item) (display " in ") (display bs-tree) (newline)
  (cond ((null? bs-tree) #f)
        ((same? item (value bs-tree)) #t)
        ((less-than? item (value bs-tree)) (bst-element? item (left bs-tree) same? less-than?))
        (else (bst-element? item (right bs-tree) same? less-than?))))

"problem 3. (b)"
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

"problem 3. (c)"
(define (bst-smallest bs-tree)
  (cond ((null? bs-tree) "error")
        ((null? (left bs-tree)) (value bs-tree))
        (else (bst-smallest (left bs-tree)))))

"problem 3. (d)"
(define (bst-largest bs-tree)
  (cond ((null? bs-tree) "error")
        ((null? (right bs-tree)) (value bs-tree))
        (else (bst-largest (right bs-tree)))))

"problem 3. (e)"
(define (bst-equal? bst1 bst2 same?)
  (or (and (null? bst1)
           (null? bst2))
      (and (not (null? bst1))
           (not (null? bst2))
           (same? (value bst1) (value bst2))
           (bst-equal? (left bst1) (left bst2) same?)
           (bst-equal? (right bst1) (right bst2) same?))))

"problem 4. (a)"
(define (bst-subset? bst1 bst2 same? less-than?)
  (or (null? bst1)
      (and (bst-element? (value bst1) bst2 same? less-than?)
           (bst-subset? (left bst1) bst2 same? less-than?)
           (bst-subset? (left bst1) bst2 same? less-than?))))

"problem 4. (b)"
(define (bst-set-difference bst1 bst2 same? less-than?)
  (if (null? bst1)
      bst1
      (let ((l (bst-set-difference (left bst1) bst2 same? less-than?))
            (r (bst-set-difference (right bst1) bst2 same? less-than?)))
        (if (bst-element? (value bst1) bst2 same? less-than?)
            (if (null? l)
                r
                (make-tree (bst-smallest l) (bst-delete-min l) r))
            (make-tree (value bst1) l r)))))

"problem 4. (c)"
(define (bst-set-equal? bst1 bst2 same? less-than?)
  (and (bst-subset? bst1 bst2 same? less-than?)
       (bst-subset? bst2 bst1 same? less-than?)))

"problem 5. (a)"
(define (bst-delete-min bst)
  (cond ((null? bst) bst)
        ((null? (left bst)) (right bst))
        (else (make-tree (value bst)
                         (bst-delete-min (left bst))
                         (right bst)))))

"problem 5. (b)"
(define (bst-delete-max bst)
  (cond ((null? bst) bst)
        ((null? (right bst)) (left bst))
        (else (make-tree (value bst)
                         (left bst)
                         (bst-delete-max (right bst))))))

"problem 5. (c)"
(define (bst-delete val bst same? less-than?)
  (cond ((null? bst) bst)
        ((same? val (value bst)) (cond ((and (null? (left bst)) (null? (right bst))) '())
                                       ((null? (left bst)) (right bst))
                                       ((null? (right bst)) (left bst))
                                       (else (make-tree (bst-largest (left bst))
                                                        (bst-delete-max (left bst))
                                                        (right bst)))))
        ((less-than? val (value bst)) (make-tree (value bst)
                                                 (bst-delete val (left bst) same? less-than?)
                                                 (right bst)))
        (else (make-tree (value bst)
                         (left bst)
                         (bst-delete val (right bst) same? less-than?)))))