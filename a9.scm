"problem 1. (a)"
(define (create-heap vw-pair left-child right-child)
  (list vw-pair left-child right-child))

"problem 1. (b)"
(define (h-min heap)
  (car heap))

"problem 1. (c)"
(define (left heap)
  (cadr heap))

"problem 1. (d)"
(define (right heap)
  (caddr heap))

"problem 1. (e)"
(define (insert vw-pair heap)
  (if (null? heap)
      (create-heap vw-pair '() '())
      (if (< (cdr vw-pair) (cdr (h-min heap)))
          ; swap left and right to maintain balance
          (create-heap vw-pair
                       (right heap)
                       (insert (h-min heap) (left heap)))
          (create-heap (h-min heap)
                       (right heap)
                       (insert vw-pair (left heap))))))

"problem 1. (f)"
(define (insert-list-of-pairs vw-pair-list heap)
  (if (null? vw-pair-list)
      heap
      (insert-list-of-pairs (cdr vw-pair-list)
                            (insert (car vw-pair-list) heap))))

"problem 1. (g)"
(define (combine-heaps H1 H2)
  (cond ((null? H1) H2)
        ((null? H2) H1)
        ((< (cdr (h-min H1)) (cdr (h-min H2)))
         (create-heap (h-min H1)
                      H2
                      (combine-heaps (left H1) (right H1))))
        (else
         (create-heap (h-min H2)
                      H1
                      (combine-heaps (left H2) (right H2))))))
(define (remove-min heap)
  (combine-heaps (left heap) (right heap)))

"problem 2."
(define (num-occurs sym lst)
  (define (loop count remaining)
    (cond ((null? remaining) count)
          ((eq? (car remaining) sym) (loop (+ count 1) (cdr remaining)))
          (else (loop count (cdr remaining)))))
  (loop 0 lst))

(define (num-occurs sym lst)
  (cond ((null? lst) 0)
        ((eq? (car lst) sym) (+ 1 (num-occurs sym (cdr lst))))
        (else (num-occurs sym (cdr lst)))))

"problem 3."
(define (freq-list lst)
  (define (in symbol pairs)
    (and (not (null? pairs))
         (or (eq? symbol (caar pairs))
             (in symbol (cdr pairs)))))
  (define (loop pairs remaining)
    (cond ((null? remaining) pairs)
          ((in (car remaining) pairs) (loop pairs (cdr remaining)))
          (else (loop (cons (cons (car remaining)
                                  (num-occurs (car remaining) lst)) pairs)
                      (cdr remaining)))))
  (loop (list) lst))

"problem 4."
(define (get-in-order heap)
  (if (null? heap)
      heap
      (cons (h-min heap) (get-in-order (remove-min heap)))))
(define (heapsort pair-list)
  (get-in-order (insert-list-of-pairs pair-list '())))

"problem 5."
(define (sorted-freq-list lst)
  (heapsort (freq-list lst)))