(define (string->word-list str)
 (define (split-off-first char-list)
     (define (split-aux char-list res)
       (if (or (null? char-list)
               (eq? (car char-list) #\space)
               (eq? (car char-list) #\newline))
           (cons (list->string (reverse res)) char-list)
           (split-aux (cdr char-list) (cons (car char-list) res))))
    (cond ((eq? (car char-list) #\space)
           (cons " " (cdr char-list)))
          ((eq? (car char-list) #\newline)
           (cons "\n" (cdr char-list)))
          (else
           (split-aux char-list '()))))
  (define (build-word-list char-list word-list)
    (if (null? char-list)
        (reverse word-list)
        (let ((split (split-off-first char-list)))
          (build-word-list (cdr split)(cons (car split) word-list)))))
  (build-word-list (string->list str) '()))

(define (surfin-bird) 
  "a well a everybodys heard about the bird
bird bird bird b-birds the word
a well a bird bird bird the bird is the word
a well a bird bird bird well the bird is the word
a well a bird bird bird b-birds the word
a well a bird bird bird well the bird is the word
a well a bird bird b-birds the word
a well a bird bird bird b-birds the word
a well a bird bird bird well the bird is the word
a well a bird bird b-birds the word
a well a dont you know about the bird
well everybody knows that the bird is the word
a well a bird bird b-birds the word
a well a

a well a everybodys heard about the bird
bird bird bird b-birds the word
a well a bird bird bird b-birds the word
a well a bird bird bird b-birds the word
a well a bird bird b-birds the word
a well a bird bird bird b-birds the word
a well a bird bird bird b-birds the word
a well a bird bird bird b-birds the word
a well a bird bird bird b-birds the word
a well a dont you know about the bird
well everybodys talking about the bird
a well a bird bird b-birds the word
a well a bird

surfin bird
bbbbbbbbbbbbbbbbbb aaah

pa pa pa pa pa pa pa pa pa pa pa pa pa pa pa pa
pa pa pa pa pa pa pa pa pa pa pa pa pa pa ooma mow mow
papa ooma mow mow

papa ooma mow mow papa ooma mow mow
papa ooma mow mow papa ooma mow mow
ooma mow mow papa ooma mow mow
papa ooma mow mow papa ooma mow mow
papa ooma mow mow papa ooma mow mow
oom oom oom oom ooma mow mow
papa ooma mow mow papa oom oom oom
oom ooma mow mow papa ooma mow mow
ooma mow mow papa ooma mow mow
papa a mow mow papa ooma mow mow
papa ooma mow mow ooma mow mow
papa ooma mow mow ooma mow mow
papa oom oom oom oom ooma mow mow
oom oom oom oom ooma mow mow
ooma mow mow papa ooma mow mow
papa ooma mow mow ooma mow mow
well dont you know about the bird
well everybody knows that the bird is the word
a well a bird bird b-birds the word

papa ooma mow mow papa ooma mow mow
papa ooma mow mow papa ooma mow mow
papa ooma mow mow papa ooma mow mow")

; see what (string->word-list (surfin-bird)) produces


;; extra functions using bit sequences
;; the strings here are strings of 0s and 1s
;; a bit-sequence is simply an integer
;; a 1 is added to the start of each bit-sequence
;; as (number->string x 2) drops leading zeroes

(define (bit-sequence->string num)
  (define (rem-first string)
    (substring string 1 (string-length string)))
  (rem-first (number->string num 2)))

(define (string->bit-sequence  str)
  (string->number (string-append "1" str) 2))

;; try (bit-sequence->string 1729) and then invert it with string->bit-sequence

; the actual number of bits used is 1 plus the length of the string,
; each number n encodes a 0-1-string of (floor (log2 n)) characters
(define (log2 x)
  (/ (log x)(log 2)))

; Functions from lab8.rkt

(define (num-occurs sym lst)
  (define (loop count remaining)
    (cond ((null? remaining) count)
          ((equal? (car remaining) sym) (loop (+ count 1) (cdr remaining)))
          (else (loop count (cdr remaining)))))
  (loop 0 lst))
(define (freq-list lst)
  (define (in symbol pairs)
    (and (not (null? pairs))
         (or (equal? symbol (caar pairs))
             (in symbol (cdr pairs)))))
  (define (loop pairs remaining)
    (cond ((null? remaining) pairs)
          ((in (car remaining) pairs) (loop pairs (cdr remaining)))
          (else (loop (cons (cons (car remaining)
                                  (num-occurs (car remaining) lst)) pairs)
                      (cdr remaining)))))
  (loop (list) lst))

(define (make-tree value left right)
  (list value left right))
(define (value tree)
  (car tree))
(define (left tree)
  (cadr tree))
(define (right tree)
  (caddr tree))

(define (create-heap vw-pair left-child right-child)
  (list vw-pair left-child right-child))
(define (h-min heap)
  (car heap))
(define (left heap)
  (cadr heap))
(define (right heap)
  (caddr heap))
(define (insert vw-pair heap)
  (if (null? heap)
      (create-heap vw-pair '() '())
      (if (< (cdr vw-pair) (cdr (h-min heap)))
          (create-heap vw-pair
                       (right heap)
                       (insert (h-min heap) (left heap)))
          (create-heap (h-min heap)
                       (right heap)
                       (insert vw-pair (left heap))))))
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

"problem 1."
(define (word-list->string wlst)
  (if (null? wlst)
      ""
      (string-append (car wlst) (word-list->string (cdr wlst)))))

"problem 2."
(define (word-frequencies text)
  (freq-list (string->word-list text)))

"problem 3. (a)"
(define (combine-htree-pairs hp1 hp2)
  (cons (make-tree 'internal (car hp1) (car hp2))
        (+ (cdr hp1) (cdr hp2))))

"problem 3. (b)"
(define (build-huffman sf-list)
  (define (heap-loop remaining-pairs heap)
    (if (null? remaining-pairs)
        heap
        (let* ((pair (car remaining-pairs))
               (heap (insert (cons (make-tree (car pair) '() '())
                                   (cdr pair))
                             heap)))
          (heap-loop (cdr remaining-pairs) heap))))
    (define (tree-loop heap)
      (if (and (null? (left heap))
               (null? (right heap)))
          (car (value heap))
          (let* ((min1 (h-min heap))
                 (heap (remove-min heap))
                 (min2 (h-min heap))
                 (heap (remove-min heap)))       
            (tree-loop (insert (combine-htree-pairs min1 min2) heap)))))
  (tree-loop (heap-loop sf-list '())))

"problem 4."
(define (get-encoding-list tree)
  (define (recurse subtree prefix)
    (if (and (null? (left subtree))
             (null? (right subtree)))
        (list (cons (value subtree) prefix))
        (append (recurse (left subtree) (string-append prefix "0"))
                (recurse (right subtree) (string-append prefix "1")))))
  (recurse tree ""))

"problem 5."
(define (encode text encoding)
  (define (word-to-code word remaining-pairs)
    (if (equal? word (car (car remaining-pairs)))
        (cdr (car remaining-pairs))
        (word-to-code word (cdr remaining-pairs))))
  (define (code-recurse words)
    (cond ((null? words) "")
          ;((equal? (car words) " ") (code-recurse (cdr words)))
          (else (string-append (word-to-code (car words) encoding)
                               (code-recurse (cdr words))))))
  (code-recurse (string->word-list text)))

"problem 6."
(define (decode text tree)
  (define (code-to-text code subtree)
    (cond ((not (equal? (value subtree) 'internal))
           (string-append (value subtree)
                          (code-to-text code tree)))
          ((null? code) "")
          ((equal? (car code) #\0)
           (code-to-text (cdr code) (left subtree)))
          ((equal? (car code) #\1)
           (code-to-text (cdr code) (right subtree)))))
  (code-to-text (string->list text) tree))

"problem 7. (a)"
(define frequencies (word-frequencies (surfin-bird)))

"problem 7. (b)"
(define tree (build-huffman frequencies))

"problem 7. (c)"
(define encoding (get-encoding-list tree))

"problem 7. (d)"
(define encoded (encode (surfin-bird) encoding))

"problem 7. (e)"
(define decoded (decode encoded tree))
(equal? decoded (surfin-bird))

"problem 8. 1."
(define (bit-encode text encoding)
  (string->bit-sequence (encode text encoding)))

"problem 8. 2."
(define (bit-decode code-number tree)
  (decode (bit-sequence->string code-number) tree))