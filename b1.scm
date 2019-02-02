"problem 1. (a)"
(define (f->c t) (* (/ 5 9)(- t 32)))

"problem 1. (b)"
(define (c->f t) (+ (* (/ 9 5) t) 32))

"problem 1. (c)"
(define (miles->km d) (* d 1.609344))

"problem 1. (d)"
(define (km->miles d) (/ d 1.609344))

"problem 2. (a)"
(define (is-triangle? a b c) (and (> (+ a b) c)
                                  (> (+ b c) a)
                                  (> (+ c a) b)))

"problem 2. (b)"
(define (area a b c) (sqrt (* (/ (+ a b c) 2)
                              (- (/ (+ a b c) 2) a)
                              (- (/ (+ a b c) 2) b)
                              (- (/ (+ a b c) 2) c))))

"problem 2. (c)"
(define (op-angle a b c) (acos (/ (- (+ (* b b) (* c c)) (* a a)) (* 2 b c))))

"problem 3. (a)"
(define (discriminant a b c) (- (* b b)
                                (* 4 a c)))
(define (root1 a b c) (/ (+ (* -1 b)
                            (sqrt (discriminant a b c)))
                         (* 2 a)))

"problem 3. (b)"
(define (root2 a b c) (/ (- (* -1 b)
                            (sqrt (discriminant a b c)))
                         (* 2 a)))

"problem 3. (c)"
(define (number-of-roots a b c) (if (= (discriminant a b c) 0) 1 2))

"problem 3. (d)"
(define (real-roots? a b c) (>= (discriminant a b c) 0))

"problem 4. (a)"
(define (polar->cart-x r theta) (* r
                                   (cos theta)))

"problem 4. (b)"
(define (polar->cart-y r theta) (* r
                                   (sin theta)))

"problem 4. (c)"
(define (cart->polar-r x y) (sqrt (+ (* x x)
                                     (* y y))))

"problem 4. (d)"
(define (cart->polar-a x y) (cond ((and (>= y 0) (not (= (cart->polar-r x y) 0))) 
                                    (acos (/ x (cart->polar-r x y))))
                                  ((< y 0)
                                    (* -1 (acos (/ x (cart->polar-r x y)))))
                                  (#t 0)))

"problem 5. (a)"
(define (wc t v) (if (and (<= t 50) (> v 3)) (+ 35.74
                                                (* 0.6215 t)
                                                (* -35.75 (expt v 0.16))
                                                (* 0.4275 t (expt v 0.16)))
                                             t))

"problem 5. (b)"
(define (wc-metric t v) (f->c (wc (c->f t) (km->miles v))))