"problem 1. (a)"
(define (usd-to-euro usd) (* usd 0.88))
(usd-to-euro 250)

"problem 1. (b)"
(define (euro-to-yen euro) (* euro 124.91))
(euro-to-yen 250)

"problem 1. (c)"
(define (usd-to-yen usd) (euro-to-yen (usd-to-euro usd)))
(usd-to-yen 250)

"problem 2. (a)"
(define pi 3.14159)

"problem 2. (b)"
(define (area-of-circle r) (* pi
                              (* r r)))

"problem 3. (c)"
(define (surface-area-of-sphere r) (* 4
                                      (area-of-circle r)))

"problem 3. (d)"
(define (volume-of-sphere r) (/ (* r
                                   (surface-area-of-sphere r))
                                3))

"problem 4. (a)"
(define (det2x2 a b c d) (- (* a d)
                            (* b c)))
(det2x2 -3 1 2 7)