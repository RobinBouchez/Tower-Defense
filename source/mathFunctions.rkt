(define pi (* 4 (atan 1.0)))

(define (mod a b)
  (* b (- (/ a b) (floor (/ a b)))))

(define tau (* 2 pi))

(define (atan2 y x)
  (if (= 0 x)
      (if (> y 0)
          (* .5 pi)
          (* 1.5 pi))
      (mod (if (< x 0)
               (+ pi (atan (/ y x)))
               (atan (/ y x)))
           tau)))

(define (distance p1 p2)
  (let ((dx (- (p1 'x) (p2 'x)))
        (dy (- (p1 'y) (p2 'y))))
    (sqrt (+ (* dx dx) (* dy dy)))))

(define (calculate-angle p1 p2)
  (* (atan2 (- (p1 'y) (p2 'y))(- (p1 'x)(p2 'x) ))(/ 180 pi)))

(define (generate-random-number lower upper)
  (+ lower (random (- upper lower 1))))
