;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Vector2 ADT                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-vector2 x y)
 
  (define (x! new-x)
    (set! x new-x))
  
  (define (y! new-y)
    (set! y new-y))

  (define (dot v)
    (+ (* x (v 'x))(* y (v 'y))))
  
  (define (add! v)
    (x! (+ x (v 'x)))
    (y! (+ y (v 'y))))
    
  (define (subtract! v)
    (x! (- x (v 'x)))
    (y! (- y (v 'y))))

  (define (multiply! v)
    (x! (* x v))
    (y! (* y v)))

  (define (distance vect)
    (sqrt (+ (* (- (vect 'x) x)(- (vect 'x) x))
             (* (- (vect 'y) y)(- (vect 'y) y)))))

  (define (magnitude! m)
    (if (not (eq? (magnitude) 0))
        (let ((co (/ m (magnitude))))
          (x! (* x co))
          (y! (* y co)))
        (let ((co (/ m 0.001)))
          (x! (* x co))
          (y! (* y co)))))

  (define (magnitude)
    (sqrt (+ (* x x)(* y y))))
  
  (define (normalize!)
    (define m (magnitude))
    (if (> m 0)
        (begin
          (x! (/ x m))
          (y! (/ y m)))))
    
  (define (limit! m)
    (let ((magnitude (magnitude)))
      (if (> magnitude m)
          (let ((scale-factor (/ m magnitude)))
            (x! (* x scale-factor))
            (y! (* y scale-factor))))))
  
  (define (dispatch-vector msg)
    (cond ((eq? msg 'x) x)
          ((eq? msg 'y) y)
          ((eq? msg 'add!) add!)
          ((eq? msg 'subtract!) subtract!)
          ((eq? msg 'multiply!) multiply!)
          ((eq? msg 'dot) dot)
          ((eq? msg 'distance) distance)
          ((eq? msg 'magnitude) (magnitude))
          ((eq? msg 'magnitude!) magnitude!)
          ((eq? msg 'limit!) limit!)
          ((eq? msg 'normalize!) (normalize!))
          ((eq? msg 'x!) x!)
          ((eq? msg 'y!) y!)))
  dispatch-vector)
