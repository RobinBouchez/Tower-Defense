;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                Enemy ADT                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-enemy layer path-idx type)
  (let* ((acceleration (new-vector2 0 0))
         (max-force 0.1)
         (end-reached? #f)
         (deleted? #f)
         (path (new-path path-idx))
         (enemy-tile
          (make-bitmap-tile (string-append "../images/enemy_" (symbol->string type) ".png")
                            "../images/enemy-mask.png"))
         (enemy (new-game-object (path 'start-point)))
         (this #f))

    (define (initialize!)
      (let* ((speed 0.02)
             (velocity (new-vector2 speed 0))
             (position (path 'start-point))
             (width (/ 18 tile-width-px))
             (height (/ 18 tile-height-px)))
        ((enemy 'initialize!) type position
                              speed velocity
                              width height))
      (cond ((eq? type 'red)
             (set! this (new-red-enemy dispatch)))
            ((eq? type 'blue)
             (set! this (new-blue-enemy dispatch)))
            ((eq? type 'grey)
             (set! this (new-grey-enemy dispatch))))
      (create-tile! layer enemy-tile))       
  
    (define (update! delta-time)
      (if (> (this 'health) 0)
          (begin
            ((enemy 'update!) delta-time)
            (if (not (eq? type 'red))
                ((this 'update!) delta-time))
            (follow-path!)
            (draw-tile! enemy-tile (enemy 'position)))      
          (when (not deleted?)
            (delete! enemy-tile layer)
            (set! deleted? #t))))

    (define (push-back!)
      (define push-factor -0.5)
      ((enemy 'velocity!) (new-vector2 (+ (* ((enemy 'velocity) 'x) -1) push-factor)
                                       (+ (* ((enemy 'velocity) 'y) -1) push-factor))))

    ;;
    ;; Path Following 
    ;;

    (define (follow-path!)
      (let ((f (follow)))
        (if (not end-reached?)
            (apply-force! f))
        (update-velocity!)))
        
    (define (update-velocity!)             
      (limit-velocity! (this 'speed))       
      ((this 'velocity!)
       (new-vector2 (+ ((this 'velocity) 'x) (acceleration 'x))
                    (+ ((this 'velocity) 'y) (acceleration 'y))))
      (acceleration! (new-vector2 0 0)))

    (define (take-hit! damage)
      ((this 'health!) (- (this 'health) damage)))
        
    (define (apply-force! force)
      (acceleration! (new-vector2 (+ (acceleration 'x)(force 'x))
                                  (+ (acceleration 'y)(force 'y)))))

    (define (find-projection pos a b) 
      (let ((v1 (new-vector2 (- (a 'x)(pos 'x))
                             (- (a 'y)(pos 'y))))
            (v2 (new-vector2 (- (b 'x)(pos 'x))
                             (- (b 'y)(pos 'y)))))
        (v2 'normalize!)
        (let ((sp ((v1 'dot) v2)))
          ((v2 'multiply!) sp)
          ((v2 'add!) pos)
          v2)))

    (define (follow)
      (if (not (eq? (path 'next-point) #f))
          (let* ((velocity (enemy 'velocity))
                 (position (enemy 'position))
                 (future (new-vector2 (velocity 'x)(velocity 'y)))
                 (multiply-force 10))
            ((future 'multiply!) multiply-force)
            ((future 'add!) position)
            (let* ((target (find-projection (path 'current-point) future (path 'next-point)))
                   (d ((future 'distance) target)))
              (if (> d (path 'radius))
                  (seek (path 'next-point))
                  (begin (path 'next-point!)
                         (new-vector2 0 0)))))
          (set! end-reached? #t)))
    
    (define (seek target)
      (let* ((position (enemy 'position))
             (force (new-vector2 (- (target 'x)(position 'x))
                                 (- (target 'y)(position 'y))))
             (desiredspeed (enemy 'speed)))
        ((force 'magnitude!) desiredspeed)
        ((force 'subtract!) (enemy 'velocity))
        ((force 'limit!) max-force)
        force))

    (define (acceleration! new-acceleration)
      (set! acceleration new-acceleration))

    (define (limit-velocity! max)
      (let* ((velocity (enemy 'velocity))
             (magnitude (velocity 'magnitude)))
        (if (> magnitude max)
            (let ((scale-factor (/ max magnitude)))
              ((enemy 'velocity!) (new-vector2 (* (velocity 'x) scale-factor)
                                               (* (velocity 'y) scale-factor)))))))
   
    (define (dispatch msg)
      (cond ((eq? msg 'deleted?) deleted?)
            ((eq? msg 'end-reached?) end-reached?)
            ((eq? msg 'take-hit!) take-hit!)
            ((eq? msg 'push-back!) push-back!)
            ((eq? msg 'update!) update!)
            (else (enemy msg))))
    (initialize!)
    dispatch))



