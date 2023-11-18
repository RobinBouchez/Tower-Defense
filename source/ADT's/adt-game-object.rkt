;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            Game-object ADT                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-game-object position)
  (let* ((speed 0.0)
         (velocity (new-vector2 0 0))
         (cooldown 1000)
         (active? #t)
         (width 1)
         (height 1)
         (boundaries (new-rect (position 'x) (position 'y) width height))
         (type 'null))

    (define (type! new-tag)
      (set! type new-tag))
    
    (define (position! new-point)
      (set! position new-point))

    (define (speed! new-number)
      (set! speed new-number))

    (define (velocity! new-vector2)
      (set! velocity new-vector2))

    (define (active! new-bool)
      (set! active? new-bool))
    
    (define (boundaries! new-rect)
      (set! boundaries new-rect))
    
    (define (initialize! type position . additional)
      (position! position)
      (type! type)
      (when (not (null? additional))
        (speed! (car additional))
        (velocity! (cadr additional))
        (set! width (caddr additional))
        (set! height (cadddr additional))))

    (define (update! delta-time)
      (if active?
          (position!
           (new-point (+ (position 'x) (velocity 'x))
                      (+ (position 'y) (velocity 'y)))))
          (boundaries! (new-rect (position 'x) (position 'y) width height)))
    
    (define (dispatch-object msg)
      (cond ((eq? msg 'position) position)
            ((eq? msg 'position!) position!)
            ((eq? msg 'speed) speed)
            ((eq? msg 'speed!) speed!)
            ((eq? msg 'velocity) velocity)
            ((eq? msg 'velocity!) velocity!)
            ((eq? msg 'boundaries) boundaries)
            ((eq? msg 'boundaries!) boundaries!)
            ((eq? msg 'active?) active?)
            ((eq? msg 'active!) active!)
            ((eq? msg 'type) type)
            ((eq? msg 'type!) type!)
            ((eq? msg 'update!) update!)
            ((eq? msg 'initialize!) initialize!)
            (else (begin (display "GAME-OBJECT: Not a valid message: ")(display msg)))))
    dispatch-object))