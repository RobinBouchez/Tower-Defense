;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Bullet ADT                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-bullet position damage)
  (define speed (new-point 0 0))
  
  (define (position! new-position)
    (set! position new-position))

  (define (damage! new-damage)
    (set! damage new-damage))
  
  (define (speed! new-speed)
    (set! speed new-speed))

  (define (update! delta-time)
    (position!
     (new-point (+ (position 'x) (speed 'x))
                (+ (position 'y) (speed 'y)))))
    
  (define (dispatch-bullet msg)
    (cond ((eq? msg 'position) position)
          ((eq? msg 'position!) position!)
          ((eq? msg 'speed) speed)
          ((eq? msg 'speed!) speed!)
          ((eq? msg 'update!) update!)
          ((eq? msg 'damage) damage)
          ((eq? msg 'damage!) 'damage!)))
  dispatch-bullet)
