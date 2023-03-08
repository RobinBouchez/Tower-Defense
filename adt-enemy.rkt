;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                Enemy ADT                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; maak-toren :: number, number -> punt
(define (new-enemy position health)
  (let ((speed (new-point 0 0)))
  
  (define (position! new-position)
    (set! position new-position))

  (define (health! new-health)
    (set! health new-health))

    (define (speed! new-speed)
    (set! speed new-speed))

  (define (update! delta-time)
    (position!
     (new-point (+ (position 'x) (speed 'x))
                (+ (position 'y) (speed 'y)))))
  
  (define (dispatch-enemy msg)
    (cond ((eq? msg 'position) position)
          ((eq? msg 'position!) position!)
          ((eq? msg 'health) health)
          ((eq? msg 'health!) health!)
          ((eq? msg 'speed) speed)
          ((eq? msg 'speed!) speed!)
          ((eq? msg 'update!) update!)))
  dispatch-enemy))
