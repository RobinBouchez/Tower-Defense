;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  Gun ADT                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-gun position range damage)
  (let ((bullets '()))
    
  (define (position! new-position)
    (set! position new-position))

  (define (fire-bullet! delta-time)
    (let ((new-x (* (velocity 'x) delta-time))
          (new-y (* (velocity 'y) delta-time))
          (bullet (new-bullet position damage)))
      (((bullet 'position) 'x!) (+ ((bullet 'position) 'x) 1))
      (((bullet 'position) 'y!) (+ ((bullet 'position) 'y) 1))))

  (define (point:< p1 p2)
    (and (< (p1 'x)(p2 'x)) (< (p1 'y)(p2 'y))))
  
    (define (aim! enemy-position delta-time)
      (let ((distance (new-point (abs (- (position 'x) ((enemy-position 'position) 'x)))
                                 (abs (- (position 'y) ((enemy-position 'position) 'y)))))
            (lock-on #t))
        (if (point:< distance (range 'center))
            (if (eq? lock-on #t)
                (fire-bullet! delta-time)))))

  (define (update! delta-time)
    (map (lambda (bullet) ((bullet 'update!) delta-time)) bullets))

    (define (dispatch-gun msg)
      (cond ((eq? msg 'position) position)
            ((eq? msg 'damage) damage)
            ((eq? msg 'position!) position!)
            ((eq? msg 'update!) update!)
            ((eq? msg 'aim!) aim!)))
    dispatch-gun))

  