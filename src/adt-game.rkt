;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  Spel ADT                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-game-adt)
  (let ((level (new-level level-width level-height))
        (draw (new-draw-adt window-width window-height))
        (has-started? #f))
    
    (define (set-key-callback! state key)
      (cond ((and (eq? state 'pressed)(eq? key 'up))
             (set! level (new-level level-width level-height)))
            ((and (eq? state 'pressed)(eq? key 'down))
             (set! has-started? #t))))
   
    (define (update! delta-time)
      (if has-started?
          (begin
            ((draw 'draw-game!) dispatch-game)
            ((level 'update!) delta-time)
            ((draw 'update!) delta-time))))

    (define (start!)
      ((draw 'set-update-callback!) update!) 
      ((draw 'set-key-callback!) set-key-callback!)
      ((draw 'set-mouse-click-callback!) mouse-click-procedure))
   
    (define (mouse-click-procedure btn event x y)
      (if (and (eq? btn 'left)
               (eq? event 'pressed))
          (let* ((x-cel (quotient x tile-width-px))
                 (y-cel (quotient y tile-height-px)))
            ((draw 'create-tower-tile))
            ((draw 'create-gun-tile))
            ((level 'click-tower) x-cel y-cel))))
 
    (define (dispatch-game msg)
      (cond ((eq? msg 'start!) start!)
            ((eq? msg 'level) level)
            ((eq? msg 'has-started?) has-started?)))
    dispatch-game))
