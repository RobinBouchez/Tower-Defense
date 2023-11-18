;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Power-up ADT                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-power-up type layer position)
  (let ((game-obj (new-game-object position))
        (exploded? #f)
        (deleted-bomb? #f)
        (deleted-impact? #f)
        (damage 2)
        (timer 0)
        (cooldown 3000)
        (off-screen-pos (new-point -2 0))
        (power-up-tile
         (make-bitmap-tile (string-append "../images/" (symbol->string type) ".png")
                           (string-append "../images/" (symbol->string type) "-mask.png")))
        (bomb-explosion-tile
         (make-bitmap-tile "../images/impact.png"
                           "../images/impact-mask.png")))
      

    (define (initialize!)
      ((game-obj 'initialize!) type position)
      ((game-obj 'boundaries!) (new-rect (position 'x)(position 'y)
                                         1 1))
      (create-tile! layer power-up-tile)
      (create-tile! layer bomb-explosion-tile))
    
 
    (define (update! delta-time)
      ((game-obj 'update!) delta-time)
      (when (not exploded?)
        ((set! timer (+ timer delta-time))
         (when (>= timer cooldown)
           (set! timer 0)
           (set! exploded? #t))
         (draw-tile! power-up-tile position))
        (else
         (delete! power-up-tile layer)
         (set! deleted-bomb? #t)))
      (when (and exploded? deleted-bomb?)
        ((set! timer (+ timer delta-time))
         (when (>= timer cooldown)
           (set! timer 0)
           (set! deleted-impact? #t))
         (draw-tile! bomb-explosion-tile position))
        (else
         (draw-tile! bomb-explosion-tile off-screen-pos)))
      (if deleted-impact?
          (delete! bomb-explosion-tile layer)))

    (initialize!)

    (define (dispatch-power-up msg)
      (cond ((eq? msg 'position) position)
            ((eq? msg 'exploded?) exploded?)
            ((eq? msg 'damage) damage)
            ((eq? msg 'cooldown) cooldown)
            ((eq? msg 'update!) update!)
            (else (game-obj msg))))
    dispatch-power-up))
