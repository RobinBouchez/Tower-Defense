;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                Tower ADT                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-tower type position tower-layer gun-layer) 
  (let* ((game-obj (new-game-object position))
         (tower-angle 180)
         (timer 0)
         (has-shot? #t)
         (bullet-speed -0.05)
         (tower-tile (make-bitmap-tile
                      (string-append "../images/" (symbol->string type) ".png")
                      (string-append "../images/" (symbol->string type) "-mask.png")))
         (gun-tile (make-bitmap-tile
                    (string-append "../images/" (symbol->string type) "-gun.png")
                    (string-append "../images/" (symbol->string type) "-gun-mask.png")))
         (bullet (new-bullet type gun-layer position tower-angle (new-vector2 0 bullet-speed)))
         (this #f))   

    (define (initialize!)      
      ((game-obj 'initialize!) type position)
      (cond ((eq? type 'standard-tower)
             (set! this (new-standard-tower protected)))
            ((eq? type 'canon-tower)
             (set! this (new-canon-tower protected)))
            ((eq? type 'bomber-tower)
             (set! this (new-bomber-tower protected)))
            ((eq? type 'manual-tower)
             (set! this (new-manual-tower protected))))
      (create-tile! tower-layer tower-tile)
      (create-tile! gun-layer gun-tile)
      (draw!))

    (define (draw!)
      (draw-tile! tower-tile position)
      (draw-tile! gun-tile position))
    
    (define (update! delta-time)
      (if (not (is-in-range? bullet position))
          ((bullet 'active!) #f))
      ((game-obj 'update!) delta-time)
    
      (timed-if (has-shot? after (this 'cooldown))
                (do (if (not (eq? type 'manual-tower))
                        ((this 'rotated!) #t)
                        ((this 'can-shoot!) #t)))
                (else (if (not (eq? type 'manual-tower))
                          (shoot!)))
                for timer delta-time)
      ((bullet 'update!) delta-time))

    (define (shoot!)
      (set! bullet (new-bullet type gun-layer position tower-angle (this 'bullet-velocity))))
  
    (define (angle! new-angle)
      (set! tower-angle new-angle))
    
    (define (has-shot! new-bool)
      (set! has-shot? new-bool))

    (define (is-in-range? bullet pos)
          (let* ((bullet-pos (bullet 'position))
                 (distance (distance bullet-pos pos)))
            (<= distance (this 'range))))
    
    (define (rotate! . pos)
      (if (eq? type 'manual-tower)
          ((this 'rotate!) pos)
          ((this 'rotate!))))

    (define (protected msg)
      (cond ((eq? msg 'angle) tower-angle)
            ((eq? msg 'angle!) angle!)
            ((eq? msg 'gun-tile) gun-tile)
            ((eq? msg 'shoot!) shoot!)
            ((eq? msg 'has-shot!) has-shot!)
            (else (dispatch msg))))
        
    (define (dispatch msg)
      (cond ((eq? msg 'update!) update!)
            ((eq? msg 'bullet) bullet)
            ((eq? msg 'rotate!) rotate!)
            ((eq? msg 'damage) (this 'damage))
            (else (game-obj msg))))
    (initialize!)
    dispatch))