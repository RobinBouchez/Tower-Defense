;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  Draw                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-draw)
  (let ((window (make-window window-width window-height "Tower-Defense"))
        (draw-game? #f))

    ((window 'set-background!) "white")

  
    ;;
    ;; Layers
    ;;
    (define background-layer ((window 'new-layer!)))
    (define tower-layer ((window 'new-layer!)))
    (define gun-layer ((window 'new-layer!)))
    (define power-up-layer ((window 'new-layer!)))
    (define bullet-layer ((window 'new-layer!)))
    (define enemy-layer ((window 'new-layer!)))
    (define foreground-layer ((window 'new-layer!)))
    (define start-layer ((window 'new-layer!)))

    ;;
    ;; Game tiles
    ;;

    (define health-tile-1 (make-bitmap-tile "../images/health_point.png" "../images/health_point_mask.png"))
    (define health-tile-2 (make-bitmap-tile "../images/health_point.png" "../images/health_point_mask.png"))
    (define health-tile-3 (make-bitmap-tile "../images/health_point.png" "../images/health_point_mask.png"))
    (define menu-tile (make-bitmap-tile "../images/menu.png" "../images/menu-mask.png"))
    (define level-tile (make-bitmap-tile  "../images/map-1.png"))
    (define start-tile (make-bitmap-tile "../images/start-screen.png"))
    (define money-tile (make-tile tile-width-px tile-height-px))

    (define (empty!)
      ((power-up-layer 'empty!))
      ((tower-layer 'empty!))
      ((gun-layer 'empty!))
      ((foreground-layer 'empty!))
      ((bullet-layer 'empty!))
      ((enemy-layer 'empty!)))

    (define (draw-health! health)
      (if (health 'active?)
          (if (>= (health 'lives) 1)
              (begin
                (draw-tile! health-tile-1 (new-point ((health 'position) 'x) ((health 'position) 'y)))
                (if (>= (health 'lives) 2)
                    (begin
                      (draw-tile! health-tile-2 (new-point (- ((health 'position) 'x) health-offset-x) ((health 'position) 'y)))
                      (if (>= (health 'lives) 3)
                          (draw-tile! health-tile-3 (new-point (- ((health 'position) 'x) (* health-offset-x 2)) ((health 'position) 'y)))
                          (delete! health-tile-3 foreground-layer)))
                    (delete! health-tile-2 foreground-layer)))
              (delete! health-tile-1 foreground-layer))))

    (define (draw-menu! menu)
      (draw-tile! menu-tile (menu 'position)))

        
    (define (draw-money! pos money-amount)
      (money-tile 'clear!)
      ((money-tile 'draw-text!) (number->string money-amount) money-text-fontsize
                                money-text-x money-text-y money-text-color)
      (draw-tile! money-tile pos))
  

    (define (start!)
      (create-tile! background-layer level-tile)
      (create-tile! foreground-layer money-tile)
      (create-tile! foreground-layer menu-tile)
      (create-tile! foreground-layer health-tile-1)
      (create-tile! foreground-layer health-tile-2)
      (create-tile! foreground-layer health-tile-3)
      (create-tile! start-layer start-tile))

    (define start-tile-pos (new-point 0 0))
    (define start-tile-pos-clear (new-point 0  (* start-tile-height -1)))

    (define (draw-level! level-index)
      (define tile
        (make-bitmap-tile (string-append "../images/map-" (number->string level-index) ".png")))
      (create-tile! background-layer tile)
      (draw-tile! level-tile (new-point level-pos-x level-pos-y)))
    
    (define (draw-game! game)
      (when (game 'start-game?)
        (if (not (= (start-tile-pos 'y)(start-tile-pos-clear 'y)))
            (set! start-tile-pos (new-point (start-tile-pos 'x) (- (start-tile-pos 'y) 1)))
            (begin
              (set! start-tile-pos start-tile-pos-clear)                 
              (when (not draw-game?)
                (delete! start-tile start-layer)
                (create-tile! background-layer level-tile)
                (set! draw-game? #t)))))
      (let ((level (game 'level)))
        (draw-menu! (level 'menu))
        (draw-money! (new-point (+ (((level 'menu) 'position) 'x) money-pos-offset-x)
                                (+ (((level 'menu) 'position) 'y) money-pos-offset-y)) (((level 'menu) 'money) 'amount))
        (draw-health! (game 'health))         
        (draw-tile! start-tile start-tile-pos)))

    (define (set-update-callback! fun)
      ((window 'set-update-callback!) fun))
    
    (define (set-key-callback! fun)
      ((window 'set-key-callback!) fun))

    (define (set-mouse-click-callback! fun)
      ((window 'set-mouse-click-callback!) fun))


    (define (dispatch-draw msg)
      (cond ((eq? msg 'set-key-callback!) set-key-callback!)
            ((eq? msg 'set-update-callback!) set-update-callback!)
            ((eq? msg 'set-mouse-click-callback!) set-mouse-click-callback!)
            ((eq? msg 'empty!) (empty!))
            ((eq? msg 'tower-layer) tower-layer)
            ((eq? msg 'gun-layer) gun-layer)
            ((eq? msg 'bullet-layer) bullet-layer)
            ((eq? msg 'enemy-layer) enemy-layer)
            ((eq? msg 'power-up-layer) power-up-layer)
            ((eq? msg 'foreground-layer) foreground-layer)
            ((eq? msg 'draw-game!) draw-game!)
            ((eq? msg 'draw-level!) draw-level!)
            ((eq? msg 'start!) (start!))
            (else (display "ERROR Draw-adt: ")(display msg))))
    dispatch-draw))