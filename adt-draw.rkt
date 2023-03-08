;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Draw ADT                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-draw-adt pixels-horizontaal pixels-verticaal)
  (let ((window (make-window pixels-horizontaal pixels-verticaal "Tower-Defense")))

    ((window 'set-background!) "white")

    ;;
    ;; Layers
    ;;

    (define background-layer ((window 'new-layer!)))
    (define tower-layer ((window 'new-layer!)))
    (define gun-layer ((window 'new-layer!)))
    (define bullet-layer ((window 'new-layer!)))
    (define enemy-layer ((window 'new-layer!)))
    (define foreground-layer ((window 'new-layer!)))
            
    ;;
    ;; Tiles
    ;;
    
    (define background-tile
      (make-bitmap-tile "images/map_a.png"))
    ((background-layer 'add-drawable!) background-tile)

    (define score-tile
      (make-bitmap-tile (string-append "images/num_0.png")
                        (string-append "images/num_0_mask.png")))
    ((foreground-layer 'add-drawable!) score-tile)
    
    (define score-tile-2
      (make-bitmap-tile (string-append "images/num_0.png")
                        (string-append "images/num_0_mask.png")))
    ((foreground-layer 'add-drawable!) score-tile-2)
   
    (define bullet-tile
      (make-bitmap-tile "images/bullet.png" "images/bullet-mask.png"))
    ;((bullet-layer 'add-drawable!) bullet-tile)
            
    (define enemy-tile
      (make-bitmap-tile "images/enemy.png" "images/enemy-mask.png"))
    ((enemy-layer 'add-drawable!) enemy-tile)
      
    (define option-tile
      (make-bitmap-tile "images/option_open.png" "images/option_open_mask.png"))
    ((foreground-layer 'add-drawable!) option-tile)
    
    (define option-tile-1
      (make-bitmap-tile "images/option_open.png" "images/option_open_mask.png"))
    ((foreground-layer 'add-drawable!) option-tile-1)
    
    (define option-tile-2
      (make-bitmap-tile "images/option_open.png" "images/option_open_mask.png"))
    ((foreground-layer 'add-drawable!) option-tile-2)
        
    (define tower-tile
      (make-bitmap-tile "images/tower.png" "images/tower-mask.png"))

    
    (define gun-tile
      (make-bitmap-tile "images/gun.png" "images/gun-mask.png"))
    
            
    ;;
    ;; Draw Functions
    ;;

    ;Background
    (define (draw-background!)
      (let* ((screen-x (* cel-breedte-px 0))
             (screen-y (* cel-hoogte-px 0)))
        ((background-tile 'set-x!) screen-x)
        ((background-tile 'set-y!) screen-y)))

    (define (draw-tile! tile offset)
      (let* ((screen-x (+ offset (- (/ pixels-horizontaal 2) 32 )))
             (screen-y (- pixels-verticaal 84)))
        ((tile 'set-x!) screen-x)
        ((tile 'set-y!) screen-y)))  

    (define (draw-object! obj tile)
      (let* ((obj-x ((obj 'position) 'x))
             (obj-y ((obj 'position) 'y))
             (screen-x (* tile-width-px obj-x))
             (screen-y (* tile-height-px obj-y)))
        ((tile 'set-x!) screen-x)
        ((tile 'set-y!) screen-y)))

   
    ;; game
    (define (draw-game! game)
      (if (game 'has-started?)
          (draw-level! (game 'level))))

    ;; level
    (define (draw-level! level)
      (draw-score! score-tile score-position-x score-position-y)
      (draw-score! score-tile-2 (+ score-position-x 0.45) score-position-y)
      (map (lambda (x)(draw-tower! x)) ((level 'towers) 'list))
      (draw-enmies! (level 'enemies))
      (draw-tile! option-tile 0)
      (draw-tile! option-tile-1 (+ 64 20))
      (draw-tile! option-tile-2 (* -1 (+ 64 20))))

    ;Bullet
    (define (draw-bullet! gun)
      (when gun
        (draw-object! gun bullet-tile)))

    ;Tower
    (define (draw-tower! tower)                            
      (if tower
          (begin
            (draw-object! tower tower-tile)
            (draw-object! tower gun-tile))))

    ;Score
    (define (draw-score! tile x y)
      (let* ((screen-x (* tile-width-px x))
             (screen-y (* tile-height-px y)))
        ((tile 'set-x!) screen-x)
        ((tile 'set-y!) screen-y)))

    ;; Enemies
    (define (draw-enmies! enemies)
      (let loop ((enemy-list (enemies 'list)))
        (if (not (null? enemy-list))
            (draw-object! (car enemy-list) enemy-tile)
            (loop (cdr enemy-list)))))
    
    (define (create-tower-tile)
      ((tower-layer 'add-drawable!) tower-tile))

    (define gun-tile-list '())
    
    (define (create-gun-tile)
      ((gun-layer 'add-drawable!) gun-tile)
      (set! gun-tile-list (cons gun-tile gun-tile-list)))
        

    ;;
    ;; Update Functions
    ;;
    
    (define (rotate-gun! tile speed)
      (if (not (null? gun-tile-list))
          (begin
            (let* ((current-rot (tile 'get-rotation))
                   (new-rotation (+ current-rot (/ speed 100))))
              ((tile 'rotate!) new-rotation)))))


    (define (update! delta-time)
      (if (not (null? gun-tile-list))
          (rotate-gun! (car gun-tile-list) 100)))
   
    ;;
    ;; Callbacks instellen
    ;;

    (define (set-update-callback! fun)
      ((window 'set-update-callback!) fun))
    
    (define (set-key-callback! fun)
      ((window 'set-key-callback!) fun))

    (define (set-mouse-click-callback! fun)
      ((window 'set-mouse-click-callback!) fun))

    ;;
    ;; Dispatch
    ;;
    
    (define (dispatch-draw-adt msg)
      (cond ((eq? msg 'set-key-callback!) set-key-callback!)
            ((eq? msg 'set-update-callback!) set-update-callback!)
            ((eq? msg 'update!) update!)
            ((eq? msg 'create-tower-tile) create-tower-tile)
            ((eq? msg 'create-gun-tile) create-gun-tile)
            ((eq? msg 'set-mouse-click-callback!) set-mouse-click-callback!)
            ((eq? msg 'draw-game!) draw-game!)))
    dispatch-draw-adt))
