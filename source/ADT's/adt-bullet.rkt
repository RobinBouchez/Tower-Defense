;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              Bullet ADT                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-bullet type bullet-layer position angle velocity)
  (let ((game-obj (new-game-object position))
        (bullet-tile
         (make-bitmap-tile (string-append "../images/" (symbol->string type) "-bullet.png")
                           (string-append "../images/" (symbol->string type) "-bullet-mask.png"))))

    (define (angle! new-angle)
      (set! angle new-angle))
        
    (define (initialize!)
      (define bullet-speed 0.05)
      ((game-obj 'initialize!) type
                               position
                               bullet-speed
                               velocity
                               (/ 16 tile-width-px)
                               (/ 28 tile-height-px))
      (create-tile! bullet-layer bullet-tile))
    
    (define (draw!)
      ((bullet-tile 'rotate!) angle)
      (draw-tile! bullet-tile (game-obj 'position)))

    (define (delete-bullet!)
      (delete! bullet-tile bullet-layer))

    (define (update! delta-time)
      (if (game-obj 'active?)
          (begin
            (draw!)
            ((game-obj 'update!) delta-time))
          (delete-bullet!)))

    (initialize!)
  
    (define (dispatch-bullet msg)
      (cond ((eq? msg 'update!) update!)
            ((eq? msg 'angle!) angle!)
            (else (game-obj msg))))
    dispatch-bullet))
